# GHC artifact collector

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Build status](https://ci.appveyor.com/api/projects/status/jxrewpla45r1n04g/branch/master?svg=true)](https://ci.appveyor.com/project/mboes/ghc-artifact-collector/branch/master)
[![CircleCI](https://circleci.com/gh/tweag/ghc-artifact-collector/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/ghc-artifact-collector/tree/master)

This is the application that collects GHC build artifacts created on Circle
CI and AppVeyor.

Usage:

```
ghc-artifact-collector - collect GHC build artifacts

Usage: ghc-artifact-collector [-t|--as-tool TOOL-NAME] FILE
  GHC artifactor collector

Available options:
  -h,--help                Show this help text
  -t,--as-tool TOOL-NAME   Upload file as a tool with this name.
  FILE                     File to store.
```

## Behavior spec

* If environment vars of [CircleCI](https://circleci.com/docs/2.0/env-vars/)
  or [AppVeyor](https://www.appveyor.com/docs/environment-variables/) are
  not set, print a message about the fact and exit with status code 1.

* If S3 variables are not set, do nothing (important because we don't want
  PRs from forks to fail because of this, and those PRs won't have S3
  variables set). S3 environment variables:

  * `GHC_ARTIFACT_COLLECTOR_ACCESS_KEY`
  * `GHC_ARTIFACT_COLLECTOR_SECRET_KEY`
  * `GHC_ARTIFACT_COLLECTOR_BUCKET_NAME`
  * `GHC_ARTIFACT_COLLECTOR_REGION`

  The variables mean what their names suggest.

* If branch is not `master` *and* the build has been triggered not by a tag
  push, do nothing. Note that one has to add filtering by tags in order to
  enable building on tag pushing on CircleCI. (This is unless `--as-tool` is
  used. With that option branch and tag do not matter, but it's used only to
  deploy the `ghc-artifact-collector` tool itself.)

* If there is no tag, use the following object keys:

  ```
  nightly/<job-name>/<DD-MM-YYYY-SHA1>/bindist.<exension-of-file>
  nightly/<job-name>/<DD-MM-YYYY-SHA1>/metadata.json
  ```

  The `metadata.json` file contains some metadata describing how the bindist
  was obtained (SHA1 of commit, etc.).

  In addition to that, the first artifact in the list of files to upload
  will be copied to this (more) stable location:

  ```
  nightly/<job-name>/latest/bindist.<extension-of-file>
  nightly/<job-name>/latest/metadata.json
  ```

* If there is a tag, use the following prefix:

  ```
  releases/<job-name>/<tag>/bindist.<extension-of-file>
  releases/<job-name>/<tag>/metadata.json
  ```

  Similarly, the latest version of artifact will be copied to this location:

  ```
  releases/<job-name>/latest/bindist.<extension-of-file>
  releases/<job-name>/latest/metadata.json
  ```

* `nightly/` folder is expected to be configured with such life cycle policy
  that old files are automatically deleted, e.g. after a month.

## License

Copyright © 2018 Tweag I/O

Distributed under BSD 3 clause license.
