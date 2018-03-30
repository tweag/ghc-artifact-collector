# GHC artifact collector

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Build status](https://ci.appveyor.com/api/projects/status/jxrewpla45r1n04g/branch/master?svg=true)](https://ci.appveyor.com/project/mboes/ghc-artifact-collector/branch/master)
[![CircleCI](https://circleci.com/gh/tweag/ghc-artifact-collector/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/ghc-artifact-collector/tree/master)

A prototype of application that will be collecting GHC build artifacts on
Circle CI and AppVeyor.

Usage:

```
$ ghc-artifact-collector file1 file2 …
```

## Behavior spec

* If environment vars of [CircleCI](https://circleci.com/docs/2.0/env-vars/)
  or [AppVeyor](https://www.appveyor.com/docs/environment-variables/) are
  not set, print a message about the fact and exit with status code 1.

* If Branch is not `master` *and* the build has been triggered not by a tag
  push, do nothing. Note that one has to add filtering by tags in order to
  enable building on tag pushing on CircleCI.

* If there is no tag, use the following prefix:

  ```
  nightly/<job-name>/<DD-MM-YYYY-SHA1>/<file-name>
  ```

* If there is a tag, use the following prefix:

  ```
  releases/<job-name>/<tag>/<file-name>
  ```

* Multiple files given as shell wildcards can be uploaded (if the command is
  run inside bash, that is).

* `nightly/` folder is expected to be configured with such life cycle policy
  that old files are automatically deleted, e.g. after a month.

## License

Copyright © 2018 Tweag I/O

Distributed under BSD 3 clause license.
