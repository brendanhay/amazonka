-- |
-- Module      : Test.AWS.Sign.V4
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Sign.V4.Base where

import Amazonka.Core
import Amazonka.Data
import Amazonka.Prelude
import Amazonka.Sign.V4
import Amazonka.Sign.V4.Base as Base
import qualified Data.Foldable as Fold
import Test.Amazonka.Arbitrary ()
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Property ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "paths"
    [ testProperty "request paths are escaped once" testRequestPathsEscapedOnce,
      testProperty "S3 canonical paths are escaped once" testS3CanonicalPathsEscapedOnce,
      testProperty "non-S3 canonical paths are escaped twice" testNonS3CanonicalPathsEscapedTwice,
      testCase "empty path" testEmptyPath,
      testCase "base path" testBasePath,
      testCase "S3 canonical path encoding example" testS3CanonicalPathExample,
      testCase "non-S3 canonical path encoding example" testNonS3CanonicalPathExample,
      testCase "S3 canonical path is not normalized" testS3ShouldNotNormalize,
      testCase "non-S3 canonical path is normalized" testNonS3ShouldNormalize
    ]

testRequestPathsEscapedOnce :: Property
testRequestPathsEscapedOnce = QC.forAll (mkV4Paths args) $ \(raw, reqPath, _) ->
  untag reqPath == toBS (escapePath raw)
  where
    args =
      V4PathArgs
        { genAbbrev = arbitrary,
          genBasePath = mkNormalizedPath,
          genPath = mkNormalizedPath
        }

testS3CanonicalPathsEscapedOnce :: Property
testS3CanonicalPathsEscapedOnce = QC.forAll (mkV4Paths args) $ \(_, reqPath, canonicalPath') ->
  untag reqPath == untag canonicalPath'
  where
    args =
      V4PathArgs
        { genAbbrev = pure "S3",
          genBasePath = mkNormalizedPath,
          genPath = mkNormalizedPath
        }

testNonS3CanonicalPathsEscapedTwice :: Property
testNonS3CanonicalPathsEscapedTwice = QC.forAll (mkV4Paths args) $ \(raw, _, canonicalPath') ->
  untag canonicalPath' == toBS (escapePathTwice raw)
  where
    args =
      V4PathArgs
        { genAbbrev = mkNonS3Service,
          genBasePath = mkNormalizedPath,
          genPath = mkNormalizedPath
        }

testEmptyPath :: Assertion
testEmptyPath = do
  (_, reqPath, canonicalPath') <-
    generate . mkV4Paths $
      V4PathArgs
        { genAbbrev = arbitrary,
          genBasePath = pure mempty,
          genPath = pure mempty
        }
  toBS reqPath @?= "/"
  toBS canonicalPath' @?= "/"

testBasePath :: Assertion
testBasePath = do
  basePath <- generate arbitrary
  path <- generate arbitrary
  (_, reqPath, _) <-
    generate . mkV4Paths $
      V4PathArgs
        { genAbbrev = arbitrary,
          genBasePath = pure basePath,
          genPath = pure path
        }
  toBS reqPath @?= toBS (escapePath $ basePath <> path)

testNonS3CanonicalPathExample :: Assertion
testNonS3CanonicalPathExample = do
  let example = Raw ["documents and settings"]

  (_, reqPath, canonicalPath') <-
    generate . mkV4Paths $
      V4PathArgs
        { genAbbrev = mkNonS3Service,
          genBasePath = pure mempty,
          genPath = pure example
        }
  toBS reqPath @?= "/documents%20and%20settings"
  toBS canonicalPath' @?= "/documents%2520and%2520settings"

testS3CanonicalPathExample :: Assertion
testS3CanonicalPathExample = do
  let example = Raw ["documents and settings"]

  (_, reqPath, canonicalPath') <-
    generate . mkV4Paths $
      V4PathArgs
        { genAbbrev = pure "S3",
          genBasePath = pure mempty,
          genPath = pure example
        }
  toBS reqPath @?= "/documents%20and%20settings"
  untag canonicalPath' @?= untag reqPath

testS3ShouldNotNormalize :: Assertion
testS3ShouldNotNormalize = do
  let key = Raw ["foo", "..", "bar", ".", "baz", "."]

  (_, reqPath, canonicalPath') <-
    generate . mkV4Paths $
      V4PathArgs
        { genAbbrev = pure "S3",
          genBasePath = pure mempty,
          genPath = pure key
        }
  toBS reqPath @?= "/foo/../bar/./baz/."
  untag canonicalPath' @?= untag reqPath

testNonS3ShouldNormalize :: Assertion
testNonS3ShouldNormalize = do
  let key = Raw ["foo", "..", "bar", ".", "baz", "."]

  (_, reqPath, canonicalPath') <-
    generate . mkV4Paths $
      V4PathArgs
        { genAbbrev = mkNonS3Service,
          genBasePath = pure mempty,
          genPath = pure key
        }
  toBS reqPath @?= "/bar/baz"
  untag canonicalPath' @?= untag reqPath

data V4PathArgs = V4PathArgs
  { genAbbrev :: Gen Abbrev,
    genBasePath :: Gen RawPath,
    genPath :: Gen RawPath
  }

mkV4Paths :: V4PathArgs -> Gen (RawPath, Base.Path, CanonicalPath)
mkV4Paths V4PathArgs {..} = do
  aReq <- arbitrary :: Gen (Request ())
  aBasePath <- genBasePath
  aPath <- genPath
  aService@Service {endpoint} <- arbitrary
  anAbbrev <- genAbbrev
  aBody <- toHashed <$> (arbitrary :: Gen ByteString)

  let reg = NorthVirginia
      end = endpoint reg
      svc =
        aService
          { signer = v4,
            abbrev = anAbbrev,
            endpoint = const $ end {basePath = aBasePath}
          }
      req =
        aReq
          { path = aPath,
            service = svc,
            body = Hashed aBody
          }

  pure (fullRawPath reg req, escapedPath reg req, canonicalPath reg req)

mkNormalizedPath :: Gen RawPath
mkNormalizedPath = arbitrary `suchThat` noDots
  where
    noDots (Raw xs :: RawPath) = isNothing $ Fold.find (\x -> x == "." || x == "..") xs

mkNonS3Service :: Gen Abbrev
mkNonS3Service = arbitrary `suchThat` (/= "S3")
