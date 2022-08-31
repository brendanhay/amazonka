-- |
-- Module      : Test.AWS.Sign.V$
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.AWS.Sign.V4.Base where

import qualified Data.Foldable as Fold
import Network.AWS.Core
import Network.AWS.Lens ((.~))
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Sign.V4.Base as Base
import Test.AWS.Arbitrary ()
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
      testCase "S3 canonical path encoding example" testS3CanonicalPathExample,
      testCase "non-S3 canonical path encoding example" testNonS3CanonicalPathExample,
      testCase "S3 canonical path is not normalized" testS3ShouldNotNormalize,
      testCase "non-S3 canonical path is normalized" testNonS3ShouldNormalize
    ]

testRequestPathsEscapedOnce :: Property
testRequestPathsEscapedOnce = QC.forAll (mkV4Paths (const True) mkNormalizedPath) $ \(raw, reqPath, _) ->
  untag reqPath == toBS (escapePath raw)

testS3CanonicalPathsEscapedOnce :: Property
testS3CanonicalPathsEscapedOnce = QC.forAll (mkV4Paths (== "S3") mkNormalizedPath) $ \(_, reqPath, canonicalPath) ->
  untag reqPath == untag canonicalPath

testNonS3CanonicalPathsEscapedTwice :: Property
testNonS3CanonicalPathsEscapedTwice = QC.forAll (mkV4Paths (/= "S3") mkNormalizedPath) $ \(raw, _, canonicalPath) ->
  untag canonicalPath == toBS (escapePathTwice raw)

testEmptyPath :: Assertion
testEmptyPath = do
  let empty = Raw []
  (_, reqPath, canonicalPath) <- generate $ mkV4Paths (const True) (pure empty)
  toBS reqPath @?= "/"
  toBS canonicalPath @?= "/"

testNonS3CanonicalPathExample :: Assertion
testNonS3CanonicalPathExample = do
  let example = Raw ["documents and settings"]

  (_, reqPath, canonicalPath) <- generate $ mkV4Paths (/= "S3") (pure example)
  toBS reqPath @?= "/documents%20and%20settings"
  toBS canonicalPath @?= "/documents%2520and%2520settings"

testS3CanonicalPathExample :: Assertion
testS3CanonicalPathExample = do
  let example = Raw ["documents and settings"]

  (_, reqPath, canonicalPath) <- generate $ mkV4Paths (== "S3") (pure example)
  toBS reqPath @?= "/documents%20and%20settings"
  untag canonicalPath @?= untag reqPath

testS3ShouldNotNormalize :: Assertion
testS3ShouldNotNormalize = do
  let key = Raw ["foo", "..", "bar", ".", "baz", "."]

  (_, reqPath, canonicalPath) <- generate $ mkV4Paths (== "S3") (pure key)
  toBS reqPath @?= "/foo/../bar/./baz/."
  untag canonicalPath @?= untag reqPath

testNonS3ShouldNormalize :: Assertion
testNonS3ShouldNormalize = do
  let key = Raw ["foo", "..", "bar", ".", "baz", "."]

  (_, reqPath, canonicalPath) <- generate $ mkV4Paths (/= "S3") (pure key)
  toBS reqPath @?= "/bar/baz"
  untag canonicalPath @?= untag reqPath

mkV4Paths :: (Abbrev -> Bool) -> Gen RawPath -> Gen (RawPath, Base.Path, CanonicalPath)
mkV4Paths p genPath = do
  aReq <- arbitrary :: Gen (Request ())
  aPath <- genPath
  aService <- arbitrary
  anAbbrev <- suchThat arbitrary p
  aBody <- toHashed <$> (arbitrary :: Gen ByteString)

  let svc =
        aService
          { _serviceSigner = v4,
            _serviceAbbrev = anAbbrev
          }
      req =
        aReq
          & requestPath .~ aPath
          & requestService .~ svc
          & requestBody .~ Hashed aBody

  (meta, _) <- base (Tag (sha256Base16 aBody)) req <$> arbitrary <*> arbitrary <*> arbitrary
  pure (aPath, metaPath meta, metaCanonicalPath meta)

mkNormalizedPath :: Gen RawPath
mkNormalizedPath = suchThat arbitrary noDots
  where
    noDots (Raw xs :: RawPath) = isNothing $ Fold.find (\x -> x == "." || x == "..") xs
