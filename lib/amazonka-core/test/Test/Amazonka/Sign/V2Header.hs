-- |
-- Module      : Test.Amazonka.Sign.V2Header.BaseSpec
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Sign.V2Header (tests) where

import Amazonka.Core hiding (length, nonEmptyText)
import Amazonka.Prelude
import Amazonka.Sign.V2Header
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import Data.List.Ordered (subset)
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Test.QuickCheck as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, Property, testProperty)

tests :: TestTree
tests =
  testGroup
    "v2Header.BaseSpec"
    [ testGroup
        "constructSigningQuery"
        [ testProperty
            "should always convert set QValues to Nothing"
            prop_QValueEmpty,
          testCase "should keep an unset QValue" $
            constructSigningQuery (QValue (Nothing :: Maybe ByteString)) @?= (QValue (Nothing :: Maybe ByteString)),
          testProperty
            "should discard QPairs that are not interesting to AWS"
            prop_UninterestingQPairs,
          testProperty
            "should keep all QPairs that are interesting to AWS"
            prop_InterestingQPairs,
          testCase "should keep an empty QList" $
            constructSigningQuery (QList []) @?= QList [],
          testCase "should keep a list of Nothing QValues" $
            constructSigningQuery (QList [(QValue Nothing)]) @?= (QList [(QValue Nothing)]),
          testProperty
            "should discard the contents of an unintersting QList"
            prop_UninterestingQLists,
          testProperty
            "should not discard all QC.elements in interesting QLists"
            prop_InterestingQLists
        ],
      testGroup
        "constructSigningHeader"
        [ testProperty
            "should convert random headers to their header value"
            prop_RandomHeaders,
          testProperty
            "should convert interesting headers to their header value"
            prop_InterestingHeaders,
          testProperty
            "should convert aws headers to a canonical string"
            prop_InterestingAwsHeaders
        ],
      testGroup
        "auxiliary headers functions"
        [ testProperty
            "should sort and preserve headers"
            prop_SortedHeaders,
          testCase "should contain empty md5 and empty content type headers if not present" $
            [(HTTP.hContentMD5, ""), (HTTP.hContentType, "")] `subset` (unionNecessaryHeaders []) @?= True,
          testCase "should preserve a set md5 and contain an empty content type header if not present" $
            [(HTTP.hContentMD5, "123"), (HTTP.hContentType, "")] `subset` (unionNecessaryHeaders [(HTTP.hContentMD5, "123")]) @?= True,
          testCase "should preserve a set content type and preserve an empty md5 header if not present" $
            [(HTTP.hContentType, "123"), (HTTP.hContentMD5, "")] `subset` (unionNecessaryHeaders [(HTTP.hContentType, "123")]) @?= True,
          testCase "should preserve md5 and content type headers if set" $
            [(HTTP.hContentType, "123"), (HTTP.hContentMD5, "456")] `subset` unionNecessaryHeaders [(HTTP.hContentType, "123"), (HTTP.hContentMD5, "456")] @?= True
        ],
      testGroup
        "toSingerQBS"
        [ testCase "should convert an empty query string" $
            toSignerQueryBS (QValue Nothing) @?= "",
          testCase "should convert an empty value of QPair to just the key" $
            toSignerQueryBS (QPair "key" (QValue Nothing)) @?= "key",
          testCase "should convert an empty value of QPair followed by QValue to just the key and just the value" $
            toSignerQueryBS
              ( QList
                  [ (QPair "key" (QValue Nothing)),
                    ( QValue $
                        Just "key2"
                    )
                  ]
              )
              @?= "key&key2"
        ],
      testGroup
        "constructFullPath"
        [ testCase "should convert an empty queryString to just the path" $
            constructFullPath "path" "" @?= "path",
          testCase "should convert an empty value of QPair and a path to just the path and the key" $
            constructFullPath "path" (toSignerQueryBS (QPair "key" (QValue Nothing))) @?= "path?key"
        ],
      testGroup
        "should construct canonical headers"
        [ testCase "should construct a canonical header from a base string" $
            newSigner [] "GET" "\\" "" @?= "GET\n\n\n\\"
        ]
    ]

-- Properties for non-empty QValues

prop_QValueEmpty :: Property
prop_QValueEmpty =
  QC.forAll uninterestingQValues $ \qv ->
    constructSigningQuery qv
      == QValue (Nothing :: Maybe ByteString)

-- Generators / properties for uninteresting query pairs

prop_UninterestingQPairs :: Property
prop_UninterestingQPairs =
  QC.forAll uninterestingQPairs $ \qpair ->
    constructSigningQuery qpair == QValue Nothing

uninterestingQPairs :: Gen QueryString
uninterestingQPairs =
  toQuery <$> ((,) <$> nonEmptyText <*> uninterestingQValues)

-- Generators / properties for interesting query pairs

prop_InterestingQPairs :: Property
prop_InterestingQPairs =
  QC.forAll interestingQPairs $ \qpair ->
    constructSigningQuery qpair /= QValue Nothing

interestingQueryKey :: Gen ByteString
interestingQueryKey =
  QC.elements
    [ "acl",
      "cors",
      "defaultObjectAcl",
      "location",
      "logging",
      "partNumber",
      "policy",
      "requestPayment",
      "torrent",
      "versioning",
      "versionId",
      "versions",
      "website",
      "uploads",
      "uploadId",
      "response-content-type",
      "response-content-language",
      "response-expires",
      "response-cache-control",
      "response-content-disposition",
      "response-content-encoding",
      "delete",
      "lifecycle",
      "tagging",
      "restore",
      "storageClass",
      "websiteConfig",
      "compose"
    ]

interestingQPairs :: Gen QueryString
interestingQPairs =
  toQuery <$> ((,) <$> interestingQueryKey <*> uninterestingQValues)

-- Generators / properties for query lists

prop_UninterestingQLists :: Property
prop_UninterestingQLists =
  QC.forAll uninterestingQLists $
    not . containsAnything . constructSigningQuery

uninterestingQLists :: Gen QueryString
uninterestingQLists =
  toQueryList
    <$> nonEmptyByteString
    <*> QC.listOf
      ( QC.frequency
          [ (1, uninterestingQPairs),
            (1, uninterestingQValues),
            (1, uninterestingQLists)
          ]
      )

containsAnything :: QueryString -> Bool
containsAnything = \case
  QValue v -> isJust v
  QPair {} -> True
  QList qs -> any containsAnything qs

interestingQLists :: Gen QueryString
interestingQLists =
  toQueryList
    <$> interestingQueryKey
    <*> QC.listOf
      ( QC.frequency
          [ (1, interestingQLists),
            (1, uninterestingQPairs),
            (1, uninterestingQValues),
            (1, uninterestingQLists)
          ]
      )

-- this property should probably be expanded to check that all qpairs actually contain interesting query keys AND that uninteresting
-- query keys are actually preserved underneath an interesting one
prop_InterestingQLists :: Property
prop_InterestingQLists =
  QC.forAll interestingQLists $
    containsAnything . constructSigningQuery

prop_RandomHeaders :: Property
prop_RandomHeaders =
  QC.forAll randomHeaderGenerator $ \hs ->
    constructSigningHeader hs == snd hs

prop_InterestingHeaders :: Property
prop_InterestingHeaders =
  QC.forAll interestingHeaderGenerator $ \hs ->
    constructSigningHeader hs == snd hs

prop_InterestingAwsHeaders :: Property
prop_InterestingAwsHeaders =
  QC.forAll interestingAwsHeaderGenerator $ \hs ->
    constructSigningHeader hs
      == (CI.foldedCase (fst hs) <> ":" <> snd hs)

-- Generator / Property for headers
randomHeaderGenerator :: Gen HTTP.Header
randomHeaderGenerator =
  (,) <$> (CI.mk <$> nonEmptyByteString) <*> nonEmptyByteString

interestingAwsHeaderName :: Gen HTTP.HeaderName
interestingAwsHeaderName =
  CI.mk <$> BS8.pack <$> fmap ("aws-" <>) nonEmptyString

interestingHeaderName :: Gen HTTP.HeaderName
interestingHeaderName =
  QC.elements [HTTP.hContentMD5, HTTP.hContentType, HTTP.hDate]

interestingHeaderGenerator :: Gen HTTP.Header
interestingHeaderGenerator =
  (,) <$> interestingHeaderName <*> nonEmptyByteString

interestingAwsHeaderGenerator :: Gen HTTP.Header
interestingAwsHeaderGenerator =
  (,) <$> interestingAwsHeaderName <*> nonEmptyByteString

-- Generators / Properties for auxiliary header functions

prop_SortedHeaders :: Property
prop_SortedHeaders =
  QC.forAll allHeadersGenerator $ \allHeaders -> testHeaders allHeaders

allHeadersGenerator :: Gen [HTTP.Header]
allHeadersGenerator =
  QC.listOf $
    QC.frequency
      [ (1, interestingHeaderGenerator),
        (1, interestingAwsHeaderGenerator)
      ]

allIncreasing :: [HTTP.Header] -> Bool
allIncreasing xs =
  and $ zipWith (<=) mapped $ drop 1 mapped
  where
    mapped = map fst xs

testHeaders :: [HTTP.Header] -> Bool
testHeaders headers =
  length sortedHeaders == length sortedHeaders && allIncreasing sortedHeaders
  where
    sortedHeaders = List.sort headers

-- Generators for Text / QueryValues

nonEmptyByteString :: Gen ByteString
nonEmptyByteString = BS8.pack <$> nonEmptyString

uninterestingQValues :: Gen QueryString
uninterestingQValues = toQuery <$> nonEmptyText

nonEmptyText :: Gen Text.Text
nonEmptyText = Text.pack <$> nonEmptyString

nonEmptyString :: Gen String
nonEmptyString = QC.listOf1 QC.arbitrary
