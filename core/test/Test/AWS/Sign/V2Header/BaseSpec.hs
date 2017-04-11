{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.Sign.V2Header.BaseSpec
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Sign.V2Header.BaseSpec (tests) where

import           Control.Applicative

import qualified Data.ByteString.Char8       as DBC
import qualified Data.CaseInsensitive        as DC
import qualified Data.Text                   as DT
import           Data.List.Ordered

import qualified Network.AWS.Data.Query      as NADQ
import qualified Network.HTTP.Types          as NHT

import qualified Test.Tasty                  as TT
import qualified Test.Tasty.QuickCheck       as TTQ

-- unqualified to more natural test case flow
import           Test.Tasty.HUnit

import           Network.AWS.Sign.V2Header.Base

--- Generators for Text / QueryValues

nonEmptyString :: TTQ.Gen String
nonEmptyString = TTQ.listOf validChars
  where validChars = TTQ.frequency[(1, TTQ.choose('A','Z')), (1, TTQ.choose ('a', 'z'))]

nonEmptyText :: TTQ.Gen DT.Text
nonEmptyText = DT.pack <$> nonEmptyString
  
nonEmptyByteString :: TTQ.Gen DBC.ByteString
nonEmptyByteString = DBC.pack <$> nonEmptyString

uninterestingQValues :: TTQ.Gen NADQ.QueryString
uninterestingQValues = NADQ.toQuery <$> nonEmptyText

--- Properties for non-empty QValues

prop_QValueEmpty :: TTQ.Property
prop_QValueEmpty = TTQ.forAll uninterestingQValues $ \qv -> constructQueryStringForSigning qv ==  (NADQ.QValue(Nothing :: Maybe(DBC.ByteString)))

--- Generators / properties for uninteresting query pairs

uninterestingQPairs  :: TTQ.Gen NADQ.QueryString
uninterestingQPairs = NADQ.toQuery <$> ((,) <$> nonEmptyText <*> uninterestingQValues)

prop_UninterestingQPairs :: TTQ.Property
prop_UninterestingQPairs = TTQ.forAll uninterestingQPairs $ \qpair -> constructQueryStringForSigning qpair == NADQ.QValue Nothing

--- Generators / properties for interesting query pairs

interestingQueryKey :: TTQ.Gen DBC.ByteString
interestingQueryKey = TTQ.elements $ map DBC.pack ["acl","cors","defaultObjectAcl","location","logging","partNumber","policy","requestPayment","torrent","versioning","versionId","versions","website","uploads","uploadId","response-content-type","response-content-language","response-expires","response-cache-control","response-content-disposition","response-content-encoding","delete","lifecycle","tagging","restore","storageClass","websiteConfig","compose"]

interestingQPairs :: TTQ.Gen NADQ.QueryString
interestingQPairs = NADQ.toQuery <$> ((,) <$> interestingQueryKey <*> uninterestingQValues)

prop_InterestingQPairs :: TTQ.Property
prop_InterestingQPairs = TTQ.forAll interestingQPairs $ \qpair -> not (constructQueryStringForSigning qpair == NADQ.QValue Nothing)

--- Generators / properties for query lists

uninterestingQLists :: TTQ.Gen NADQ.QueryString
uninterestingQLists = NADQ.toQueryList <$> nonEmptyByteString <*> (TTQ.listOf $ TTQ.frequency [(1, uninterestingQPairs), (1, uninterestingQValues), (1, uninterestingQLists)])

containsAnything :: NADQ.QueryString -> Bool
containsAnything (NADQ.QValue (Just _)) = True
containsAnything (NADQ.QPair _ _) = True
containsAnything (NADQ.QList qlist) = any containsAnything qlist
containsAnything (NADQ.QValue Nothing) = False 

prop_UninterestingQLists :: TTQ.Property
prop_UninterestingQLists = TTQ.forAll uninterestingQLists $ \qlist -> not $ containsAnything $ constructQueryStringForSigning qlist

interestingQLists :: TTQ.Gen NADQ.QueryString
interestingQLists = NADQ.toQueryList <$> interestingQueryKey <*> (TTQ.listOf $ TTQ.frequency [(1, interestingQLists), (1, uninterestingQPairs), (1, uninterestingQValues), (1, uninterestingQLists)])

-- this property should probably be expanded to check that all qpairs actually contain interesting query keys AND that uninteresting
-- query keys are actually preserved underneath an interesting one
prop_InterestingQLists :: TTQ.Property
prop_InterestingQLists = TTQ.forAll interestingQLists $ \qlist -> containsAnything $ constructQueryStringForSigning qlist

-- Generator / Property for headers
randomHeaderGenerator :: TTQ.Gen NHT.Header
randomHeaderGenerator = (,) <$> (DC.mk <$> nonEmptyByteString) <*> nonEmptyByteString

interestingAwsHeaderName :: TTQ.Gen NHT.HeaderName
interestingAwsHeaderName = DC.mk <$> DBC.pack <$> (("aws-" ++) <$> nonEmptyString)

interestingHeaderName :: TTQ.Gen NHT.HeaderName
interestingHeaderName = TTQ.elements [NHT.hContentMD5, NHT.hContentType, NHT.hDate]

interestingHeaderGenerator :: TTQ.Gen NHT.Header
interestingHeaderGenerator = (,) <$> interestingHeaderName <*> nonEmptyByteString

interestingAwsHeaderGenerator :: TTQ.Gen NHT.Header
interestingAwsHeaderGenerator = (,) <$> interestingAwsHeaderName <*> nonEmptyByteString

prop_RandomHeaders :: TTQ.Property
prop_RandomHeaders = TTQ.forAll randomHeaderGenerator $ \randomHeader -> constructHeaderStringForSigning randomHeader == snd randomHeader 

prop_InterestingHeaders :: TTQ.Property
prop_InterestingHeaders = TTQ.forAll interestingHeaderGenerator $ \interestingHeader -> constructHeaderStringForSigning interestingHeader == snd interestingHeader 

prop_InterestingAwsHeaders :: TTQ.Property
prop_InterestingAwsHeaders = TTQ.forAll interestingAwsHeaderGenerator $ \interestingHeader -> constructHeaderStringForSigning interestingHeader == (DBC.append (DC.foldedCase $ fst interestingHeader) $ DBC.append ":" (snd interestingHeader))

-- Generators / Properties for auxiliary header functions
allHeadersGenerator :: TTQ.Gen [NHT.Header]
allHeadersGenerator = TTQ.listOf $ TTQ.frequency[(1, interestingHeaderGenerator), (1, interestingAwsHeaderGenerator)]

allIncreasing :: [NHT.Header] -> Bool
allIncreasing xs = and $ zipWith (<=) mapped $ drop 1 mapped
  where mapped = map (\x -> fst x) xs

testHeaders :: [NHT.Header] -> Bool
testHeaders headers = (length sortedHeaders == length sortedHeaders) && allIncreasing sortedHeaders
  where
    sortedHeaders = sortHeaders headers

prop_SortedHeaders :: TTQ.Property
prop_SortedHeaders = TTQ.forAll allHeadersGenerator $ \allHeaders -> testHeaders allHeaders


tests :: TT.TestTree
tests = TT.testGroup "v2Header.BaseSpec"

  [ TT.testGroup "constructQueryStringForSigning"

    [ TTQ.testProperty "should always convert set QValues to Nothing" prop_QValueEmpty
    , testCase "should keep an unset QValue" $ constructQueryStringForSigning (NADQ.QValue (Nothing :: Maybe(DBC.ByteString))) @?= (NADQ.QValue (Nothing :: Maybe(DBC.ByteString)))
    , TTQ.testProperty "should discard QPairs that are not interesting to AWS" prop_UninterestingQPairs
    , TTQ.testProperty "should keep all QPairs that are interesting to AWS" prop_InterestingQPairs
    , testCase "should keep an empty QList" $ constructQueryStringForSigning (NADQ.QList []) @?= (NADQ.QList [])
    , testCase "should keep a list of Nothing QValues" $ constructQueryStringForSigning (NADQ.QList [(NADQ.QValue Nothing)]) @?= (NADQ.QList [(NADQ.QValue Nothing)])
    , TTQ.testProperty "should discard the contents of an unintersting QList" prop_UninterestingQLists
    , TTQ.testProperty "should not discard all elements in interesting QLists" prop_InterestingQLists
    ]
    
  , TT.testGroup "constructHeaderStringForSigning"
    [ TTQ.testProperty "should convert random headers to their header value" prop_RandomHeaders
    , TTQ.testProperty "should convert interesting headers to their header value" prop_InterestingHeaders
    , TTQ.testProperty "should convert aws headers to a canonical string" prop_InterestingAwsHeaders 
    ]

  , TT.testGroup "auxiliary headers functions"
    [ TTQ.testProperty "should sort and preserve headers" prop_SortedHeaders
    , testCase "should contain empty md5 and empty content type headers if not present" $ [(NHT.hContentMD5, ""), (NHT.hContentType, "")] `subset` (unionNecessaryHeaders []) @?= True
    , testCase "should preserve a set md5 and contain an empty content type header if not present" $ [(NHT.hContentMD5, "123"), (NHT.hContentType, "")] `subset` (unionNecessaryHeaders [(NHT.hContentMD5, "123")]) @?= True
    , testCase "should preserve a set content type and preserve an empty md5 header if not present" $ [(NHT.hContentType, "123"), (NHT.hContentMD5, "")] `subset` (unionNecessaryHeaders [(NHT.hContentType, "123")]) @?= True
    , testCase "should preserve md5 and content type headers if set" $ [(NHT.hContentType, "123"), (NHT.hContentMD5, "456")] `subset` unionNecessaryHeaders [(NHT.hContentType, "123"), (NHT.hContentMD5, "456")] @?= True
    ]

  , TT.testGroup "toSingerQBS"
    [ testCase "should convert an empty query string" $ toSignerQBS (NADQ.QValue Nothing) @?= ""
    , testCase "should convert an empty value of QPair to just the key" $ toSignerQBS (NADQ.QPair "key" (NADQ.QValue Nothing)) @?= "key"
    , testCase "should convert an empty value of QPair followed by QValue to just the key and just the value" $ toSignerQBS (NADQ.QList [(NADQ.QPair "key" (NADQ.QValue Nothing)), (NADQ.QValue $ Just "key2")]) @?= "key&key2"
    ]

  , TT.testGroup "constructFullPath"
    [ testCase "should convert an empty queryString to just the path" $ constructFullPath "path" "" @?= "path"
    , testCase "should convert an empty value of QPair and a path to just the path and the key" $ constructFullPath "path" (toSignerQBS (NADQ.QPair "key" (NADQ.QValue Nothing))) @?= "path?key"
    ]

  , TT.testGroup "should construct canonical headers"
    [ testCase "should construct a canonical header from a base string" $ constructHeaderSigner [] "GET" "\\" "" @?= "GET\n\n\n\\"
    ]

  ]
