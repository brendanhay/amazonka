{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudSearch is a fully-managed service in the AWS Cloud that makes
-- it simple and cost-effective to set up, manage, and scale a search solution
-- for your website or application. Amazon CloudSearch supports 34 languages
-- and popular search features such as highlighting, autocomplete, and
-- geospatial search.
module Network.AWS.CloudSearch.V2013_01_01.Types
    (
    -- * Service
      CloudSearch
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * AlgorithmicStemming
    , AlgorithmicStemming (..)

    -- * AnalysisSchemeLanguage
    , AnalysisSchemeLanguage (..)

    -- * IndexFieldType
    , IndexFieldType (..)

    -- * OptionState
    , OptionState (..)

    -- * PartitionInstanceType
    , PartitionInstanceType (..)

    -- * SuggesterFuzzyMatching
    , SuggesterFuzzyMatching (..)

    -- * ServiceEndpoint
    , ServiceEndpoint (..)
    , seEndpoint

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus (..)
    , apsOptions
    , apsStatus

    -- * AnalysisOptions
    , AnalysisOptions (..)
    , aoSynonyms
    , aoStopwords
    , aoStemmingDictionary
    , aoAlgorithmicStemming

    -- * AnalysisScheme
    , AnalysisScheme (..)
    , asAnalysisSchemeName
    , asAnalysisSchemeLanguage
    , asAnalysisOptions

    -- * AnalysisSchemeStatus
    , AnalysisSchemeStatus (..)
    , assOptions
    , assStatus

    -- * AvailabilityOptionsStatus
    , AvailabilityOptionsStatus (..)
    , aosOptions
    , aosStatus

    -- * DateArrayOptions
    , DateArrayOptions (..)
    , dapDefaultValue
    , dapSourceFields
    , dapFacetEnabled
    , dapSearchEnabled
    , dapReturnEnabled

    -- * DateOptions
    , DateOptions (..)
    , dvDefaultValue
    , dvSourceField
    , dvFacetEnabled
    , dvSearchEnabled
    , dvReturnEnabled
    , dvSortEnabled

    -- * DocumentSuggesterOptions
    , DocumentSuggesterOptions (..)
    , dsoSourceField
    , dsoFuzzyMatching
    , dsoSortExpression

    -- * DomainStatus
    , DomainStatus (..)
    , dsDomainId
    , dsDomainName
    , dsARN
    , dsCreated
    , dsDeleted
    , dsDocService
    , dsSearchService
    , dsRequiresIndexDocuments
    , dsProcessing
    , dsSearchInstanceType
    , dsSearchPartitionCount
    , dsSearchInstanceCount

    -- * DoubleArrayOptions
    , DoubleArrayOptions (..)
    , daoDefaultValue
    , daoSourceFields
    , daoFacetEnabled
    , daoSearchEnabled
    , daoReturnEnabled

    -- * DoubleOptions
    , DoubleOptions (..)
    , duDefaultValue
    , duSourceField
    , duFacetEnabled
    , duSearchEnabled
    , duReturnEnabled
    , duSortEnabled

    -- * Expression
    , Expression (..)
    , gExpressionName
    , gExpressionValue

    -- * ExpressionStatus
    , ExpressionStatus (..)
    , esOptions
    , esStatus

    -- * IndexField
    , IndexField (..)
    , ifIndexFieldName
    , ifIndexFieldType
    , ifIntOptions
    , ifDoubleOptions
    , ifLiteralOptions
    , ifTextOptions
    , ifDateOptions
    , ifLatLonOptions
    , ifIntArrayOptions
    , ifDoubleArrayOptions
    , ifLiteralArrayOptions
    , ifTextArrayOptions
    , ifDateArrayOptions

    -- * IndexFieldStatus
    , IndexFieldStatus (..)
    , ifsOptions
    , ifsStatus

    -- * IntArrayOptions
    , IntArrayOptions (..)
    , iaoDefaultValue
    , iaoSourceFields
    , iaoFacetEnabled
    , iaoSearchEnabled
    , iaoReturnEnabled

    -- * IntOptions
    , IntOptions (..)
    , ioDefaultValue
    , ioSourceField
    , ioFacetEnabled
    , ioSearchEnabled
    , ioReturnEnabled
    , ioSortEnabled

    -- * LatLonOptions
    , LatLonOptions (..)
    , lloDefaultValue
    , lloSourceField
    , lloFacetEnabled
    , lloSearchEnabled
    , lloReturnEnabled
    , lloSortEnabled

    -- * LiteralArrayOptions
    , LiteralArrayOptions (..)
    , laoDefaultValue
    , laoSourceFields
    , laoFacetEnabled
    , laoSearchEnabled
    , laoReturnEnabled

    -- * LiteralOptions
    , LiteralOptions (..)
    , loDefaultValue
    , loSourceField
    , loFacetEnabled
    , loSearchEnabled
    , loReturnEnabled
    , loSortEnabled

    -- * OptionStatus
    , OptionStatus (..)
    , osCreationDate
    , osUpdateDate
    , osUpdateVersion
    , osState
    , osPendingDeletion

    -- * ScalingParameters
    , ScalingParameters (..)
    , sssDesiredInstanceType
    , sssDesiredReplicationCount
    , sssDesiredPartitionCount

    -- * ScalingParametersStatus
    , ScalingParametersStatus (..)
    , spsOptions
    , spsStatus

    -- * Suggester
    , Suggester (..)
    , srSuggesterName
    , srDocumentSuggesterOptions

    -- * SuggesterStatus
    , SuggesterStatus (..)
    , ssOptions
    , ssStatus

    -- * TextArrayOptions
    , TextArrayOptions (..)
    , taoDefaultValue
    , taoSourceFields
    , taoReturnEnabled
    , taoHighlightEnabled
    , taoAnalysisScheme

    -- * TextOptions
    , TextOptions (..)
    , toDefaultValue
    , toSourceField
    , toReturnEnabled
    , toSortEnabled
    , toHighlightEnabled
    , toAnalysisScheme

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-01-01@) of the
-- @Amazon CloudSearch@ service.
data CloudSearch deriving (Typeable)

instance AWSService CloudSearch where
    type Sg CloudSearch = V4
    data Er CloudSearch
        = BaseException
            { _beCode :: Maybe Text
            , _beMessage :: Maybe Text
            }
        | CloudSearchClient HttpException
        | CloudSearchSerializer String
        | CloudSearchService String
        | DisabledOperationException
        | InternalException
        | InvalidTypeException
        | LimitExceededException
        | ResourceNotFoundException

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudsearch"
        , _svcVersion  = "2013-01-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CloudSearch)
deriving instance Generic (Er CloudSearch)

instance AWSError (Er CloudSearch) where
    awsError = const "CloudSearchError"

instance AWSServiceError (Er CloudSearch) where
    serviceError    = CloudSearchService
    clientError     = CloudSearchClient
    serializerError = CloudSearchSerializer

instance Exception (Er CloudSearch)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://cloudsearch.amazonaws.com/doc/2013-01-01/"
    }

-- | The level of algorithmic stemming to perform: none, minimal, light, or
-- full. The available levels vary depending on the language. For more
-- information, see Language Specific Text Processing Settings in the Amazon
-- CloudSearch Developer Guide.
data AlgorithmicStemming
    = AlgorithmicStemmingFull -- ^ full
    | AlgorithmicStemmingLight -- ^ light
    | AlgorithmicStemmingMinimal -- ^ minimal
    | AlgorithmicStemmingNone -- ^ none
      deriving (Eq, Show, Generic)

instance Hashable AlgorithmicStemming

instance FromText AlgorithmicStemming where
    parser = match "full" AlgorithmicStemmingFull
         <|> match "light" AlgorithmicStemmingLight
         <|> match "minimal" AlgorithmicStemmingMinimal
         <|> match "none" AlgorithmicStemmingNone

instance ToText AlgorithmicStemming where
    toText AlgorithmicStemmingFull = "full"
    toText AlgorithmicStemmingLight = "light"
    toText AlgorithmicStemmingMinimal = "minimal"
    toText AlgorithmicStemmingNone = "none"

instance ToByteString AlgorithmicStemming

instance FromXML AlgorithmicStemming where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AlgorithmicStemming"

instance ToQuery AlgorithmicStemming where
    toQuery = genericQuery def

-- | An IETF RFC 4646 language code or mul for multiple languages.
data AnalysisSchemeLanguage
    = AnalysisSchemeLanguageAr -- ^ ar
    | AnalysisSchemeLanguageBg -- ^ bg
    | AnalysisSchemeLanguageCa -- ^ ca
    | AnalysisSchemeLanguageCs -- ^ cs
    | AnalysisSchemeLanguageDa -- ^ da
    | AnalysisSchemeLanguageDe -- ^ de
    | AnalysisSchemeLanguageEl -- ^ el
    | AnalysisSchemeLanguageEn -- ^ en
    | AnalysisSchemeLanguageEs -- ^ es
    | AnalysisSchemeLanguageEu -- ^ eu
    | AnalysisSchemeLanguageFa -- ^ fa
    | AnalysisSchemeLanguageFi -- ^ fi
    | AnalysisSchemeLanguageFr -- ^ fr
    | AnalysisSchemeLanguageGa -- ^ ga
    | AnalysisSchemeLanguageGl -- ^ gl
    | AnalysisSchemeLanguageHe -- ^ he
    | AnalysisSchemeLanguageHi -- ^ hi
    | AnalysisSchemeLanguageHu -- ^ hu
    | AnalysisSchemeLanguageHy -- ^ hy
    | AnalysisSchemeLanguageId -- ^ id
    | AnalysisSchemeLanguageIt -- ^ it
    | AnalysisSchemeLanguageJa -- ^ ja
    | AnalysisSchemeLanguageKo -- ^ ko
    | AnalysisSchemeLanguageLv -- ^ lv
    | AnalysisSchemeLanguageMul -- ^ mul
    | AnalysisSchemeLanguageNl -- ^ nl
    | AnalysisSchemeLanguageNo -- ^ no
    | AnalysisSchemeLanguagePt -- ^ pt
    | AnalysisSchemeLanguageRo -- ^ ro
    | AnalysisSchemeLanguageRu -- ^ ru
    | AnalysisSchemeLanguageSv -- ^ sv
    | AnalysisSchemeLanguageTh -- ^ th
    | AnalysisSchemeLanguageTr -- ^ tr
    | AnalysisSchemeLanguageZhHans -- ^ zh-Hans
    | AnalysisSchemeLanguageZhHant -- ^ zh-Hant
      deriving (Eq, Show, Generic)

instance Hashable AnalysisSchemeLanguage

instance FromText AnalysisSchemeLanguage where
    parser = match "ar" AnalysisSchemeLanguageAr
         <|> match "bg" AnalysisSchemeLanguageBg
         <|> match "ca" AnalysisSchemeLanguageCa
         <|> match "cs" AnalysisSchemeLanguageCs
         <|> match "da" AnalysisSchemeLanguageDa
         <|> match "de" AnalysisSchemeLanguageDe
         <|> match "el" AnalysisSchemeLanguageEl
         <|> match "en" AnalysisSchemeLanguageEn
         <|> match "es" AnalysisSchemeLanguageEs
         <|> match "eu" AnalysisSchemeLanguageEu
         <|> match "fa" AnalysisSchemeLanguageFa
         <|> match "fi" AnalysisSchemeLanguageFi
         <|> match "fr" AnalysisSchemeLanguageFr
         <|> match "ga" AnalysisSchemeLanguageGa
         <|> match "gl" AnalysisSchemeLanguageGl
         <|> match "he" AnalysisSchemeLanguageHe
         <|> match "hi" AnalysisSchemeLanguageHi
         <|> match "hu" AnalysisSchemeLanguageHu
         <|> match "hy" AnalysisSchemeLanguageHy
         <|> match "id" AnalysisSchemeLanguageId
         <|> match "it" AnalysisSchemeLanguageIt
         <|> match "ja" AnalysisSchemeLanguageJa
         <|> match "ko" AnalysisSchemeLanguageKo
         <|> match "lv" AnalysisSchemeLanguageLv
         <|> match "mul" AnalysisSchemeLanguageMul
         <|> match "nl" AnalysisSchemeLanguageNl
         <|> match "no" AnalysisSchemeLanguageNo
         <|> match "pt" AnalysisSchemeLanguagePt
         <|> match "ro" AnalysisSchemeLanguageRo
         <|> match "ru" AnalysisSchemeLanguageRu
         <|> match "sv" AnalysisSchemeLanguageSv
         <|> match "th" AnalysisSchemeLanguageTh
         <|> match "tr" AnalysisSchemeLanguageTr
         <|> match "zh-Hans" AnalysisSchemeLanguageZhHans
         <|> match "zh-Hant" AnalysisSchemeLanguageZhHant

instance ToText AnalysisSchemeLanguage where
    toText AnalysisSchemeLanguageAr = "ar"
    toText AnalysisSchemeLanguageBg = "bg"
    toText AnalysisSchemeLanguageCa = "ca"
    toText AnalysisSchemeLanguageCs = "cs"
    toText AnalysisSchemeLanguageDa = "da"
    toText AnalysisSchemeLanguageDe = "de"
    toText AnalysisSchemeLanguageEl = "el"
    toText AnalysisSchemeLanguageEn = "en"
    toText AnalysisSchemeLanguageEs = "es"
    toText AnalysisSchemeLanguageEu = "eu"
    toText AnalysisSchemeLanguageFa = "fa"
    toText AnalysisSchemeLanguageFi = "fi"
    toText AnalysisSchemeLanguageFr = "fr"
    toText AnalysisSchemeLanguageGa = "ga"
    toText AnalysisSchemeLanguageGl = "gl"
    toText AnalysisSchemeLanguageHe = "he"
    toText AnalysisSchemeLanguageHi = "hi"
    toText AnalysisSchemeLanguageHu = "hu"
    toText AnalysisSchemeLanguageHy = "hy"
    toText AnalysisSchemeLanguageId = "id"
    toText AnalysisSchemeLanguageIt = "it"
    toText AnalysisSchemeLanguageJa = "ja"
    toText AnalysisSchemeLanguageKo = "ko"
    toText AnalysisSchemeLanguageLv = "lv"
    toText AnalysisSchemeLanguageMul = "mul"
    toText AnalysisSchemeLanguageNl = "nl"
    toText AnalysisSchemeLanguageNo = "no"
    toText AnalysisSchemeLanguagePt = "pt"
    toText AnalysisSchemeLanguageRo = "ro"
    toText AnalysisSchemeLanguageRu = "ru"
    toText AnalysisSchemeLanguageSv = "sv"
    toText AnalysisSchemeLanguageTh = "th"
    toText AnalysisSchemeLanguageTr = "tr"
    toText AnalysisSchemeLanguageZhHans = "zh-Hans"
    toText AnalysisSchemeLanguageZhHant = "zh-Hant"

instance ToByteString AnalysisSchemeLanguage

instance FromXML AnalysisSchemeLanguage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisSchemeLanguage"

instance ToQuery AnalysisSchemeLanguage where
    toQuery = genericQuery def

-- | The type of field. The valid options for a field depend on the field type.
-- For more information about the supported field types, see Configuring Index
-- Fields in the Amazon CloudSearch Developer Guide.
data IndexFieldType
    = IndexFieldTypeDate -- ^ date
    | IndexFieldTypeDateArray -- ^ date-array
    | IndexFieldTypeDouble -- ^ double
    | IndexFieldTypeDoubleArray -- ^ double-array
    | IndexFieldTypeInt -- ^ int
    | IndexFieldTypeIntArray -- ^ int-array
    | IndexFieldTypeLatlon -- ^ latlon
    | IndexFieldTypeLiteral -- ^ literal
    | IndexFieldTypeLiteralArray -- ^ literal-array
    | IndexFieldTypeText -- ^ text
    | IndexFieldTypeTextArray -- ^ text-array
      deriving (Eq, Show, Generic)

instance Hashable IndexFieldType

instance FromText IndexFieldType where
    parser = match "date" IndexFieldTypeDate
         <|> match "date-array" IndexFieldTypeDateArray
         <|> match "double" IndexFieldTypeDouble
         <|> match "double-array" IndexFieldTypeDoubleArray
         <|> match "int" IndexFieldTypeInt
         <|> match "int-array" IndexFieldTypeIntArray
         <|> match "latlon" IndexFieldTypeLatlon
         <|> match "literal" IndexFieldTypeLiteral
         <|> match "literal-array" IndexFieldTypeLiteralArray
         <|> match "text" IndexFieldTypeText
         <|> match "text-array" IndexFieldTypeTextArray

instance ToText IndexFieldType where
    toText IndexFieldTypeDate = "date"
    toText IndexFieldTypeDateArray = "date-array"
    toText IndexFieldTypeDouble = "double"
    toText IndexFieldTypeDoubleArray = "double-array"
    toText IndexFieldTypeInt = "int"
    toText IndexFieldTypeIntArray = "int-array"
    toText IndexFieldTypeLatlon = "latlon"
    toText IndexFieldTypeLiteral = "literal"
    toText IndexFieldTypeLiteralArray = "literal-array"
    toText IndexFieldTypeText = "text"
    toText IndexFieldTypeTextArray = "text-array"

instance ToByteString IndexFieldType

instance FromXML IndexFieldType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexFieldType"

instance ToQuery IndexFieldType where
    toQuery = genericQuery def

-- | The state of processing a change to an option. Possible values:
-- RequiresIndexDocuments: the option's latest value will not be deployed
-- until IndexDocuments has been called and indexing is complete. Processing:
-- the option's latest value is in the process of being activated. Active: the
-- option's latest value is completely deployed. FailedToValidate: the option
-- value is not compatible with the domain's data and cannot be used to index
-- the data. You must either modify the option value or update or remove the
-- incompatible documents.
data OptionState
    = OptionStateActive -- ^ Active
    | OptionStateFailedToValidate -- ^ FailedToValidate
    | OptionStateProcessing -- ^ Processing
    | OptionStateRequiresIndexDocuments -- ^ RequiresIndexDocuments
      deriving (Eq, Show, Generic)

instance Hashable OptionState

instance FromText OptionState where
    parser = match "Active" OptionStateActive
         <|> match "FailedToValidate" OptionStateFailedToValidate
         <|> match "Processing" OptionStateProcessing
         <|> match "RequiresIndexDocuments" OptionStateRequiresIndexDocuments

instance ToText OptionState where
    toText OptionStateActive = "Active"
    toText OptionStateFailedToValidate = "FailedToValidate"
    toText OptionStateProcessing = "Processing"
    toText OptionStateRequiresIndexDocuments = "RequiresIndexDocuments"

instance ToByteString OptionState

instance FromXML OptionState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionState"

instance ToQuery OptionState where
    toQuery = genericQuery def

-- | The instance type that you want to preconfigure for your domain. For
-- example, search.m1.small.
data PartitionInstanceType
    = PartitionInstanceTypeSearchM1Large -- ^ search.m1.large
    | PartitionInstanceTypeSearchM1Small -- ^ search.m1.small
    | PartitionInstanceTypeSearchM22Xlarge -- ^ search.m2.2xlarge
    | PartitionInstanceTypeSearchM2Xlarge -- ^ search.m2.xlarge
      deriving (Eq, Show, Generic)

instance Hashable PartitionInstanceType

instance FromText PartitionInstanceType where
    parser = match "search.m1.large" PartitionInstanceTypeSearchM1Large
         <|> match "search.m1.small" PartitionInstanceTypeSearchM1Small
         <|> match "search.m2.2xlarge" PartitionInstanceTypeSearchM22Xlarge
         <|> match "search.m2.xlarge" PartitionInstanceTypeSearchM2Xlarge

instance ToText PartitionInstanceType where
    toText PartitionInstanceTypeSearchM1Large = "search.m1.large"
    toText PartitionInstanceTypeSearchM1Small = "search.m1.small"
    toText PartitionInstanceTypeSearchM22Xlarge = "search.m2.2xlarge"
    toText PartitionInstanceTypeSearchM2Xlarge = "search.m2.xlarge"

instance ToByteString PartitionInstanceType

instance FromXML PartitionInstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PartitionInstanceType"

instance ToQuery PartitionInstanceType where
    toQuery = genericQuery def

-- | The level of fuzziness allowed when suggesting matches for a string: none,
-- low, or high. With none, the specified string is treated as an exact
-- prefix. With low, suggestions must differ from the specified string by no
-- more than one character. With high, suggestions can differ by up to two
-- characters. The default is none.
data SuggesterFuzzyMatching
    = SuggesterFuzzyMatchingHigh -- ^ high
    | SuggesterFuzzyMatchingLow -- ^ low
    | SuggesterFuzzyMatchingNone -- ^ none
      deriving (Eq, Show, Generic)

instance Hashable SuggesterFuzzyMatching

instance FromText SuggesterFuzzyMatching where
    parser = match "high" SuggesterFuzzyMatchingHigh
         <|> match "low" SuggesterFuzzyMatchingLow
         <|> match "none" SuggesterFuzzyMatchingNone

instance ToText SuggesterFuzzyMatching where
    toText SuggesterFuzzyMatchingHigh = "high"
    toText SuggesterFuzzyMatchingLow = "low"
    toText SuggesterFuzzyMatchingNone = "none"

instance ToByteString SuggesterFuzzyMatching

instance FromXML SuggesterFuzzyMatching where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuggesterFuzzyMatching"

instance ToQuery SuggesterFuzzyMatching where
    toQuery = genericQuery def

-- | The service endpoint for updating documents in a search domain.
newtype ServiceEndpoint = ServiceEndpoint
    { _seEndpoint :: Maybe Text
      -- ^ The endpoint to which service requests can be submitted. For
      -- example,
      -- search-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.cloudsearch.amazonaws.com
      -- or
      -- doc-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.cloudsearch.amazonaws.com.
      -- 
    } deriving (Show, Generic)

-- | The endpoint to which service requests can be submitted. For example,
-- search-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.cloudsearch.amazonaws.com
-- or
-- doc-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.cloudsearch.amazonaws.com.
-- 
seEndpoint :: Lens' ServiceEndpoint (Maybe Text)
seEndpoint f x =
    f (_seEndpoint x)
        <&> \y -> x { _seEndpoint = y }
{-# INLINE seEndpoint #-}

instance FromXML ServiceEndpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServiceEndpoint"

instance ToQuery ServiceEndpoint where
    toQuery = genericQuery def

-- | The access rules configured for the domain specified in the request.
data AccessPoliciesStatus = AccessPoliciesStatus
    { _apsOptions :: Text
      -- ^ Access rules for a domain's document or search service endpoints.
      -- For more information, see Configuring Access for a Search Domain
      -- in the Amazon CloudSearch Developer Guide. The maximum size of a
      -- policy document is 100 KB.
    , _apsStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    } deriving (Show, Generic)

-- | Access rules for a domain's document or search service endpoints. For more
-- information, see Configuring Access for a Search Domain in the Amazon
-- CloudSearch Developer Guide. The maximum size of a policy document is 100
-- KB.
apsOptions :: Lens' AccessPoliciesStatus (Text)
apsOptions f x =
    f (_apsOptions x)
        <&> \y -> x { _apsOptions = y }
{-# INLINE apsOptions #-}

-- | The status of domain configuration option.
apsStatus :: Lens' AccessPoliciesStatus (OptionStatus)
apsStatus f x =
    f (_apsStatus x)
        <&> \y -> x { _apsStatus = y }
{-# INLINE apsStatus #-}

instance FromXML AccessPoliciesStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessPoliciesStatus"

-- | Synonyms, stopwords, and stemming options for an analysis scheme.
data AnalysisOptions = AnalysisOptions
    { _aoSynonyms :: Maybe Text
      -- ^ A JSON object that defines synonym groups and aliases. A synonym
      -- group is an array of arrays, where each sub-array is a group of
      -- terms where each term in the group is considered a synonym of
      -- every other term in the group. The aliases value is an object
      -- that contains a collection of string:value pairs where the string
      -- specifies a term and the array of values specifies each of the
      -- aliases for that term. An alias is considered a synonym of the
      -- specified term, but the term is not considered a synonym of the
      -- alias. For more information about specifying synonyms, see
      -- Synonyms in the Amazon CloudSearch Developer Guide.
    , _aoStopwords :: Maybe Text
      -- ^ A JSON array of terms to ignore during indexing and searching.
      -- For example, ["a", "an", "the", "of"]. The stopwords dictionary
      -- must explicitly list each word you want to ignore. Wildcards and
      -- regular expressions are not supported.
    , _aoStemmingDictionary :: Maybe Text
      -- ^ A JSON object that contains a collection of string:value pairs
      -- that each map a term to its stem. For example, {"term1": "stem1",
      -- "term2": "stem2", "term3": "stem3"}. The stemming dictionary is
      -- applied in addition to any algorithmic stemming. This enables you
      -- to override the results of the algorithmic stemming to correct
      -- specific cases of overstemming or understemming. The maximum size
      -- of a stemming dictionary is 500 KB.
    , _aoAlgorithmicStemming :: Maybe AlgorithmicStemming
      -- ^ The level of algorithmic stemming to perform: none, minimal,
      -- light, or full. The available levels vary depending on the
      -- language. For more information, see Language Specific Text
      -- Processing Settings in the Amazon CloudSearch Developer Guide.
    } deriving (Show, Generic)

-- | A JSON object that defines synonym groups and aliases. A synonym group is
-- an array of arrays, where each sub-array is a group of terms where each
-- term in the group is considered a synonym of every other term in the group.
-- The aliases value is an object that contains a collection of string:value
-- pairs where the string specifies a term and the array of values specifies
-- each of the aliases for that term. An alias is considered a synonym of the
-- specified term, but the term is not considered a synonym of the alias. For
-- more information about specifying synonyms, see Synonyms in the Amazon
-- CloudSearch Developer Guide.
aoSynonyms :: Lens' AnalysisOptions (Maybe Text)
aoSynonyms f x =
    f (_aoSynonyms x)
        <&> \y -> x { _aoSynonyms = y }
{-# INLINE aoSynonyms #-}

-- | A JSON array of terms to ignore during indexing and searching. For example,
-- ["a", "an", "the", "of"]. The stopwords dictionary must explicitly list
-- each word you want to ignore. Wildcards and regular expressions are not
-- supported.
aoStopwords :: Lens' AnalysisOptions (Maybe Text)
aoStopwords f x =
    f (_aoStopwords x)
        <&> \y -> x { _aoStopwords = y }
{-# INLINE aoStopwords #-}

-- | A JSON object that contains a collection of string:value pairs that each
-- map a term to its stem. For example, {"term1": "stem1", "term2": "stem2",
-- "term3": "stem3"}. The stemming dictionary is applied in addition to any
-- algorithmic stemming. This enables you to override the results of the
-- algorithmic stemming to correct specific cases of overstemming or
-- understemming. The maximum size of a stemming dictionary is 500 KB.
aoStemmingDictionary :: Lens' AnalysisOptions (Maybe Text)
aoStemmingDictionary f x =
    f (_aoStemmingDictionary x)
        <&> \y -> x { _aoStemmingDictionary = y }
{-# INLINE aoStemmingDictionary #-}

-- | The level of algorithmic stemming to perform: none, minimal, light, or
-- full. The available levels vary depending on the language. For more
-- information, see Language Specific Text Processing Settings in the Amazon
-- CloudSearch Developer Guide.
aoAlgorithmicStemming :: Lens' AnalysisOptions (Maybe AlgorithmicStemming)
aoAlgorithmicStemming f x =
    f (_aoAlgorithmicStemming x)
        <&> \y -> x { _aoAlgorithmicStemming = y }
{-# INLINE aoAlgorithmicStemming #-}

instance FromXML AnalysisOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisOptions"

instance ToQuery AnalysisOptions where
    toQuery = genericQuery def

-- | Configuration information for an analysis scheme. Each analysis scheme has
-- a unique name and specifies the language of the text to be processed. The
-- following options can be configured for an analysis scheme: Synonyms,
-- Stopwords, StemmingDictionary, and AlgorithmicStemming.
data AnalysisScheme = AnalysisScheme
    { _asAnalysisSchemeName :: Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _asAnalysisSchemeLanguage :: AnalysisSchemeLanguage
      -- ^ An IETF RFC 4646 language code or mul for multiple languages.
    , _asAnalysisOptions :: Maybe AnalysisOptions
      -- ^ Synonyms, stopwords, and stemming options for an analysis scheme.
    } deriving (Show, Generic)

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
asAnalysisSchemeName :: Lens' AnalysisScheme (Text)
asAnalysisSchemeName f x =
    f (_asAnalysisSchemeName x)
        <&> \y -> x { _asAnalysisSchemeName = y }
{-# INLINE asAnalysisSchemeName #-}

-- | An IETF RFC 4646 language code or mul for multiple languages.
asAnalysisSchemeLanguage :: Lens' AnalysisScheme (AnalysisSchemeLanguage)
asAnalysisSchemeLanguage f x =
    f (_asAnalysisSchemeLanguage x)
        <&> \y -> x { _asAnalysisSchemeLanguage = y }
{-# INLINE asAnalysisSchemeLanguage #-}

-- | Synonyms, stopwords, and stemming options for an analysis scheme.
asAnalysisOptions :: Lens' AnalysisScheme (Maybe AnalysisOptions)
asAnalysisOptions f x =
    f (_asAnalysisOptions x)
        <&> \y -> x { _asAnalysisOptions = y }
{-# INLINE asAnalysisOptions #-}

instance FromXML AnalysisScheme where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisScheme"

instance ToQuery AnalysisScheme where
    toQuery = genericQuery def

-- | The status and configuration of an AnalysisScheme.
data AnalysisSchemeStatus = AnalysisSchemeStatus
    { _assOptions :: AnalysisScheme
      -- ^ Configuration information for an analysis scheme. Each analysis
      -- scheme has a unique name and specifies the language of the text
      -- to be processed. The following options can be configured for an
      -- analysis scheme: Synonyms, Stopwords, StemmingDictionary, and
      -- AlgorithmicStemming.
    , _assStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    } deriving (Show, Generic)

-- | Configuration information for an analysis scheme. Each analysis scheme has
-- a unique name and specifies the language of the text to be processed. The
-- following options can be configured for an analysis scheme: Synonyms,
-- Stopwords, StemmingDictionary, and AlgorithmicStemming.
assOptions :: Lens' AnalysisSchemeStatus (AnalysisScheme)
assOptions f x =
    f (_assOptions x)
        <&> \y -> x { _assOptions = y }
{-# INLINE assOptions #-}

-- | The status of domain configuration option.
assStatus :: Lens' AnalysisSchemeStatus (OptionStatus)
assStatus f x =
    f (_assStatus x)
        <&> \y -> x { _assStatus = y }
{-# INLINE assStatus #-}

instance FromXML AnalysisSchemeStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisSchemeStatus"

-- | The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
data AvailabilityOptionsStatus = AvailabilityOptionsStatus
    { _aosOptions :: Bool
      -- ^ The availability options configured for the domain.
    , _aosStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    } deriving (Show, Generic)

-- | The availability options configured for the domain.
aosOptions :: Lens' AvailabilityOptionsStatus (Bool)
aosOptions f x =
    f (_aosOptions x)
        <&> \y -> x { _aosOptions = y }
{-# INLINE aosOptions #-}

-- | The status of domain configuration option.
aosStatus :: Lens' AvailabilityOptionsStatus (OptionStatus)
aosStatus f x =
    f (_aosStatus x)
        <&> \y -> x { _aosStatus = y }
{-# INLINE aosStatus #-}

instance FromXML AvailabilityOptionsStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityOptionsStatus"

-- | Options for a field that contains an array of dates. Present if
-- IndexFieldType specifies the field is of type date-array. All options are
-- enabled by default.
data DateArrayOptions = DateArrayOptions
    { _dapDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _dapSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _dapFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _dapSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _dapReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
dapDefaultValue :: Lens' DateArrayOptions (Maybe Text)
dapDefaultValue f x =
    f (_dapDefaultValue x)
        <&> \y -> x { _dapDefaultValue = y }
{-# INLINE dapDefaultValue #-}

-- | A list of source fields to map to the field.
dapSourceFields :: Lens' DateArrayOptions (Maybe Text)
dapSourceFields f x =
    f (_dapSourceFields x)
        <&> \y -> x { _dapSourceFields = y }
{-# INLINE dapSourceFields #-}

-- | Whether facet information can be returned for the field.
dapFacetEnabled :: Lens' DateArrayOptions (Maybe Bool)
dapFacetEnabled f x =
    f (_dapFacetEnabled x)
        <&> \y -> x { _dapFacetEnabled = y }
{-# INLINE dapFacetEnabled #-}

-- | Whether the contents of the field are searchable.
dapSearchEnabled :: Lens' DateArrayOptions (Maybe Bool)
dapSearchEnabled f x =
    f (_dapSearchEnabled x)
        <&> \y -> x { _dapSearchEnabled = y }
{-# INLINE dapSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
dapReturnEnabled :: Lens' DateArrayOptions (Maybe Bool)
dapReturnEnabled f x =
    f (_dapReturnEnabled x)
        <&> \y -> x { _dapReturnEnabled = y }
{-# INLINE dapReturnEnabled #-}

instance FromXML DateArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DateArrayOptions"

instance ToQuery DateArrayOptions where
    toQuery = genericQuery def

-- | Options for a date field. Dates and times are specified in UTC (Coordinated
-- Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if
-- IndexFieldType specifies the field is of type date. All options are enabled
-- by default.
data DateOptions = DateOptions
    { _dvDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _dvSourceField :: Maybe Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _dvFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _dvSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _dvReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _dvSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
dvDefaultValue :: Lens' DateOptions (Maybe Text)
dvDefaultValue f x =
    f (_dvDefaultValue x)
        <&> \y -> x { _dvDefaultValue = y }
{-# INLINE dvDefaultValue #-}

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
dvSourceField :: Lens' DateOptions (Maybe Text)
dvSourceField f x =
    f (_dvSourceField x)
        <&> \y -> x { _dvSourceField = y }
{-# INLINE dvSourceField #-}

-- | Whether facet information can be returned for the field.
dvFacetEnabled :: Lens' DateOptions (Maybe Bool)
dvFacetEnabled f x =
    f (_dvFacetEnabled x)
        <&> \y -> x { _dvFacetEnabled = y }
{-# INLINE dvFacetEnabled #-}

-- | Whether the contents of the field are searchable.
dvSearchEnabled :: Lens' DateOptions (Maybe Bool)
dvSearchEnabled f x =
    f (_dvSearchEnabled x)
        <&> \y -> x { _dvSearchEnabled = y }
{-# INLINE dvSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
dvReturnEnabled :: Lens' DateOptions (Maybe Bool)
dvReturnEnabled f x =
    f (_dvReturnEnabled x)
        <&> \y -> x { _dvReturnEnabled = y }
{-# INLINE dvReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
dvSortEnabled :: Lens' DateOptions (Maybe Bool)
dvSortEnabled f x =
    f (_dvSortEnabled x)
        <&> \y -> x { _dvSortEnabled = y }
{-# INLINE dvSortEnabled #-}

instance FromXML DateOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DateOptions"

instance ToQuery DateOptions where
    toQuery = genericQuery def

-- | Options for a search suggester.
data DocumentSuggesterOptions = DocumentSuggesterOptions
    { _dsoSourceField :: Text
      -- ^ The name of the index field you want to use for suggestions.
    , _dsoFuzzyMatching :: Maybe SuggesterFuzzyMatching
      -- ^ The level of fuzziness allowed when suggesting matches for a
      -- string: none, low, or high. With none, the specified string is
      -- treated as an exact prefix. With low, suggestions must differ
      -- from the specified string by no more than one character. With
      -- high, suggestions can differ by up to two characters. The default
      -- is none.
    , _dsoSortExpression :: Maybe Text
      -- ^ An expression that computes a score for each suggestion to
      -- control how they are sorted.
    } deriving (Show, Generic)

-- | The name of the index field you want to use for suggestions.
dsoSourceField :: Lens' DocumentSuggesterOptions (Text)
dsoSourceField f x =
    f (_dsoSourceField x)
        <&> \y -> x { _dsoSourceField = y }
{-# INLINE dsoSourceField #-}

-- | The level of fuzziness allowed when suggesting matches for a string: none,
-- low, or high. With none, the specified string is treated as an exact
-- prefix. With low, suggestions must differ from the specified string by no
-- more than one character. With high, suggestions can differ by up to two
-- characters. The default is none.
dsoFuzzyMatching :: Lens' DocumentSuggesterOptions (Maybe SuggesterFuzzyMatching)
dsoFuzzyMatching f x =
    f (_dsoFuzzyMatching x)
        <&> \y -> x { _dsoFuzzyMatching = y }
{-# INLINE dsoFuzzyMatching #-}

-- | An expression that computes a score for each suggestion to control how they
-- are sorted.
dsoSortExpression :: Lens' DocumentSuggesterOptions (Maybe Text)
dsoSortExpression f x =
    f (_dsoSortExpression x)
        <&> \y -> x { _dsoSortExpression = y }
{-# INLINE dsoSortExpression #-}

instance FromXML DocumentSuggesterOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DocumentSuggesterOptions"

instance ToQuery DocumentSuggesterOptions where
    toQuery = genericQuery def

-- | The current status of the search domain.
data DomainStatus = DomainStatus
    { _dsDomainId :: Text
      -- ^ An internally generated unique identifier for a domain.
    , _dsDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _dsARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the search domain. See
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management for more information.
    , _dsCreated :: Maybe Bool
      -- ^ True if the search domain is created. It can take several minutes
      -- to initialize a domain when CreateDomain is called. Newly created
      -- search domains are returned from DescribeDomains with a false
      -- value for Created until domain creation is complete.
    , _dsDeleted :: Maybe Bool
      -- ^ True if the search domain has been deleted. The system must clean
      -- up resources dedicated to the search domain when DeleteDomain is
      -- called. Newly deleted search domains are returned from
      -- DescribeDomains with a true value for IsDeleted for several
      -- minutes until resource cleanup is complete.
    , _dsDocService :: Maybe ServiceEndpoint
      -- ^ The service endpoint for updating documents in a search domain.
    , _dsSearchService :: Maybe ServiceEndpoint
      -- ^ The service endpoint for requesting search results from a search
      -- domain.
    , _dsRequiresIndexDocuments :: Bool
      -- ^ True if IndexDocuments needs to be called to activate the current
      -- domain configuration.
    , _dsProcessing :: Maybe Bool
      -- ^ True if processing is being done to activate the current domain
      -- configuration.
    , _dsSearchInstanceType :: Maybe Text
      -- ^ The instance type that is being used to process search requests.
    , _dsSearchPartitionCount :: Maybe Integer
      -- ^ The number of partitions across which the search index is spread.
    , _dsSearchInstanceCount :: Maybe Integer
      -- ^ The number of search instances that are available to process
      -- search requests.
    } deriving (Show, Generic)

-- | An internally generated unique identifier for a domain.
dsDomainId :: Lens' DomainStatus (Text)
dsDomainId f x =
    f (_dsDomainId x)
        <&> \y -> x { _dsDomainId = y }
{-# INLINE dsDomainId #-}

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dsDomainName :: Lens' DomainStatus (Text)
dsDomainName f x =
    f (_dsDomainName x)
        <&> \y -> x { _dsDomainName = y }
{-# INLINE dsDomainName #-}

-- | The Amazon Resource Name (ARN) of the search domain. See Identifiers for
-- IAM Entities in Using AWS Identity and Access Management for more
-- information.
dsARN :: Lens' DomainStatus (Maybe Text)
dsARN f x =
    f (_dsARN x)
        <&> \y -> x { _dsARN = y }
{-# INLINE dsARN #-}

-- | True if the search domain is created. It can take several minutes to
-- initialize a domain when CreateDomain is called. Newly created search
-- domains are returned from DescribeDomains with a false value for Created
-- until domain creation is complete.
dsCreated :: Lens' DomainStatus (Maybe Bool)
dsCreated f x =
    f (_dsCreated x)
        <&> \y -> x { _dsCreated = y }
{-# INLINE dsCreated #-}

-- | True if the search domain has been deleted. The system must clean up
-- resources dedicated to the search domain when DeleteDomain is called. Newly
-- deleted search domains are returned from DescribeDomains with a true value
-- for IsDeleted for several minutes until resource cleanup is complete.
dsDeleted :: Lens' DomainStatus (Maybe Bool)
dsDeleted f x =
    f (_dsDeleted x)
        <&> \y -> x { _dsDeleted = y }
{-# INLINE dsDeleted #-}

-- | The service endpoint for updating documents in a search domain.
dsDocService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsDocService f x =
    f (_dsDocService x)
        <&> \y -> x { _dsDocService = y }
{-# INLINE dsDocService #-}

-- | The service endpoint for requesting search results from a search domain.
dsSearchService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsSearchService f x =
    f (_dsSearchService x)
        <&> \y -> x { _dsSearchService = y }
{-# INLINE dsSearchService #-}

-- | True if IndexDocuments needs to be called to activate the current domain
-- configuration.
dsRequiresIndexDocuments :: Lens' DomainStatus (Bool)
dsRequiresIndexDocuments f x =
    f (_dsRequiresIndexDocuments x)
        <&> \y -> x { _dsRequiresIndexDocuments = y }
{-# INLINE dsRequiresIndexDocuments #-}

-- | True if processing is being done to activate the current domain
-- configuration.
dsProcessing :: Lens' DomainStatus (Maybe Bool)
dsProcessing f x =
    f (_dsProcessing x)
        <&> \y -> x { _dsProcessing = y }
{-# INLINE dsProcessing #-}

-- | The instance type that is being used to process search requests.
dsSearchInstanceType :: Lens' DomainStatus (Maybe Text)
dsSearchInstanceType f x =
    f (_dsSearchInstanceType x)
        <&> \y -> x { _dsSearchInstanceType = y }
{-# INLINE dsSearchInstanceType #-}

-- | The number of partitions across which the search index is spread.
dsSearchPartitionCount :: Lens' DomainStatus (Maybe Integer)
dsSearchPartitionCount f x =
    f (_dsSearchPartitionCount x)
        <&> \y -> x { _dsSearchPartitionCount = y }
{-# INLINE dsSearchPartitionCount #-}

-- | The number of search instances that are available to process search
-- requests.
dsSearchInstanceCount :: Lens' DomainStatus (Maybe Integer)
dsSearchInstanceCount f x =
    f (_dsSearchInstanceCount x)
        <&> \y -> x { _dsSearchInstanceCount = y }
{-# INLINE dsSearchInstanceCount #-}

instance FromXML DomainStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DomainStatus"

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if IndexFieldType specifies the field is of
-- type double-array. All options are enabled by default.
data DoubleArrayOptions = DoubleArrayOptions
    { _daoDefaultValue :: Maybe Double
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _daoSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _daoFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _daoSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _daoReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
daoDefaultValue :: Lens' DoubleArrayOptions (Maybe Double)
daoDefaultValue f x =
    f (_daoDefaultValue x)
        <&> \y -> x { _daoDefaultValue = y }
{-# INLINE daoDefaultValue #-}

-- | A list of source fields to map to the field.
daoSourceFields :: Lens' DoubleArrayOptions (Maybe Text)
daoSourceFields f x =
    f (_daoSourceFields x)
        <&> \y -> x { _daoSourceFields = y }
{-# INLINE daoSourceFields #-}

-- | Whether facet information can be returned for the field.
daoFacetEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoFacetEnabled f x =
    f (_daoFacetEnabled x)
        <&> \y -> x { _daoFacetEnabled = y }
{-# INLINE daoFacetEnabled #-}

-- | Whether the contents of the field are searchable.
daoSearchEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoSearchEnabled f x =
    f (_daoSearchEnabled x)
        <&> \y -> x { _daoSearchEnabled = y }
{-# INLINE daoSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
daoReturnEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoReturnEnabled f x =
    f (_daoReturnEnabled x)
        <&> \y -> x { _daoReturnEnabled = y }
{-# INLINE daoReturnEnabled #-}

instance FromXML DoubleArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DoubleArrayOptions"

instance ToQuery DoubleArrayOptions where
    toQuery = genericQuery def

-- | Options for a double-precision 64-bit floating point field. Present if
-- IndexFieldType specifies the field is of type double. All options are
-- enabled by default.
data DoubleOptions = DoubleOptions
    { _duDefaultValue :: Maybe Double
      -- ^ A value to use for the field if the field isn't specified for a
      -- document. This can be important if you are using the field in an
      -- expression and that field is not present in every document.
    , _duSourceField :: Maybe Text
      -- ^ The name of the source field to map to the field.
    , _duFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _duSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _duReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _duSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
-- This can be important if you are using the field in an expression and that
-- field is not present in every document.
duDefaultValue :: Lens' DoubleOptions (Maybe Double)
duDefaultValue f x =
    f (_duDefaultValue x)
        <&> \y -> x { _duDefaultValue = y }
{-# INLINE duDefaultValue #-}

-- | The name of the source field to map to the field.
duSourceField :: Lens' DoubleOptions (Maybe Text)
duSourceField f x =
    f (_duSourceField x)
        <&> \y -> x { _duSourceField = y }
{-# INLINE duSourceField #-}

-- | Whether facet information can be returned for the field.
duFacetEnabled :: Lens' DoubleOptions (Maybe Bool)
duFacetEnabled f x =
    f (_duFacetEnabled x)
        <&> \y -> x { _duFacetEnabled = y }
{-# INLINE duFacetEnabled #-}

-- | Whether the contents of the field are searchable.
duSearchEnabled :: Lens' DoubleOptions (Maybe Bool)
duSearchEnabled f x =
    f (_duSearchEnabled x)
        <&> \y -> x { _duSearchEnabled = y }
{-# INLINE duSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
duReturnEnabled :: Lens' DoubleOptions (Maybe Bool)
duReturnEnabled f x =
    f (_duReturnEnabled x)
        <&> \y -> x { _duReturnEnabled = y }
{-# INLINE duReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
duSortEnabled :: Lens' DoubleOptions (Maybe Bool)
duSortEnabled f x =
    f (_duSortEnabled x)
        <&> \y -> x { _duSortEnabled = y }
{-# INLINE duSortEnabled #-}

instance FromXML DoubleOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DoubleOptions"

instance ToQuery DoubleOptions where
    toQuery = genericQuery def

-- | A named expression that can be evaluated at search time. Can be used for
-- sorting and filtering search results and constructing other expressions.
data Expression = Expression
    { _gExpressionName :: Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _gExpressionValue :: Text
      -- ^ The expression to evaluate for sorting while processing a search
      -- request. The Expression syntax is based on JavaScript
      -- expressions. For more information, see Configuring Expressions in
      -- the Amazon CloudSearch Developer Guide.
    } deriving (Show, Generic)

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
gExpressionName :: Lens' Expression (Text)
gExpressionName f x =
    f (_gExpressionName x)
        <&> \y -> x { _gExpressionName = y }
{-# INLINE gExpressionName #-}

-- | The expression to evaluate for sorting while processing a search request.
-- The Expression syntax is based on JavaScript expressions. For more
-- information, see Configuring Expressions in the Amazon CloudSearch
-- Developer Guide.
gExpressionValue :: Lens' Expression (Text)
gExpressionValue f x =
    f (_gExpressionValue x)
        <&> \y -> x { _gExpressionValue = y }
{-# INLINE gExpressionValue #-}

instance FromXML Expression where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Expression"

instance ToQuery Expression where
    toQuery = genericQuery def

-- | The value of an Expression and its current status.
data ExpressionStatus = ExpressionStatus
    { _esOptions :: Expression
      -- ^ The expression that is evaluated for sorting or filtering while
      -- processing a search request.
    , _esStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    } deriving (Show, Generic)

-- | The expression that is evaluated for sorting or filtering while processing
-- a search request.
esOptions :: Lens' ExpressionStatus (Expression)
esOptions f x =
    f (_esOptions x)
        <&> \y -> x { _esOptions = y }
{-# INLINE esOptions #-}

-- | The status of domain configuration option.
esStatus :: Lens' ExpressionStatus (OptionStatus)
esStatus f x =
    f (_esStatus x)
        <&> \y -> x { _esStatus = y }
{-# INLINE esStatus #-}

instance FromXML ExpressionStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExpressionStatus"

-- | The index field and field options you want to configure.
data IndexField = IndexField
    { _ifIndexFieldName :: Text
      -- ^ The name of a field in the search index. Field names must begin
      -- with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). Uppercase letters and
      -- hyphens are not allowed. The name "score" is reserved and cannot
      -- be specified as field or expression name.
    , _ifIndexFieldType :: IndexFieldType
      -- ^ The type of field. The valid options for a field depend on the
      -- field type. For more information about the supported field types,
      -- see Configuring Index Fields in the Amazon CloudSearch Developer
      -- Guide.
    , _ifIntOptions :: Maybe IntOptions
      -- ^ Options for a 64-bit signed integer field. Present if
      -- IndexFieldType specifies the field is of type int. All options
      -- are enabled by default.
    , _ifDoubleOptions :: Maybe DoubleOptions
      -- ^ Options for a double-precision 64-bit floating point field.
      -- Present if IndexFieldType specifies the field is of type double.
      -- All options are enabled by default.
    , _ifLiteralOptions :: Maybe LiteralOptions
      -- ^ Options for literal field. Present if IndexFieldType specifies
      -- the field is of type literal. All options are enabled by default.
    , _ifTextOptions :: Maybe TextOptions
      -- ^ Options for text field. Present if IndexFieldType specifies the
      -- field is of type text. A text field is always searchable. All
      -- options are enabled by default.
    , _ifDateOptions :: Maybe DateOptions
      -- ^ Options for a date field. Dates and times are specified in UTC
      -- (Coordinated Universal Time) according to IETF RFC3339:
      -- yyyy-mm-ddT00:00:00Z. Present if IndexFieldType specifies the
      -- field is of type date. All options are enabled by default.
    , _ifLatLonOptions :: Maybe LatLonOptions
      -- ^ Options for a latlon field. A latlon field contains a location
      -- stored as a latitude and longitude value pair. Present if
      -- IndexFieldType specifies the field is of type latlon. All options
      -- are enabled by default.
    , _ifIntArrayOptions :: Maybe IntArrayOptions
      -- ^ Options for a field that contains an array of 64-bit signed
      -- integers. Present if IndexFieldType specifies the field is of
      -- type int-array. All options are enabled by default.
    , _ifDoubleArrayOptions :: Maybe DoubleArrayOptions
      -- ^ Options for a field that contains an array of double-precision
      -- 64-bit floating point values. Present if IndexFieldType specifies
      -- the field is of type double-array. All options are enabled by
      -- default.
    , _ifLiteralArrayOptions :: Maybe LiteralArrayOptions
      -- ^ Options for a field that contains an array of literal strings.
      -- Present if IndexFieldType specifies the field is of type
      -- literal-array. All options are enabled by default.
    , _ifTextArrayOptions :: Maybe TextArrayOptions
      -- ^ Options for a field that contains an array of text strings.
      -- Present if IndexFieldType specifies the field is of type
      -- text-array. A text-array field is always searchable. All options
      -- are enabled by default.
    , _ifDateArrayOptions :: Maybe DateArrayOptions
      -- ^ Options for a field that contains an array of dates. Present if
      -- IndexFieldType specifies the field is of type date-array. All
      -- options are enabled by default.
    } deriving (Show, Generic)

-- | The name of a field in the search index. Field names must begin with a
-- letter and can contain the following characters: a-z (lowercase), 0-9, and
-- _ (underscore). Uppercase letters and hyphens are not allowed. The name
-- "score" is reserved and cannot be specified as field or expression name.
ifIndexFieldName :: Lens' IndexField (Text)
ifIndexFieldName f x =
    f (_ifIndexFieldName x)
        <&> \y -> x { _ifIndexFieldName = y }
{-# INLINE ifIndexFieldName #-}

-- | The type of field. The valid options for a field depend on the field type.
-- For more information about the supported field types, see Configuring Index
-- Fields in the Amazon CloudSearch Developer Guide.
ifIndexFieldType :: Lens' IndexField (IndexFieldType)
ifIndexFieldType f x =
    f (_ifIndexFieldType x)
        <&> \y -> x { _ifIndexFieldType = y }
{-# INLINE ifIndexFieldType #-}

-- | Options for a 64-bit signed integer field. Present if IndexFieldType
-- specifies the field is of type int. All options are enabled by default.
ifIntOptions :: Lens' IndexField (Maybe IntOptions)
ifIntOptions f x =
    f (_ifIntOptions x)
        <&> \y -> x { _ifIntOptions = y }
{-# INLINE ifIntOptions #-}

-- | Options for a double-precision 64-bit floating point field. Present if
-- IndexFieldType specifies the field is of type double. All options are
-- enabled by default.
ifDoubleOptions :: Lens' IndexField (Maybe DoubleOptions)
ifDoubleOptions f x =
    f (_ifDoubleOptions x)
        <&> \y -> x { _ifDoubleOptions = y }
{-# INLINE ifDoubleOptions #-}

-- | Options for literal field. Present if IndexFieldType specifies the field is
-- of type literal. All options are enabled by default.
ifLiteralOptions :: Lens' IndexField (Maybe LiteralOptions)
ifLiteralOptions f x =
    f (_ifLiteralOptions x)
        <&> \y -> x { _ifLiteralOptions = y }
{-# INLINE ifLiteralOptions #-}

-- | Options for text field. Present if IndexFieldType specifies the field is of
-- type text. A text field is always searchable. All options are enabled by
-- default.
ifTextOptions :: Lens' IndexField (Maybe TextOptions)
ifTextOptions f x =
    f (_ifTextOptions x)
        <&> \y -> x { _ifTextOptions = y }
{-# INLINE ifTextOptions #-}

-- | Options for a date field. Dates and times are specified in UTC (Coordinated
-- Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if
-- IndexFieldType specifies the field is of type date. All options are enabled
-- by default.
ifDateOptions :: Lens' IndexField (Maybe DateOptions)
ifDateOptions f x =
    f (_ifDateOptions x)
        <&> \y -> x { _ifDateOptions = y }
{-# INLINE ifDateOptions #-}

-- | Options for a latlon field. A latlon field contains a location stored as a
-- latitude and longitude value pair. Present if IndexFieldType specifies the
-- field is of type latlon. All options are enabled by default.
ifLatLonOptions :: Lens' IndexField (Maybe LatLonOptions)
ifLatLonOptions f x =
    f (_ifLatLonOptions x)
        <&> \y -> x { _ifLatLonOptions = y }
{-# INLINE ifLatLonOptions #-}

-- | Options for a field that contains an array of 64-bit signed integers.
-- Present if IndexFieldType specifies the field is of type int-array. All
-- options are enabled by default.
ifIntArrayOptions :: Lens' IndexField (Maybe IntArrayOptions)
ifIntArrayOptions f x =
    f (_ifIntArrayOptions x)
        <&> \y -> x { _ifIntArrayOptions = y }
{-# INLINE ifIntArrayOptions #-}

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if IndexFieldType specifies the field is of
-- type double-array. All options are enabled by default.
ifDoubleArrayOptions :: Lens' IndexField (Maybe DoubleArrayOptions)
ifDoubleArrayOptions f x =
    f (_ifDoubleArrayOptions x)
        <&> \y -> x { _ifDoubleArrayOptions = y }
{-# INLINE ifDoubleArrayOptions #-}

-- | Options for a field that contains an array of literal strings. Present if
-- IndexFieldType specifies the field is of type literal-array. All options
-- are enabled by default.
ifLiteralArrayOptions :: Lens' IndexField (Maybe LiteralArrayOptions)
ifLiteralArrayOptions f x =
    f (_ifLiteralArrayOptions x)
        <&> \y -> x { _ifLiteralArrayOptions = y }
{-# INLINE ifLiteralArrayOptions #-}

-- | Options for a field that contains an array of text strings. Present if
-- IndexFieldType specifies the field is of type text-array. A text-array
-- field is always searchable. All options are enabled by default.
ifTextArrayOptions :: Lens' IndexField (Maybe TextArrayOptions)
ifTextArrayOptions f x =
    f (_ifTextArrayOptions x)
        <&> \y -> x { _ifTextArrayOptions = y }
{-# INLINE ifTextArrayOptions #-}

-- | Options for a field that contains an array of dates. Present if
-- IndexFieldType specifies the field is of type date-array. All options are
-- enabled by default.
ifDateArrayOptions :: Lens' IndexField (Maybe DateArrayOptions)
ifDateArrayOptions f x =
    f (_ifDateArrayOptions x)
        <&> \y -> x { _ifDateArrayOptions = y }
{-# INLINE ifDateArrayOptions #-}

instance FromXML IndexField where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexField"

instance ToQuery IndexField where
    toQuery = genericQuery def

-- | The value of an IndexField and its current status.
data IndexFieldStatus = IndexFieldStatus
    { _ifsOptions :: IndexField
      -- ^ Configuration information for a field in the index, including its
      -- name, type, and options. The supported options depend on the
      -- IndexFieldType.
    , _ifsStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    } deriving (Show, Generic)

-- | Configuration information for a field in the index, including its name,
-- type, and options. The supported options depend on the IndexFieldType.
ifsOptions :: Lens' IndexFieldStatus (IndexField)
ifsOptions f x =
    f (_ifsOptions x)
        <&> \y -> x { _ifsOptions = y }
{-# INLINE ifsOptions #-}

-- | The status of domain configuration option.
ifsStatus :: Lens' IndexFieldStatus (OptionStatus)
ifsStatus f x =
    f (_ifsStatus x)
        <&> \y -> x { _ifsStatus = y }
{-# INLINE ifsStatus #-}

instance FromXML IndexFieldStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexFieldStatus"

-- | Options for a field that contains an array of 64-bit signed integers.
-- Present if IndexFieldType specifies the field is of type int-array. All
-- options are enabled by default.
data IntArrayOptions = IntArrayOptions
    { _iaoDefaultValue :: Maybe Integer
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _iaoSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _iaoFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _iaoSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _iaoReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
iaoDefaultValue :: Lens' IntArrayOptions (Maybe Integer)
iaoDefaultValue f x =
    f (_iaoDefaultValue x)
        <&> \y -> x { _iaoDefaultValue = y }
{-# INLINE iaoDefaultValue #-}

-- | A list of source fields to map to the field.
iaoSourceFields :: Lens' IntArrayOptions (Maybe Text)
iaoSourceFields f x =
    f (_iaoSourceFields x)
        <&> \y -> x { _iaoSourceFields = y }
{-# INLINE iaoSourceFields #-}

-- | Whether facet information can be returned for the field.
iaoFacetEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoFacetEnabled f x =
    f (_iaoFacetEnabled x)
        <&> \y -> x { _iaoFacetEnabled = y }
{-# INLINE iaoFacetEnabled #-}

-- | Whether the contents of the field are searchable.
iaoSearchEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoSearchEnabled f x =
    f (_iaoSearchEnabled x)
        <&> \y -> x { _iaoSearchEnabled = y }
{-# INLINE iaoSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
iaoReturnEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoReturnEnabled f x =
    f (_iaoReturnEnabled x)
        <&> \y -> x { _iaoReturnEnabled = y }
{-# INLINE iaoReturnEnabled #-}

instance FromXML IntArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IntArrayOptions"

instance ToQuery IntArrayOptions where
    toQuery = genericQuery def

-- | Options for a 64-bit signed integer field. Present if IndexFieldType
-- specifies the field is of type int. All options are enabled by default.
data IntOptions = IntOptions
    { _ioDefaultValue :: Maybe Integer
      -- ^ A value to use for the field if the field isn't specified for a
      -- document. This can be important if you are using the field in an
      -- expression and that field is not present in every document.
    , _ioSourceField :: Maybe Text
      -- ^ The name of the source field to map to the field.
    , _ioFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _ioSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _ioReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _ioSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
-- This can be important if you are using the field in an expression and that
-- field is not present in every document.
ioDefaultValue :: Lens' IntOptions (Maybe Integer)
ioDefaultValue f x =
    f (_ioDefaultValue x)
        <&> \y -> x { _ioDefaultValue = y }
{-# INLINE ioDefaultValue #-}

-- | The name of the source field to map to the field.
ioSourceField :: Lens' IntOptions (Maybe Text)
ioSourceField f x =
    f (_ioSourceField x)
        <&> \y -> x { _ioSourceField = y }
{-# INLINE ioSourceField #-}

-- | Whether facet information can be returned for the field.
ioFacetEnabled :: Lens' IntOptions (Maybe Bool)
ioFacetEnabled f x =
    f (_ioFacetEnabled x)
        <&> \y -> x { _ioFacetEnabled = y }
{-# INLINE ioFacetEnabled #-}

-- | Whether the contents of the field are searchable.
ioSearchEnabled :: Lens' IntOptions (Maybe Bool)
ioSearchEnabled f x =
    f (_ioSearchEnabled x)
        <&> \y -> x { _ioSearchEnabled = y }
{-# INLINE ioSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
ioReturnEnabled :: Lens' IntOptions (Maybe Bool)
ioReturnEnabled f x =
    f (_ioReturnEnabled x)
        <&> \y -> x { _ioReturnEnabled = y }
{-# INLINE ioReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
ioSortEnabled :: Lens' IntOptions (Maybe Bool)
ioSortEnabled f x =
    f (_ioSortEnabled x)
        <&> \y -> x { _ioSortEnabled = y }
{-# INLINE ioSortEnabled #-}

instance FromXML IntOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IntOptions"

instance ToQuery IntOptions where
    toQuery = genericQuery def

-- | Options for a latlon field. A latlon field contains a location stored as a
-- latitude and longitude value pair. Present if IndexFieldType specifies the
-- field is of type latlon. All options are enabled by default.
data LatLonOptions = LatLonOptions
    { _lloDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _lloSourceField :: Maybe Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _lloFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _lloSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _lloReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _lloSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
lloDefaultValue :: Lens' LatLonOptions (Maybe Text)
lloDefaultValue f x =
    f (_lloDefaultValue x)
        <&> \y -> x { _lloDefaultValue = y }
{-# INLINE lloDefaultValue #-}

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
lloSourceField :: Lens' LatLonOptions (Maybe Text)
lloSourceField f x =
    f (_lloSourceField x)
        <&> \y -> x { _lloSourceField = y }
{-# INLINE lloSourceField #-}

-- | Whether facet information can be returned for the field.
lloFacetEnabled :: Lens' LatLonOptions (Maybe Bool)
lloFacetEnabled f x =
    f (_lloFacetEnabled x)
        <&> \y -> x { _lloFacetEnabled = y }
{-# INLINE lloFacetEnabled #-}

-- | Whether the contents of the field are searchable.
lloSearchEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSearchEnabled f x =
    f (_lloSearchEnabled x)
        <&> \y -> x { _lloSearchEnabled = y }
{-# INLINE lloSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
lloReturnEnabled :: Lens' LatLonOptions (Maybe Bool)
lloReturnEnabled f x =
    f (_lloReturnEnabled x)
        <&> \y -> x { _lloReturnEnabled = y }
{-# INLINE lloReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
lloSortEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSortEnabled f x =
    f (_lloSortEnabled x)
        <&> \y -> x { _lloSortEnabled = y }
{-# INLINE lloSortEnabled #-}

instance FromXML LatLonOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LatLonOptions"

instance ToQuery LatLonOptions where
    toQuery = genericQuery def

-- | Options for a field that contains an array of literal strings. Present if
-- IndexFieldType specifies the field is of type literal-array. All options
-- are enabled by default.
data LiteralArrayOptions = LiteralArrayOptions
    { _laoDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _laoSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _laoFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _laoSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _laoReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
laoDefaultValue :: Lens' LiteralArrayOptions (Maybe Text)
laoDefaultValue f x =
    f (_laoDefaultValue x)
        <&> \y -> x { _laoDefaultValue = y }
{-# INLINE laoDefaultValue #-}

-- | A list of source fields to map to the field.
laoSourceFields :: Lens' LiteralArrayOptions (Maybe Text)
laoSourceFields f x =
    f (_laoSourceFields x)
        <&> \y -> x { _laoSourceFields = y }
{-# INLINE laoSourceFields #-}

-- | Whether facet information can be returned for the field.
laoFacetEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoFacetEnabled f x =
    f (_laoFacetEnabled x)
        <&> \y -> x { _laoFacetEnabled = y }
{-# INLINE laoFacetEnabled #-}

-- | Whether the contents of the field are searchable.
laoSearchEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoSearchEnabled f x =
    f (_laoSearchEnabled x)
        <&> \y -> x { _laoSearchEnabled = y }
{-# INLINE laoSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
laoReturnEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoReturnEnabled f x =
    f (_laoReturnEnabled x)
        <&> \y -> x { _laoReturnEnabled = y }
{-# INLINE laoReturnEnabled #-}

instance FromXML LiteralArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LiteralArrayOptions"

instance ToQuery LiteralArrayOptions where
    toQuery = genericQuery def

-- | Options for literal field. Present if IndexFieldType specifies the field is
-- of type literal. All options are enabled by default.
data LiteralOptions = LiteralOptions
    { _loDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _loSourceField :: Maybe Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _loFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _loSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _loReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _loSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
loDefaultValue :: Lens' LiteralOptions (Maybe Text)
loDefaultValue f x =
    f (_loDefaultValue x)
        <&> \y -> x { _loDefaultValue = y }
{-# INLINE loDefaultValue #-}

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
loSourceField :: Lens' LiteralOptions (Maybe Text)
loSourceField f x =
    f (_loSourceField x)
        <&> \y -> x { _loSourceField = y }
{-# INLINE loSourceField #-}

-- | Whether facet information can be returned for the field.
loFacetEnabled :: Lens' LiteralOptions (Maybe Bool)
loFacetEnabled f x =
    f (_loFacetEnabled x)
        <&> \y -> x { _loFacetEnabled = y }
{-# INLINE loFacetEnabled #-}

-- | Whether the contents of the field are searchable.
loSearchEnabled :: Lens' LiteralOptions (Maybe Bool)
loSearchEnabled f x =
    f (_loSearchEnabled x)
        <&> \y -> x { _loSearchEnabled = y }
{-# INLINE loSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
loReturnEnabled :: Lens' LiteralOptions (Maybe Bool)
loReturnEnabled f x =
    f (_loReturnEnabled x)
        <&> \y -> x { _loReturnEnabled = y }
{-# INLINE loReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
loSortEnabled :: Lens' LiteralOptions (Maybe Bool)
loSortEnabled f x =
    f (_loSortEnabled x)
        <&> \y -> x { _loSortEnabled = y }
{-# INLINE loSortEnabled #-}

instance FromXML LiteralOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LiteralOptions"

instance ToQuery LiteralOptions where
    toQuery = genericQuery def

-- | The status of domain configuration option.
data OptionStatus = OptionStatus
    { _osCreationDate :: ISO8601
      -- ^ A timestamp for when this option was created.
    , _osUpdateDate :: ISO8601
      -- ^ A timestamp for when this option was last updated.
    , _osUpdateVersion :: Maybe Integer
      -- ^ A unique integer that indicates when this option was last
      -- updated.
    , _osState :: OptionState
      -- ^ The state of processing a change to an option. Possible values:
      -- RequiresIndexDocuments: the option's latest value will not be
      -- deployed until IndexDocuments has been called and indexing is
      -- complete. Processing: the option's latest value is in the process
      -- of being activated. Active: the option's latest value is
      -- completely deployed. FailedToValidate: the option value is not
      -- compatible with the domain's data and cannot be used to index the
      -- data. You must either modify the option value or update or remove
      -- the incompatible documents.
    , _osPendingDeletion :: Maybe Bool
      -- ^ Indicates that the option will be deleted once processing is
      -- complete.
    } deriving (Show, Generic)

-- | A timestamp for when this option was created.
osCreationDate :: Lens' OptionStatus (ISO8601)
osCreationDate f x =
    f (_osCreationDate x)
        <&> \y -> x { _osCreationDate = y }
{-# INLINE osCreationDate #-}

-- | A timestamp for when this option was last updated.
osUpdateDate :: Lens' OptionStatus (ISO8601)
osUpdateDate f x =
    f (_osUpdateDate x)
        <&> \y -> x { _osUpdateDate = y }
{-# INLINE osUpdateDate #-}

-- | A unique integer that indicates when this option was last updated.
osUpdateVersion :: Lens' OptionStatus (Maybe Integer)
osUpdateVersion f x =
    f (_osUpdateVersion x)
        <&> \y -> x { _osUpdateVersion = y }
{-# INLINE osUpdateVersion #-}

-- | The state of processing a change to an option. Possible values:
-- RequiresIndexDocuments: the option's latest value will not be deployed
-- until IndexDocuments has been called and indexing is complete. Processing:
-- the option's latest value is in the process of being activated. Active: the
-- option's latest value is completely deployed. FailedToValidate: the option
-- value is not compatible with the domain's data and cannot be used to index
-- the data. You must either modify the option value or update or remove the
-- incompatible documents.
osState :: Lens' OptionStatus (OptionState)
osState f x =
    f (_osState x)
        <&> \y -> x { _osState = y }
{-# INLINE osState #-}

-- | Indicates that the option will be deleted once processing is complete.
osPendingDeletion :: Lens' OptionStatus (Maybe Bool)
osPendingDeletion f x =
    f (_osPendingDeletion x)
        <&> \y -> x { _osPendingDeletion = y }
{-# INLINE osPendingDeletion #-}

instance FromXML OptionStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionStatus"

instance ToQuery OptionStatus where
    toQuery = genericQuery def

-- | The desired instance type and desired number of replicas of each index
-- partition.
data ScalingParameters = ScalingParameters
    { _sssDesiredInstanceType :: Maybe PartitionInstanceType
      -- ^ The instance type that you want to preconfigure for your domain.
      -- For example, search.m1.small.
    , _sssDesiredReplicationCount :: Maybe Integer
      -- ^ The number of replicas you want to preconfigure for each index
      -- partition.
    , _sssDesiredPartitionCount :: Maybe Integer
      -- ^ The number of partitions you want to preconfigure for your
      -- domain. Only valid when you select m2.2xlarge as the desired
      -- instance type.
    } deriving (Show, Generic)

-- | The instance type that you want to preconfigure for your domain. For
-- example, search.m1.small.
sssDesiredInstanceType :: Lens' ScalingParameters (Maybe PartitionInstanceType)
sssDesiredInstanceType f x =
    f (_sssDesiredInstanceType x)
        <&> \y -> x { _sssDesiredInstanceType = y }
{-# INLINE sssDesiredInstanceType #-}

-- | The number of replicas you want to preconfigure for each index partition.
sssDesiredReplicationCount :: Lens' ScalingParameters (Maybe Integer)
sssDesiredReplicationCount f x =
    f (_sssDesiredReplicationCount x)
        <&> \y -> x { _sssDesiredReplicationCount = y }
{-# INLINE sssDesiredReplicationCount #-}

-- | The number of partitions you want to preconfigure for your domain. Only
-- valid when you select m2.2xlarge as the desired instance type.
sssDesiredPartitionCount :: Lens' ScalingParameters (Maybe Integer)
sssDesiredPartitionCount f x =
    f (_sssDesiredPartitionCount x)
        <&> \y -> x { _sssDesiredPartitionCount = y }
{-# INLINE sssDesiredPartitionCount #-}

instance FromXML ScalingParameters where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingParameters"

instance ToQuery ScalingParameters where
    toQuery = genericQuery def

-- | The status and configuration of a search domain's scaling parameters.
data ScalingParametersStatus = ScalingParametersStatus
    { _spsOptions :: ScalingParameters
      -- ^ The desired instance type and desired number of replicas of each
      -- index partition.
    , _spsStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    } deriving (Show, Generic)

-- | The desired instance type and desired number of replicas of each index
-- partition.
spsOptions :: Lens' ScalingParametersStatus (ScalingParameters)
spsOptions f x =
    f (_spsOptions x)
        <&> \y -> x { _spsOptions = y }
{-# INLINE spsOptions #-}

-- | The status of domain configuration option.
spsStatus :: Lens' ScalingParametersStatus (OptionStatus)
spsStatus f x =
    f (_spsStatus x)
        <&> \y -> x { _spsStatus = y }
{-# INLINE spsStatus #-}

instance FromXML ScalingParametersStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingParametersStatus"

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for suggestions.
-- The following options can be configured for a suggester: FuzzyMatching,
-- SortExpression.
data Suggester = Suggester
    { _srSuggesterName :: Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _srDocumentSuggesterOptions :: DocumentSuggesterOptions
      -- ^ Options for a search suggester.
    } deriving (Show, Generic)

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
srSuggesterName :: Lens' Suggester (Text)
srSuggesterName f x =
    f (_srSuggesterName x)
        <&> \y -> x { _srSuggesterName = y }
{-# INLINE srSuggesterName #-}

-- | Options for a search suggester.
srDocumentSuggesterOptions :: Lens' Suggester (DocumentSuggesterOptions)
srDocumentSuggesterOptions f x =
    f (_srDocumentSuggesterOptions x)
        <&> \y -> x { _srDocumentSuggesterOptions = y }
{-# INLINE srDocumentSuggesterOptions #-}

instance FromXML Suggester where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Suggester"

instance ToQuery Suggester where
    toQuery = genericQuery def

-- | The value of a Suggester and its current status.
data SuggesterStatus = SuggesterStatus
    { _ssOptions :: Suggester
      -- ^ Configuration information for a search suggester. Each suggester
      -- has a unique name and specifies the text field you want to use
      -- for suggestions. The following options can be configured for a
      -- suggester: FuzzyMatching, SortExpression.
    , _ssStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    } deriving (Show, Generic)

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for suggestions.
-- The following options can be configured for a suggester: FuzzyMatching,
-- SortExpression.
ssOptions :: Lens' SuggesterStatus (Suggester)
ssOptions f x =
    f (_ssOptions x)
        <&> \y -> x { _ssOptions = y }
{-# INLINE ssOptions #-}

-- | The status of domain configuration option.
ssStatus :: Lens' SuggesterStatus (OptionStatus)
ssStatus f x =
    f (_ssStatus x)
        <&> \y -> x { _ssStatus = y }
{-# INLINE ssStatus #-}

instance FromXML SuggesterStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuggesterStatus"

-- | Options for a field that contains an array of text strings. Present if
-- IndexFieldType specifies the field is of type text-array. A text-array
-- field is always searchable. All options are enabled by default.
data TextArrayOptions = TextArrayOptions
    { _taoDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _taoSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _taoReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _taoHighlightEnabled :: Maybe Bool
      -- ^ Whether highlights can be returned for the field.
    , _taoAnalysisScheme :: Maybe Text
      -- ^ The name of an analysis scheme for a text-array field.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
taoDefaultValue :: Lens' TextArrayOptions (Maybe Text)
taoDefaultValue f x =
    f (_taoDefaultValue x)
        <&> \y -> x { _taoDefaultValue = y }
{-# INLINE taoDefaultValue #-}

-- | A list of source fields to map to the field.
taoSourceFields :: Lens' TextArrayOptions (Maybe Text)
taoSourceFields f x =
    f (_taoSourceFields x)
        <&> \y -> x { _taoSourceFields = y }
{-# INLINE taoSourceFields #-}

-- | Whether the contents of the field can be returned in the search results.
taoReturnEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoReturnEnabled f x =
    f (_taoReturnEnabled x)
        <&> \y -> x { _taoReturnEnabled = y }
{-# INLINE taoReturnEnabled #-}

-- | Whether highlights can be returned for the field.
taoHighlightEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoHighlightEnabled f x =
    f (_taoHighlightEnabled x)
        <&> \y -> x { _taoHighlightEnabled = y }
{-# INLINE taoHighlightEnabled #-}

-- | The name of an analysis scheme for a text-array field.
taoAnalysisScheme :: Lens' TextArrayOptions (Maybe Text)
taoAnalysisScheme f x =
    f (_taoAnalysisScheme x)
        <&> \y -> x { _taoAnalysisScheme = y }
{-# INLINE taoAnalysisScheme #-}

instance FromXML TextArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TextArrayOptions"

instance ToQuery TextArrayOptions where
    toQuery = genericQuery def

-- | Options for text field. Present if IndexFieldType specifies the field is of
-- type text. A text field is always searchable. All options are enabled by
-- default.
data TextOptions = TextOptions
    { _toDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    , _toSourceField :: Maybe Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _toReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _toSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    , _toHighlightEnabled :: Maybe Bool
      -- ^ Whether highlights can be returned for the field.
    , _toAnalysisScheme :: Maybe Text
      -- ^ The name of an analysis scheme for a text field.
    } deriving (Show, Generic)

-- | A value to use for the field if the field isn't specified for a document.
toDefaultValue :: Lens' TextOptions (Maybe Text)
toDefaultValue f x =
    f (_toDefaultValue x)
        <&> \y -> x { _toDefaultValue = y }
{-# INLINE toDefaultValue #-}

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
toSourceField :: Lens' TextOptions (Maybe Text)
toSourceField f x =
    f (_toSourceField x)
        <&> \y -> x { _toSourceField = y }
{-# INLINE toSourceField #-}

-- | Whether the contents of the field can be returned in the search results.
toReturnEnabled :: Lens' TextOptions (Maybe Bool)
toReturnEnabled f x =
    f (_toReturnEnabled x)
        <&> \y -> x { _toReturnEnabled = y }
{-# INLINE toReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
toSortEnabled :: Lens' TextOptions (Maybe Bool)
toSortEnabled f x =
    f (_toSortEnabled x)
        <&> \y -> x { _toSortEnabled = y }
{-# INLINE toSortEnabled #-}

-- | Whether highlights can be returned for the field.
toHighlightEnabled :: Lens' TextOptions (Maybe Bool)
toHighlightEnabled f x =
    f (_toHighlightEnabled x)
        <&> \y -> x { _toHighlightEnabled = y }
{-# INLINE toHighlightEnabled #-}

-- | The name of an analysis scheme for a text field.
toAnalysisScheme :: Lens' TextOptions (Maybe Text)
toAnalysisScheme f x =
    f (_toAnalysisScheme x)
        <&> \y -> x { _toAnalysisScheme = y }
{-# INLINE toAnalysisScheme #-}

instance FromXML TextOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TextOptions"

instance ToQuery TextOptions where
    toQuery = genericQuery def
