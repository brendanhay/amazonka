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
seEndpoint
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ServiceEndpoint
    -> f ServiceEndpoint
seEndpoint f x =
    (\y -> x { _seEndpoint = y })
       <$> f (_seEndpoint x)
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
apsOptions
    :: Functor f
    => (Text
    -> f (Text))
    -> AccessPoliciesStatus
    -> f AccessPoliciesStatus
apsOptions f x =
    (\y -> x { _apsOptions = y })
       <$> f (_apsOptions x)
{-# INLINE apsOptions #-}

-- | The status of domain configuration option.
apsStatus
    :: Functor f
    => (OptionStatus
    -> f (OptionStatus))
    -> AccessPoliciesStatus
    -> f AccessPoliciesStatus
apsStatus f x =
    (\y -> x { _apsStatus = y })
       <$> f (_apsStatus x)
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
aoSynonyms
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AnalysisOptions
    -> f AnalysisOptions
aoSynonyms f x =
    (\y -> x { _aoSynonyms = y })
       <$> f (_aoSynonyms x)
{-# INLINE aoSynonyms #-}

-- | A JSON array of terms to ignore during indexing and searching. For example,
-- ["a", "an", "the", "of"]. The stopwords dictionary must explicitly list
-- each word you want to ignore. Wildcards and regular expressions are not
-- supported.
aoStopwords
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AnalysisOptions
    -> f AnalysisOptions
aoStopwords f x =
    (\y -> x { _aoStopwords = y })
       <$> f (_aoStopwords x)
{-# INLINE aoStopwords #-}

-- | A JSON object that contains a collection of string:value pairs that each
-- map a term to its stem. For example, {"term1": "stem1", "term2": "stem2",
-- "term3": "stem3"}. The stemming dictionary is applied in addition to any
-- algorithmic stemming. This enables you to override the results of the
-- algorithmic stemming to correct specific cases of overstemming or
-- understemming. The maximum size of a stemming dictionary is 500 KB.
aoStemmingDictionary
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AnalysisOptions
    -> f AnalysisOptions
aoStemmingDictionary f x =
    (\y -> x { _aoStemmingDictionary = y })
       <$> f (_aoStemmingDictionary x)
{-# INLINE aoStemmingDictionary #-}

-- | The level of algorithmic stemming to perform: none, minimal, light, or
-- full. The available levels vary depending on the language. For more
-- information, see Language Specific Text Processing Settings in the Amazon
-- CloudSearch Developer Guide.
aoAlgorithmicStemming
    :: Functor f
    => (Maybe AlgorithmicStemming
    -> f (Maybe AlgorithmicStemming))
    -> AnalysisOptions
    -> f AnalysisOptions
aoAlgorithmicStemming f x =
    (\y -> x { _aoAlgorithmicStemming = y })
       <$> f (_aoAlgorithmicStemming x)
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
asAnalysisSchemeName
    :: Functor f
    => (Text
    -> f (Text))
    -> AnalysisScheme
    -> f AnalysisScheme
asAnalysisSchemeName f x =
    (\y -> x { _asAnalysisSchemeName = y })
       <$> f (_asAnalysisSchemeName x)
{-# INLINE asAnalysisSchemeName #-}

-- | An IETF RFC 4646 language code or mul for multiple languages.
asAnalysisSchemeLanguage
    :: Functor f
    => (AnalysisSchemeLanguage
    -> f (AnalysisSchemeLanguage))
    -> AnalysisScheme
    -> f AnalysisScheme
asAnalysisSchemeLanguage f x =
    (\y -> x { _asAnalysisSchemeLanguage = y })
       <$> f (_asAnalysisSchemeLanguage x)
{-# INLINE asAnalysisSchemeLanguage #-}

-- | Synonyms, stopwords, and stemming options for an analysis scheme.
asAnalysisOptions
    :: Functor f
    => (Maybe AnalysisOptions
    -> f (Maybe AnalysisOptions))
    -> AnalysisScheme
    -> f AnalysisScheme
asAnalysisOptions f x =
    (\y -> x { _asAnalysisOptions = y })
       <$> f (_asAnalysisOptions x)
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
assOptions
    :: Functor f
    => (AnalysisScheme
    -> f (AnalysisScheme))
    -> AnalysisSchemeStatus
    -> f AnalysisSchemeStatus
assOptions f x =
    (\y -> x { _assOptions = y })
       <$> f (_assOptions x)
{-# INLINE assOptions #-}

-- | The status of domain configuration option.
assStatus
    :: Functor f
    => (OptionStatus
    -> f (OptionStatus))
    -> AnalysisSchemeStatus
    -> f AnalysisSchemeStatus
assStatus f x =
    (\y -> x { _assStatus = y })
       <$> f (_assStatus x)
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
aosOptions
    :: Functor f
    => (Bool
    -> f (Bool))
    -> AvailabilityOptionsStatus
    -> f AvailabilityOptionsStatus
aosOptions f x =
    (\y -> x { _aosOptions = y })
       <$> f (_aosOptions x)
{-# INLINE aosOptions #-}

-- | The status of domain configuration option.
aosStatus
    :: Functor f
    => (OptionStatus
    -> f (OptionStatus))
    -> AvailabilityOptionsStatus
    -> f AvailabilityOptionsStatus
aosStatus f x =
    (\y -> x { _aosStatus = y })
       <$> f (_aosStatus x)
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
dapDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DateArrayOptions
    -> f DateArrayOptions
dapDefaultValue f x =
    (\y -> x { _dapDefaultValue = y })
       <$> f (_dapDefaultValue x)
{-# INLINE dapDefaultValue #-}

-- | A list of source fields to map to the field.
dapSourceFields
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DateArrayOptions
    -> f DateArrayOptions
dapSourceFields f x =
    (\y -> x { _dapSourceFields = y })
       <$> f (_dapSourceFields x)
{-# INLINE dapSourceFields #-}

-- | Whether facet information can be returned for the field.
dapFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DateArrayOptions
    -> f DateArrayOptions
dapFacetEnabled f x =
    (\y -> x { _dapFacetEnabled = y })
       <$> f (_dapFacetEnabled x)
{-# INLINE dapFacetEnabled #-}

-- | Whether the contents of the field are searchable.
dapSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DateArrayOptions
    -> f DateArrayOptions
dapSearchEnabled f x =
    (\y -> x { _dapSearchEnabled = y })
       <$> f (_dapSearchEnabled x)
{-# INLINE dapSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
dapReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DateArrayOptions
    -> f DateArrayOptions
dapReturnEnabled f x =
    (\y -> x { _dapReturnEnabled = y })
       <$> f (_dapReturnEnabled x)
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
dvDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DateOptions
    -> f DateOptions
dvDefaultValue f x =
    (\y -> x { _dvDefaultValue = y })
       <$> f (_dvDefaultValue x)
{-# INLINE dvDefaultValue #-}

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
dvSourceField
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DateOptions
    -> f DateOptions
dvSourceField f x =
    (\y -> x { _dvSourceField = y })
       <$> f (_dvSourceField x)
{-# INLINE dvSourceField #-}

-- | Whether facet information can be returned for the field.
dvFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DateOptions
    -> f DateOptions
dvFacetEnabled f x =
    (\y -> x { _dvFacetEnabled = y })
       <$> f (_dvFacetEnabled x)
{-# INLINE dvFacetEnabled #-}

-- | Whether the contents of the field are searchable.
dvSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DateOptions
    -> f DateOptions
dvSearchEnabled f x =
    (\y -> x { _dvSearchEnabled = y })
       <$> f (_dvSearchEnabled x)
{-# INLINE dvSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
dvReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DateOptions
    -> f DateOptions
dvReturnEnabled f x =
    (\y -> x { _dvReturnEnabled = y })
       <$> f (_dvReturnEnabled x)
{-# INLINE dvReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
dvSortEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DateOptions
    -> f DateOptions
dvSortEnabled f x =
    (\y -> x { _dvSortEnabled = y })
       <$> f (_dvSortEnabled x)
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
dsoSourceField
    :: Functor f
    => (Text
    -> f (Text))
    -> DocumentSuggesterOptions
    -> f DocumentSuggesterOptions
dsoSourceField f x =
    (\y -> x { _dsoSourceField = y })
       <$> f (_dsoSourceField x)
{-# INLINE dsoSourceField #-}

-- | The level of fuzziness allowed when suggesting matches for a string: none,
-- low, or high. With none, the specified string is treated as an exact
-- prefix. With low, suggestions must differ from the specified string by no
-- more than one character. With high, suggestions can differ by up to two
-- characters. The default is none.
dsoFuzzyMatching
    :: Functor f
    => (Maybe SuggesterFuzzyMatching
    -> f (Maybe SuggesterFuzzyMatching))
    -> DocumentSuggesterOptions
    -> f DocumentSuggesterOptions
dsoFuzzyMatching f x =
    (\y -> x { _dsoFuzzyMatching = y })
       <$> f (_dsoFuzzyMatching x)
{-# INLINE dsoFuzzyMatching #-}

-- | An expression that computes a score for each suggestion to control how they
-- are sorted.
dsoSortExpression
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DocumentSuggesterOptions
    -> f DocumentSuggesterOptions
dsoSortExpression f x =
    (\y -> x { _dsoSortExpression = y })
       <$> f (_dsoSortExpression x)
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
dsDomainId
    :: Functor f
    => (Text
    -> f (Text))
    -> DomainStatus
    -> f DomainStatus
dsDomainId f x =
    (\y -> x { _dsDomainId = y })
       <$> f (_dsDomainId x)
{-# INLINE dsDomainId #-}

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dsDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> DomainStatus
    -> f DomainStatus
dsDomainName f x =
    (\y -> x { _dsDomainName = y })
       <$> f (_dsDomainName x)
{-# INLINE dsDomainName #-}

-- | The Amazon Resource Name (ARN) of the search domain. See Identifiers for
-- IAM Entities in Using AWS Identity and Access Management for more
-- information.
dsARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DomainStatus
    -> f DomainStatus
dsARN f x =
    (\y -> x { _dsARN = y })
       <$> f (_dsARN x)
{-# INLINE dsARN #-}

-- | True if the search domain is created. It can take several minutes to
-- initialize a domain when CreateDomain is called. Newly created search
-- domains are returned from DescribeDomains with a false value for Created
-- until domain creation is complete.
dsCreated
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DomainStatus
    -> f DomainStatus
dsCreated f x =
    (\y -> x { _dsCreated = y })
       <$> f (_dsCreated x)
{-# INLINE dsCreated #-}

-- | True if the search domain has been deleted. The system must clean up
-- resources dedicated to the search domain when DeleteDomain is called. Newly
-- deleted search domains are returned from DescribeDomains with a true value
-- for IsDeleted for several minutes until resource cleanup is complete.
dsDeleted
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DomainStatus
    -> f DomainStatus
dsDeleted f x =
    (\y -> x { _dsDeleted = y })
       <$> f (_dsDeleted x)
{-# INLINE dsDeleted #-}

-- | The service endpoint for updating documents in a search domain.
dsDocService
    :: Functor f
    => (Maybe ServiceEndpoint
    -> f (Maybe ServiceEndpoint))
    -> DomainStatus
    -> f DomainStatus
dsDocService f x =
    (\y -> x { _dsDocService = y })
       <$> f (_dsDocService x)
{-# INLINE dsDocService #-}

-- | The service endpoint for requesting search results from a search domain.
dsSearchService
    :: Functor f
    => (Maybe ServiceEndpoint
    -> f (Maybe ServiceEndpoint))
    -> DomainStatus
    -> f DomainStatus
dsSearchService f x =
    (\y -> x { _dsSearchService = y })
       <$> f (_dsSearchService x)
{-# INLINE dsSearchService #-}

-- | True if IndexDocuments needs to be called to activate the current domain
-- configuration.
dsRequiresIndexDocuments
    :: Functor f
    => (Bool
    -> f (Bool))
    -> DomainStatus
    -> f DomainStatus
dsRequiresIndexDocuments f x =
    (\y -> x { _dsRequiresIndexDocuments = y })
       <$> f (_dsRequiresIndexDocuments x)
{-# INLINE dsRequiresIndexDocuments #-}

-- | True if processing is being done to activate the current domain
-- configuration.
dsProcessing
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DomainStatus
    -> f DomainStatus
dsProcessing f x =
    (\y -> x { _dsProcessing = y })
       <$> f (_dsProcessing x)
{-# INLINE dsProcessing #-}

-- | The instance type that is being used to process search requests.
dsSearchInstanceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DomainStatus
    -> f DomainStatus
dsSearchInstanceType f x =
    (\y -> x { _dsSearchInstanceType = y })
       <$> f (_dsSearchInstanceType x)
{-# INLINE dsSearchInstanceType #-}

-- | The number of partitions across which the search index is spread.
dsSearchPartitionCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainStatus
    -> f DomainStatus
dsSearchPartitionCount f x =
    (\y -> x { _dsSearchPartitionCount = y })
       <$> f (_dsSearchPartitionCount x)
{-# INLINE dsSearchPartitionCount #-}

-- | The number of search instances that are available to process search
-- requests.
dsSearchInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainStatus
    -> f DomainStatus
dsSearchInstanceCount f x =
    (\y -> x { _dsSearchInstanceCount = y })
       <$> f (_dsSearchInstanceCount x)
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
daoDefaultValue
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> DoubleArrayOptions
    -> f DoubleArrayOptions
daoDefaultValue f x =
    (\y -> x { _daoDefaultValue = y })
       <$> f (_daoDefaultValue x)
{-# INLINE daoDefaultValue #-}

-- | A list of source fields to map to the field.
daoSourceFields
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DoubleArrayOptions
    -> f DoubleArrayOptions
daoSourceFields f x =
    (\y -> x { _daoSourceFields = y })
       <$> f (_daoSourceFields x)
{-# INLINE daoSourceFields #-}

-- | Whether facet information can be returned for the field.
daoFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DoubleArrayOptions
    -> f DoubleArrayOptions
daoFacetEnabled f x =
    (\y -> x { _daoFacetEnabled = y })
       <$> f (_daoFacetEnabled x)
{-# INLINE daoFacetEnabled #-}

-- | Whether the contents of the field are searchable.
daoSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DoubleArrayOptions
    -> f DoubleArrayOptions
daoSearchEnabled f x =
    (\y -> x { _daoSearchEnabled = y })
       <$> f (_daoSearchEnabled x)
{-# INLINE daoSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
daoReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DoubleArrayOptions
    -> f DoubleArrayOptions
daoReturnEnabled f x =
    (\y -> x { _daoReturnEnabled = y })
       <$> f (_daoReturnEnabled x)
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
duDefaultValue
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> DoubleOptions
    -> f DoubleOptions
duDefaultValue f x =
    (\y -> x { _duDefaultValue = y })
       <$> f (_duDefaultValue x)
{-# INLINE duDefaultValue #-}

-- | The name of the source field to map to the field.
duSourceField
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DoubleOptions
    -> f DoubleOptions
duSourceField f x =
    (\y -> x { _duSourceField = y })
       <$> f (_duSourceField x)
{-# INLINE duSourceField #-}

-- | Whether facet information can be returned for the field.
duFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DoubleOptions
    -> f DoubleOptions
duFacetEnabled f x =
    (\y -> x { _duFacetEnabled = y })
       <$> f (_duFacetEnabled x)
{-# INLINE duFacetEnabled #-}

-- | Whether the contents of the field are searchable.
duSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DoubleOptions
    -> f DoubleOptions
duSearchEnabled f x =
    (\y -> x { _duSearchEnabled = y })
       <$> f (_duSearchEnabled x)
{-# INLINE duSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
duReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DoubleOptions
    -> f DoubleOptions
duReturnEnabled f x =
    (\y -> x { _duReturnEnabled = y })
       <$> f (_duReturnEnabled x)
{-# INLINE duReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
duSortEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DoubleOptions
    -> f DoubleOptions
duSortEnabled f x =
    (\y -> x { _duSortEnabled = y })
       <$> f (_duSortEnabled x)
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
gExpressionName
    :: Functor f
    => (Text
    -> f (Text))
    -> Expression
    -> f Expression
gExpressionName f x =
    (\y -> x { _gExpressionName = y })
       <$> f (_gExpressionName x)
{-# INLINE gExpressionName #-}

-- | The expression to evaluate for sorting while processing a search request.
-- The Expression syntax is based on JavaScript expressions. For more
-- information, see Configuring Expressions in the Amazon CloudSearch
-- Developer Guide.
gExpressionValue
    :: Functor f
    => (Text
    -> f (Text))
    -> Expression
    -> f Expression
gExpressionValue f x =
    (\y -> x { _gExpressionValue = y })
       <$> f (_gExpressionValue x)
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
esOptions
    :: Functor f
    => (Expression
    -> f (Expression))
    -> ExpressionStatus
    -> f ExpressionStatus
esOptions f x =
    (\y -> x { _esOptions = y })
       <$> f (_esOptions x)
{-# INLINE esOptions #-}

-- | The status of domain configuration option.
esStatus
    :: Functor f
    => (OptionStatus
    -> f (OptionStatus))
    -> ExpressionStatus
    -> f ExpressionStatus
esStatus f x =
    (\y -> x { _esStatus = y })
       <$> f (_esStatus x)
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
ifIndexFieldName
    :: Functor f
    => (Text
    -> f (Text))
    -> IndexField
    -> f IndexField
ifIndexFieldName f x =
    (\y -> x { _ifIndexFieldName = y })
       <$> f (_ifIndexFieldName x)
{-# INLINE ifIndexFieldName #-}

-- | The type of field. The valid options for a field depend on the field type.
-- For more information about the supported field types, see Configuring Index
-- Fields in the Amazon CloudSearch Developer Guide.
ifIndexFieldType
    :: Functor f
    => (IndexFieldType
    -> f (IndexFieldType))
    -> IndexField
    -> f IndexField
ifIndexFieldType f x =
    (\y -> x { _ifIndexFieldType = y })
       <$> f (_ifIndexFieldType x)
{-# INLINE ifIndexFieldType #-}

-- | Options for a 64-bit signed integer field. Present if IndexFieldType
-- specifies the field is of type int. All options are enabled by default.
ifIntOptions
    :: Functor f
    => (Maybe IntOptions
    -> f (Maybe IntOptions))
    -> IndexField
    -> f IndexField
ifIntOptions f x =
    (\y -> x { _ifIntOptions = y })
       <$> f (_ifIntOptions x)
{-# INLINE ifIntOptions #-}

-- | Options for a double-precision 64-bit floating point field. Present if
-- IndexFieldType specifies the field is of type double. All options are
-- enabled by default.
ifDoubleOptions
    :: Functor f
    => (Maybe DoubleOptions
    -> f (Maybe DoubleOptions))
    -> IndexField
    -> f IndexField
ifDoubleOptions f x =
    (\y -> x { _ifDoubleOptions = y })
       <$> f (_ifDoubleOptions x)
{-# INLINE ifDoubleOptions #-}

-- | Options for literal field. Present if IndexFieldType specifies the field is
-- of type literal. All options are enabled by default.
ifLiteralOptions
    :: Functor f
    => (Maybe LiteralOptions
    -> f (Maybe LiteralOptions))
    -> IndexField
    -> f IndexField
ifLiteralOptions f x =
    (\y -> x { _ifLiteralOptions = y })
       <$> f (_ifLiteralOptions x)
{-# INLINE ifLiteralOptions #-}

-- | Options for text field. Present if IndexFieldType specifies the field is of
-- type text. A text field is always searchable. All options are enabled by
-- default.
ifTextOptions
    :: Functor f
    => (Maybe TextOptions
    -> f (Maybe TextOptions))
    -> IndexField
    -> f IndexField
ifTextOptions f x =
    (\y -> x { _ifTextOptions = y })
       <$> f (_ifTextOptions x)
{-# INLINE ifTextOptions #-}

-- | Options for a date field. Dates and times are specified in UTC (Coordinated
-- Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if
-- IndexFieldType specifies the field is of type date. All options are enabled
-- by default.
ifDateOptions
    :: Functor f
    => (Maybe DateOptions
    -> f (Maybe DateOptions))
    -> IndexField
    -> f IndexField
ifDateOptions f x =
    (\y -> x { _ifDateOptions = y })
       <$> f (_ifDateOptions x)
{-# INLINE ifDateOptions #-}

-- | Options for a latlon field. A latlon field contains a location stored as a
-- latitude and longitude value pair. Present if IndexFieldType specifies the
-- field is of type latlon. All options are enabled by default.
ifLatLonOptions
    :: Functor f
    => (Maybe LatLonOptions
    -> f (Maybe LatLonOptions))
    -> IndexField
    -> f IndexField
ifLatLonOptions f x =
    (\y -> x { _ifLatLonOptions = y })
       <$> f (_ifLatLonOptions x)
{-# INLINE ifLatLonOptions #-}

-- | Options for a field that contains an array of 64-bit signed integers.
-- Present if IndexFieldType specifies the field is of type int-array. All
-- options are enabled by default.
ifIntArrayOptions
    :: Functor f
    => (Maybe IntArrayOptions
    -> f (Maybe IntArrayOptions))
    -> IndexField
    -> f IndexField
ifIntArrayOptions f x =
    (\y -> x { _ifIntArrayOptions = y })
       <$> f (_ifIntArrayOptions x)
{-# INLINE ifIntArrayOptions #-}

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if IndexFieldType specifies the field is of
-- type double-array. All options are enabled by default.
ifDoubleArrayOptions
    :: Functor f
    => (Maybe DoubleArrayOptions
    -> f (Maybe DoubleArrayOptions))
    -> IndexField
    -> f IndexField
ifDoubleArrayOptions f x =
    (\y -> x { _ifDoubleArrayOptions = y })
       <$> f (_ifDoubleArrayOptions x)
{-# INLINE ifDoubleArrayOptions #-}

-- | Options for a field that contains an array of literal strings. Present if
-- IndexFieldType specifies the field is of type literal-array. All options
-- are enabled by default.
ifLiteralArrayOptions
    :: Functor f
    => (Maybe LiteralArrayOptions
    -> f (Maybe LiteralArrayOptions))
    -> IndexField
    -> f IndexField
ifLiteralArrayOptions f x =
    (\y -> x { _ifLiteralArrayOptions = y })
       <$> f (_ifLiteralArrayOptions x)
{-# INLINE ifLiteralArrayOptions #-}

-- | Options for a field that contains an array of text strings. Present if
-- IndexFieldType specifies the field is of type text-array. A text-array
-- field is always searchable. All options are enabled by default.
ifTextArrayOptions
    :: Functor f
    => (Maybe TextArrayOptions
    -> f (Maybe TextArrayOptions))
    -> IndexField
    -> f IndexField
ifTextArrayOptions f x =
    (\y -> x { _ifTextArrayOptions = y })
       <$> f (_ifTextArrayOptions x)
{-# INLINE ifTextArrayOptions #-}

-- | Options for a field that contains an array of dates. Present if
-- IndexFieldType specifies the field is of type date-array. All options are
-- enabled by default.
ifDateArrayOptions
    :: Functor f
    => (Maybe DateArrayOptions
    -> f (Maybe DateArrayOptions))
    -> IndexField
    -> f IndexField
ifDateArrayOptions f x =
    (\y -> x { _ifDateArrayOptions = y })
       <$> f (_ifDateArrayOptions x)
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
ifsOptions
    :: Functor f
    => (IndexField
    -> f (IndexField))
    -> IndexFieldStatus
    -> f IndexFieldStatus
ifsOptions f x =
    (\y -> x { _ifsOptions = y })
       <$> f (_ifsOptions x)
{-# INLINE ifsOptions #-}

-- | The status of domain configuration option.
ifsStatus
    :: Functor f
    => (OptionStatus
    -> f (OptionStatus))
    -> IndexFieldStatus
    -> f IndexFieldStatus
ifsStatus f x =
    (\y -> x { _ifsStatus = y })
       <$> f (_ifsStatus x)
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
iaoDefaultValue
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> IntArrayOptions
    -> f IntArrayOptions
iaoDefaultValue f x =
    (\y -> x { _iaoDefaultValue = y })
       <$> f (_iaoDefaultValue x)
{-# INLINE iaoDefaultValue #-}

-- | A list of source fields to map to the field.
iaoSourceFields
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IntArrayOptions
    -> f IntArrayOptions
iaoSourceFields f x =
    (\y -> x { _iaoSourceFields = y })
       <$> f (_iaoSourceFields x)
{-# INLINE iaoSourceFields #-}

-- | Whether facet information can be returned for the field.
iaoFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> IntArrayOptions
    -> f IntArrayOptions
iaoFacetEnabled f x =
    (\y -> x { _iaoFacetEnabled = y })
       <$> f (_iaoFacetEnabled x)
{-# INLINE iaoFacetEnabled #-}

-- | Whether the contents of the field are searchable.
iaoSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> IntArrayOptions
    -> f IntArrayOptions
iaoSearchEnabled f x =
    (\y -> x { _iaoSearchEnabled = y })
       <$> f (_iaoSearchEnabled x)
{-# INLINE iaoSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
iaoReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> IntArrayOptions
    -> f IntArrayOptions
iaoReturnEnabled f x =
    (\y -> x { _iaoReturnEnabled = y })
       <$> f (_iaoReturnEnabled x)
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
ioDefaultValue
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> IntOptions
    -> f IntOptions
ioDefaultValue f x =
    (\y -> x { _ioDefaultValue = y })
       <$> f (_ioDefaultValue x)
{-# INLINE ioDefaultValue #-}

-- | The name of the source field to map to the field.
ioSourceField
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IntOptions
    -> f IntOptions
ioSourceField f x =
    (\y -> x { _ioSourceField = y })
       <$> f (_ioSourceField x)
{-# INLINE ioSourceField #-}

-- | Whether facet information can be returned for the field.
ioFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> IntOptions
    -> f IntOptions
ioFacetEnabled f x =
    (\y -> x { _ioFacetEnabled = y })
       <$> f (_ioFacetEnabled x)
{-# INLINE ioFacetEnabled #-}

-- | Whether the contents of the field are searchable.
ioSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> IntOptions
    -> f IntOptions
ioSearchEnabled f x =
    (\y -> x { _ioSearchEnabled = y })
       <$> f (_ioSearchEnabled x)
{-# INLINE ioSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
ioReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> IntOptions
    -> f IntOptions
ioReturnEnabled f x =
    (\y -> x { _ioReturnEnabled = y })
       <$> f (_ioReturnEnabled x)
{-# INLINE ioReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
ioSortEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> IntOptions
    -> f IntOptions
ioSortEnabled f x =
    (\y -> x { _ioSortEnabled = y })
       <$> f (_ioSortEnabled x)
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
lloDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LatLonOptions
    -> f LatLonOptions
lloDefaultValue f x =
    (\y -> x { _lloDefaultValue = y })
       <$> f (_lloDefaultValue x)
{-# INLINE lloDefaultValue #-}

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
lloSourceField
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LatLonOptions
    -> f LatLonOptions
lloSourceField f x =
    (\y -> x { _lloSourceField = y })
       <$> f (_lloSourceField x)
{-# INLINE lloSourceField #-}

-- | Whether facet information can be returned for the field.
lloFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LatLonOptions
    -> f LatLonOptions
lloFacetEnabled f x =
    (\y -> x { _lloFacetEnabled = y })
       <$> f (_lloFacetEnabled x)
{-# INLINE lloFacetEnabled #-}

-- | Whether the contents of the field are searchable.
lloSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LatLonOptions
    -> f LatLonOptions
lloSearchEnabled f x =
    (\y -> x { _lloSearchEnabled = y })
       <$> f (_lloSearchEnabled x)
{-# INLINE lloSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
lloReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LatLonOptions
    -> f LatLonOptions
lloReturnEnabled f x =
    (\y -> x { _lloReturnEnabled = y })
       <$> f (_lloReturnEnabled x)
{-# INLINE lloReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
lloSortEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LatLonOptions
    -> f LatLonOptions
lloSortEnabled f x =
    (\y -> x { _lloSortEnabled = y })
       <$> f (_lloSortEnabled x)
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
laoDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LiteralArrayOptions
    -> f LiteralArrayOptions
laoDefaultValue f x =
    (\y -> x { _laoDefaultValue = y })
       <$> f (_laoDefaultValue x)
{-# INLINE laoDefaultValue #-}

-- | A list of source fields to map to the field.
laoSourceFields
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LiteralArrayOptions
    -> f LiteralArrayOptions
laoSourceFields f x =
    (\y -> x { _laoSourceFields = y })
       <$> f (_laoSourceFields x)
{-# INLINE laoSourceFields #-}

-- | Whether facet information can be returned for the field.
laoFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LiteralArrayOptions
    -> f LiteralArrayOptions
laoFacetEnabled f x =
    (\y -> x { _laoFacetEnabled = y })
       <$> f (_laoFacetEnabled x)
{-# INLINE laoFacetEnabled #-}

-- | Whether the contents of the field are searchable.
laoSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LiteralArrayOptions
    -> f LiteralArrayOptions
laoSearchEnabled f x =
    (\y -> x { _laoSearchEnabled = y })
       <$> f (_laoSearchEnabled x)
{-# INLINE laoSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
laoReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LiteralArrayOptions
    -> f LiteralArrayOptions
laoReturnEnabled f x =
    (\y -> x { _laoReturnEnabled = y })
       <$> f (_laoReturnEnabled x)
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
loDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LiteralOptions
    -> f LiteralOptions
loDefaultValue f x =
    (\y -> x { _loDefaultValue = y })
       <$> f (_loDefaultValue x)
{-# INLINE loDefaultValue #-}

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
loSourceField
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LiteralOptions
    -> f LiteralOptions
loSourceField f x =
    (\y -> x { _loSourceField = y })
       <$> f (_loSourceField x)
{-# INLINE loSourceField #-}

-- | Whether facet information can be returned for the field.
loFacetEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LiteralOptions
    -> f LiteralOptions
loFacetEnabled f x =
    (\y -> x { _loFacetEnabled = y })
       <$> f (_loFacetEnabled x)
{-# INLINE loFacetEnabled #-}

-- | Whether the contents of the field are searchable.
loSearchEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LiteralOptions
    -> f LiteralOptions
loSearchEnabled f x =
    (\y -> x { _loSearchEnabled = y })
       <$> f (_loSearchEnabled x)
{-# INLINE loSearchEnabled #-}

-- | Whether the contents of the field can be returned in the search results.
loReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LiteralOptions
    -> f LiteralOptions
loReturnEnabled f x =
    (\y -> x { _loReturnEnabled = y })
       <$> f (_loReturnEnabled x)
{-# INLINE loReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
loSortEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LiteralOptions
    -> f LiteralOptions
loSortEnabled f x =
    (\y -> x { _loSortEnabled = y })
       <$> f (_loSortEnabled x)
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
osCreationDate
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> OptionStatus
    -> f OptionStatus
osCreationDate f x =
    (\y -> x { _osCreationDate = y })
       <$> f (_osCreationDate x)
{-# INLINE osCreationDate #-}

-- | A timestamp for when this option was last updated.
osUpdateDate
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> OptionStatus
    -> f OptionStatus
osUpdateDate f x =
    (\y -> x { _osUpdateDate = y })
       <$> f (_osUpdateDate x)
{-# INLINE osUpdateDate #-}

-- | A unique integer that indicates when this option was last updated.
osUpdateVersion
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> OptionStatus
    -> f OptionStatus
osUpdateVersion f x =
    (\y -> x { _osUpdateVersion = y })
       <$> f (_osUpdateVersion x)
{-# INLINE osUpdateVersion #-}

-- | The state of processing a change to an option. Possible values:
-- RequiresIndexDocuments: the option's latest value will not be deployed
-- until IndexDocuments has been called and indexing is complete. Processing:
-- the option's latest value is in the process of being activated. Active: the
-- option's latest value is completely deployed. FailedToValidate: the option
-- value is not compatible with the domain's data and cannot be used to index
-- the data. You must either modify the option value or update or remove the
-- incompatible documents.
osState
    :: Functor f
    => (OptionState
    -> f (OptionState))
    -> OptionStatus
    -> f OptionStatus
osState f x =
    (\y -> x { _osState = y })
       <$> f (_osState x)
{-# INLINE osState #-}

-- | Indicates that the option will be deleted once processing is complete.
osPendingDeletion
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OptionStatus
    -> f OptionStatus
osPendingDeletion f x =
    (\y -> x { _osPendingDeletion = y })
       <$> f (_osPendingDeletion x)
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
sssDesiredInstanceType
    :: Functor f
    => (Maybe PartitionInstanceType
    -> f (Maybe PartitionInstanceType))
    -> ScalingParameters
    -> f ScalingParameters
sssDesiredInstanceType f x =
    (\y -> x { _sssDesiredInstanceType = y })
       <$> f (_sssDesiredInstanceType x)
{-# INLINE sssDesiredInstanceType #-}

-- | The number of replicas you want to preconfigure for each index partition.
sssDesiredReplicationCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ScalingParameters
    -> f ScalingParameters
sssDesiredReplicationCount f x =
    (\y -> x { _sssDesiredReplicationCount = y })
       <$> f (_sssDesiredReplicationCount x)
{-# INLINE sssDesiredReplicationCount #-}

-- | The number of partitions you want to preconfigure for your domain. Only
-- valid when you select m2.2xlarge as the desired instance type.
sssDesiredPartitionCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ScalingParameters
    -> f ScalingParameters
sssDesiredPartitionCount f x =
    (\y -> x { _sssDesiredPartitionCount = y })
       <$> f (_sssDesiredPartitionCount x)
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
spsOptions
    :: Functor f
    => (ScalingParameters
    -> f (ScalingParameters))
    -> ScalingParametersStatus
    -> f ScalingParametersStatus
spsOptions f x =
    (\y -> x { _spsOptions = y })
       <$> f (_spsOptions x)
{-# INLINE spsOptions #-}

-- | The status of domain configuration option.
spsStatus
    :: Functor f
    => (OptionStatus
    -> f (OptionStatus))
    -> ScalingParametersStatus
    -> f ScalingParametersStatus
spsStatus f x =
    (\y -> x { _spsStatus = y })
       <$> f (_spsStatus x)
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
srSuggesterName
    :: Functor f
    => (Text
    -> f (Text))
    -> Suggester
    -> f Suggester
srSuggesterName f x =
    (\y -> x { _srSuggesterName = y })
       <$> f (_srSuggesterName x)
{-# INLINE srSuggesterName #-}

-- | Options for a search suggester.
srDocumentSuggesterOptions
    :: Functor f
    => (DocumentSuggesterOptions
    -> f (DocumentSuggesterOptions))
    -> Suggester
    -> f Suggester
srDocumentSuggesterOptions f x =
    (\y -> x { _srDocumentSuggesterOptions = y })
       <$> f (_srDocumentSuggesterOptions x)
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
ssOptions
    :: Functor f
    => (Suggester
    -> f (Suggester))
    -> SuggesterStatus
    -> f SuggesterStatus
ssOptions f x =
    (\y -> x { _ssOptions = y })
       <$> f (_ssOptions x)
{-# INLINE ssOptions #-}

-- | The status of domain configuration option.
ssStatus
    :: Functor f
    => (OptionStatus
    -> f (OptionStatus))
    -> SuggesterStatus
    -> f SuggesterStatus
ssStatus f x =
    (\y -> x { _ssStatus = y })
       <$> f (_ssStatus x)
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
taoDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TextArrayOptions
    -> f TextArrayOptions
taoDefaultValue f x =
    (\y -> x { _taoDefaultValue = y })
       <$> f (_taoDefaultValue x)
{-# INLINE taoDefaultValue #-}

-- | A list of source fields to map to the field.
taoSourceFields
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TextArrayOptions
    -> f TextArrayOptions
taoSourceFields f x =
    (\y -> x { _taoSourceFields = y })
       <$> f (_taoSourceFields x)
{-# INLINE taoSourceFields #-}

-- | Whether the contents of the field can be returned in the search results.
taoReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TextArrayOptions
    -> f TextArrayOptions
taoReturnEnabled f x =
    (\y -> x { _taoReturnEnabled = y })
       <$> f (_taoReturnEnabled x)
{-# INLINE taoReturnEnabled #-}

-- | Whether highlights can be returned for the field.
taoHighlightEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TextArrayOptions
    -> f TextArrayOptions
taoHighlightEnabled f x =
    (\y -> x { _taoHighlightEnabled = y })
       <$> f (_taoHighlightEnabled x)
{-# INLINE taoHighlightEnabled #-}

-- | The name of an analysis scheme for a text-array field.
taoAnalysisScheme
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TextArrayOptions
    -> f TextArrayOptions
taoAnalysisScheme f x =
    (\y -> x { _taoAnalysisScheme = y })
       <$> f (_taoAnalysisScheme x)
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
toDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TextOptions
    -> f TextOptions
toDefaultValue f x =
    (\y -> x { _toDefaultValue = y })
       <$> f (_toDefaultValue x)
{-# INLINE toDefaultValue #-}

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
toSourceField
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TextOptions
    -> f TextOptions
toSourceField f x =
    (\y -> x { _toSourceField = y })
       <$> f (_toSourceField x)
{-# INLINE toSourceField #-}

-- | Whether the contents of the field can be returned in the search results.
toReturnEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TextOptions
    -> f TextOptions
toReturnEnabled f x =
    (\y -> x { _toReturnEnabled = y })
       <$> f (_toReturnEnabled x)
{-# INLINE toReturnEnabled #-}

-- | Whether the field can be used to sort the search results.
toSortEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TextOptions
    -> f TextOptions
toSortEnabled f x =
    (\y -> x { _toSortEnabled = y })
       <$> f (_toSortEnabled x)
{-# INLINE toSortEnabled #-}

-- | Whether highlights can be returned for the field.
toHighlightEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TextOptions
    -> f TextOptions
toHighlightEnabled f x =
    (\y -> x { _toHighlightEnabled = y })
       <$> f (_toHighlightEnabled x)
{-# INLINE toHighlightEnabled #-}

-- | The name of an analysis scheme for a text field.
toAnalysisScheme
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TextOptions
    -> f TextOptions
toAnalysisScheme f x =
    (\y -> x { _toAnalysisScheme = y })
       <$> f (_toAnalysisScheme x)
{-# INLINE toAnalysisScheme #-}

instance FromXML TextOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TextOptions"

instance ToQuery TextOptions where
    toQuery = genericQuery def
