{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

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
module Network.AWS.CloudSearch.V2013_01_01.Types where

import Control.Applicative
import Control.Exception      (Exception)
import Data.Default
import Data.Tagged
import Data.Text              (Text)
import Data.Typeable
import GHC.Generics
import Network.AWS.Data
import Network.AWS.Signing.V4
import Network.AWS.Types      hiding (Error, Endpoint, Region)
import Network.HTTP.Client    (HttpException)

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

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudsearch"
        , _svcVersion  = "2013-01-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CloudSearch)
deriving instance Generic (Er CloudSearch)

instance AWSError (Er CloudSearch) where
    awsError = const "CloudSearchError"

instance ServiceError (Er CloudSearch) where
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

instance ToByteString AlgorithmicStemming where
    toBS AlgorithmicStemmingFull = "full"
    toBS AlgorithmicStemmingLight = "light"
    toBS AlgorithmicStemmingMinimal = "minimal"
    toBS AlgorithmicStemmingNone = "none"

instance FromXML AlgorithmicStemming where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AlgorithmicStemming"

instance ToQuery AlgorithmicStemming where
    toQuery = genericToQuery def

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

instance ToByteString AnalysisSchemeLanguage where
    toBS AnalysisSchemeLanguageAr = "ar"
    toBS AnalysisSchemeLanguageBg = "bg"
    toBS AnalysisSchemeLanguageCa = "ca"
    toBS AnalysisSchemeLanguageCs = "cs"
    toBS AnalysisSchemeLanguageDa = "da"
    toBS AnalysisSchemeLanguageDe = "de"
    toBS AnalysisSchemeLanguageEl = "el"
    toBS AnalysisSchemeLanguageEn = "en"
    toBS AnalysisSchemeLanguageEs = "es"
    toBS AnalysisSchemeLanguageEu = "eu"
    toBS AnalysisSchemeLanguageFa = "fa"
    toBS AnalysisSchemeLanguageFi = "fi"
    toBS AnalysisSchemeLanguageFr = "fr"
    toBS AnalysisSchemeLanguageGa = "ga"
    toBS AnalysisSchemeLanguageGl = "gl"
    toBS AnalysisSchemeLanguageHe = "he"
    toBS AnalysisSchemeLanguageHi = "hi"
    toBS AnalysisSchemeLanguageHu = "hu"
    toBS AnalysisSchemeLanguageHy = "hy"
    toBS AnalysisSchemeLanguageId = "id"
    toBS AnalysisSchemeLanguageIt = "it"
    toBS AnalysisSchemeLanguageJa = "ja"
    toBS AnalysisSchemeLanguageKo = "ko"
    toBS AnalysisSchemeLanguageLv = "lv"
    toBS AnalysisSchemeLanguageMul = "mul"
    toBS AnalysisSchemeLanguageNl = "nl"
    toBS AnalysisSchemeLanguageNo = "no"
    toBS AnalysisSchemeLanguagePt = "pt"
    toBS AnalysisSchemeLanguageRo = "ro"
    toBS AnalysisSchemeLanguageRu = "ru"
    toBS AnalysisSchemeLanguageSv = "sv"
    toBS AnalysisSchemeLanguageTh = "th"
    toBS AnalysisSchemeLanguageTr = "tr"
    toBS AnalysisSchemeLanguageZhHans = "zh-Hans"
    toBS AnalysisSchemeLanguageZhHant = "zh-Hant"

instance FromXML AnalysisSchemeLanguage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisSchemeLanguage"

instance ToQuery AnalysisSchemeLanguage where
    toQuery = genericToQuery def

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

instance ToByteString IndexFieldType where
    toBS IndexFieldTypeDate = "date"
    toBS IndexFieldTypeDateArray = "date-array"
    toBS IndexFieldTypeDouble = "double"
    toBS IndexFieldTypeDoubleArray = "double-array"
    toBS IndexFieldTypeInt = "int"
    toBS IndexFieldTypeIntArray = "int-array"
    toBS IndexFieldTypeLatlon = "latlon"
    toBS IndexFieldTypeLiteral = "literal"
    toBS IndexFieldTypeLiteralArray = "literal-array"
    toBS IndexFieldTypeText = "text"
    toBS IndexFieldTypeTextArray = "text-array"

instance FromXML IndexFieldType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexFieldType"

instance ToQuery IndexFieldType where
    toQuery = genericToQuery def

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

instance ToByteString OptionState where
    toBS OptionStateActive = "Active"
    toBS OptionStateFailedToValidate = "FailedToValidate"
    toBS OptionStateProcessing = "Processing"
    toBS OptionStateRequiresIndexDocuments = "RequiresIndexDocuments"

instance FromXML OptionState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionState"

instance ToQuery OptionState where
    toQuery = genericToQuery def

-- | The instance type that you want to preconfigure for your domain. For
-- example, search.m1.small.
data PartitionInstanceType
    = PartitionInstanceTypeSearchM1Large -- ^ search.m1.large
    | PartitionInstanceTypeSearchM1Small -- ^ search.m1.small
    | PartitionInstanceTypeSearchM22Xlarge -- ^ search.m2.2xlarge
    | PartitionInstanceTypeSearchM2Xlarge -- ^ search.m2.xlarge
      deriving (Eq, Show, Generic)

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

instance ToByteString PartitionInstanceType where
    toBS PartitionInstanceTypeSearchM1Large = "search.m1.large"
    toBS PartitionInstanceTypeSearchM1Small = "search.m1.small"
    toBS PartitionInstanceTypeSearchM22Xlarge = "search.m2.2xlarge"
    toBS PartitionInstanceTypeSearchM2Xlarge = "search.m2.xlarge"

instance FromXML PartitionInstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PartitionInstanceType"

instance ToQuery PartitionInstanceType where
    toQuery = genericToQuery def

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

instance FromText SuggesterFuzzyMatching where
    parser = match "high" SuggesterFuzzyMatchingHigh
         <|> match "low" SuggesterFuzzyMatchingLow
         <|> match "none" SuggesterFuzzyMatchingNone

instance ToText SuggesterFuzzyMatching where
    toText SuggesterFuzzyMatchingHigh = "high"
    toText SuggesterFuzzyMatchingLow = "low"
    toText SuggesterFuzzyMatchingNone = "none"

instance ToByteString SuggesterFuzzyMatching where
    toBS SuggesterFuzzyMatchingHigh = "high"
    toBS SuggesterFuzzyMatchingLow = "low"
    toBS SuggesterFuzzyMatchingNone = "none"

instance FromXML SuggesterFuzzyMatching where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuggesterFuzzyMatching"

instance ToQuery SuggesterFuzzyMatching where
    toQuery = genericToQuery def

-- | The service endpoint for updating documents in a search domain.
newtype ServiceEndpoint = ServiceEndpoint
    { _seEndpoint :: Maybe Text
      -- ^ The endpoint to which service requests can be submitted. For
      -- example,
      -- search-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.cloudsearch.amazonaws.com
      -- or
      -- doc-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.cloudsearch.amazonaws.com.
      -- 
    } deriving (Generic)

instance FromXML ServiceEndpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServiceEndpoint"

instance ToQuery ServiceEndpoint where
    toQuery = genericToQuery def

-- | The access rules configured for the domain specified in the request.
data AccessPoliciesStatus = AccessPoliciesStatus
    { _apsStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    , _apsOptions :: Text
      -- ^ Access rules for a domain's document or search service endpoints.
      -- For more information, see Configuring Access for a Search Domain
      -- in the Amazon CloudSearch Developer Guide. The maximum size of a
      -- policy document is 100 KB.
    } deriving (Generic)

instance FromXML AccessPoliciesStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessPoliciesStatus"

-- | Synonyms, stopwords, and stemming options for an analysis scheme.
data AnalysisOptions = AnalysisOptions
    { _aoAlgorithmicStemming :: Maybe AlgorithmicStemming
      -- ^ The level of algorithmic stemming to perform: none, minimal,
      -- light, or full. The available levels vary depending on the
      -- language. For more information, see Language Specific Text
      -- Processing Settings in the Amazon CloudSearch Developer Guide.
    , _aoStopwords :: Maybe Text
      -- ^ A JSON array of terms to ignore during indexing and searching.
      -- For example, ["a", "an", "the", "of"]. The stopwords dictionary
      -- must explicitly list each word you want to ignore. Wildcards and
      -- regular expressions are not supported.
    , _aoSynonyms :: Maybe Text
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
    , _aoStemmingDictionary :: Maybe Text
      -- ^ A JSON object that contains a collection of string:value pairs
      -- that each map a term to its stem. For example, {"term1": "stem1",
      -- "term2": "stem2", "term3": "stem3"}. The stemming dictionary is
      -- applied in addition to any algorithmic stemming. This enables you
      -- to override the results of the algorithmic stemming to correct
      -- specific cases of overstemming or understemming. The maximum size
      -- of a stemming dictionary is 500 KB.
    } deriving (Generic)

instance FromXML AnalysisOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisOptions"

instance ToQuery AnalysisOptions where
    toQuery = genericToQuery def

-- | Configuration information for an analysis scheme. Each analysis scheme has
-- a unique name and specifies the language of the text to be processed. The
-- following options can be configured for an analysis scheme: Synonyms,
-- Stopwords, StemmingDictionary, and AlgorithmicStemming.
data AnalysisScheme = AnalysisScheme
    { _asAnalysisOptions :: Maybe AnalysisOptions
      -- ^ Synonyms, stopwords, and stemming options for an analysis scheme.
    , _asAnalysisSchemeLanguage :: AnalysisSchemeLanguage
      -- ^ An IETF RFC 4646 language code or mul for multiple languages.
    , _asAnalysisSchemeName :: Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    } deriving (Generic)

instance FromXML AnalysisScheme where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisScheme"

instance ToQuery AnalysisScheme where
    toQuery = genericToQuery def

-- | The status of the analysis scheme being deleted.
data AnalysisSchemeStatus = AnalysisSchemeStatus
    { _assStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    , _assOptions :: AnalysisScheme
      -- ^ Configuration information for an analysis scheme. Each analysis
      -- scheme has a unique name and specifies the language of the text
      -- to be processed. The following options can be configured for an
      -- analysis scheme: Synonyms, Stopwords, StemmingDictionary, and
      -- AlgorithmicStemming.
    } deriving (Generic)

instance FromXML AnalysisSchemeStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisSchemeStatus"

-- | The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
data AvailabilityOptionsStatus = AvailabilityOptionsStatus
    { _aosStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    , _aosOptions :: Bool
      -- ^ The availability options configured for the domain.
    } deriving (Generic)

instance FromXML AvailabilityOptionsStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityOptionsStatus"

-- | Options for a field that contains an array of dates. Present if
-- IndexFieldType specifies the field is of type date-array. All options are
-- enabled by default.
data DateArrayOptions = DateArrayOptions
    { _dapSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _dapReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _dapFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _dapSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _dapDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML DateArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DateArrayOptions"

instance ToQuery DateArrayOptions where
    toQuery = genericToQuery def

-- | Options for a date field. Dates and times are specified in UTC (Coordinated
-- Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if
-- IndexFieldType specifies the field is of type date. All options are enabled
-- by default.
data DateOptions = DateOptions
    { _dduSourceField :: Maybe Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _dduReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _dduFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _dduSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _dduSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    , _dduDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML DateOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DateOptions"

instance ToQuery DateOptions where
    toQuery = genericToQuery def

-- | Options for a search suggester.
data DocumentSuggesterOptions = DocumentSuggesterOptions
    { _dsoSourceField :: Text
      -- ^ The name of the index field you want to use for suggestions.
    , _dsoSortExpression :: Maybe Text
      -- ^ An expression that computes a score for each suggestion to
      -- control how they are sorted.
    , _dsoFuzzyMatching :: Maybe SuggesterFuzzyMatching
      -- ^ The level of fuzziness allowed when suggesting matches for a
      -- string: none, low, or high. With none, the specified string is
      -- treated as an exact prefix. With low, suggestions must differ
      -- from the specified string by no more than one character. With
      -- high, suggestions can differ by up to two characters. The default
      -- is none.
    } deriving (Generic)

instance FromXML DocumentSuggesterOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DocumentSuggesterOptions"

instance ToQuery DocumentSuggesterOptions where
    toQuery = genericToQuery def

-- | The current status of the search domain.
data DomainStatus = DomainStatus
    { _dySearchInstanceCount :: Maybe Integer
      -- ^ The number of search instances that are available to process
      -- search requests.
    , _dySearchInstanceType :: Maybe Text
      -- ^ The instance type that is being used to process search requests.
    , _dyDocService :: Maybe ServiceEndpoint
      -- ^ The service endpoint for updating documents in a search domain.
    , _dyARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the search domain. See
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management for more information.
    , _dyCreated :: Maybe Bool
      -- ^ True if the search domain is created. It can take several minutes
      -- to initialize a domain when CreateDomain is called. Newly created
      -- search domains are returned from DescribeDomains with a false
      -- value for Created until domain creation is complete.
    , _dySearchService :: Maybe ServiceEndpoint
      -- ^ The service endpoint for requesting search results from a search
      -- domain.
    , _dyRequiresIndexDocuments :: Bool
      -- ^ True if IndexDocuments needs to be called to activate the current
      -- domain configuration.
    , _dyDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _dySearchPartitionCount :: Maybe Integer
      -- ^ The number of partitions across which the search index is spread.
    , _dyDeleted :: Maybe Bool
      -- ^ True if the search domain has been deleted. The system must clean
      -- up resources dedicated to the search domain when DeleteDomain is
      -- called. Newly deleted search domains are returned from
      -- DescribeDomains with a true value for IsDeleted for several
      -- minutes until resource cleanup is complete.
    , _dyDomainId :: Text
      -- ^ An internally generated unique identifier for a domain.
    , _dyProcessing :: Maybe Bool
      -- ^ True if processing is being done to activate the current domain
      -- configuration.
    } deriving (Generic)

instance FromXML DomainStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DomainStatus"

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if IndexFieldType specifies the field is of
-- type double-array. All options are enabled by default.
data DoubleArrayOptions = DoubleArrayOptions
    { _daoSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _daoReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _daoFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _daoSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _daoDefaultValue :: Maybe Double
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML DoubleArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DoubleArrayOptions"

instance ToQuery DoubleArrayOptions where
    toQuery = genericToQuery def

-- | Options for a double-precision 64-bit floating point field. Present if
-- IndexFieldType specifies the field is of type double. All options are
-- enabled by default.
data DoubleOptions = DoubleOptions
    { _ddvSourceField :: Maybe Text
      -- ^ The name of the source field to map to the field.
    , _ddvReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _ddvFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _ddvSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _ddvSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    , _ddvDefaultValue :: Maybe Double
      -- ^ A value to use for the field if the field isn't specified for a
      -- document. This can be important if you are using the field in an
      -- expression and that field is not present in every document.
    } deriving (Generic)

instance FromXML DoubleOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DoubleOptions"

instance ToQuery DoubleOptions where
    toQuery = genericToQuery def

-- | The expression that is evaluated for sorting or filtering while processing
-- a search request.
data Expression = Expression
    { _fExpressionName :: Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _fExpressionValue :: Text
      -- ^ The expression to evaluate for sorting while processing a search
      -- request. The Expression syntax is based on JavaScript
      -- expressions. For more information, see Configuring Expressions in
      -- the Amazon CloudSearch Developer Guide.
    } deriving (Generic)

instance FromXML Expression where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Expression"

instance ToQuery Expression where
    toQuery = genericToQuery def

-- | The value of an Expression and its current status.
data ExpressionStatus = ExpressionStatus
    { _esStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    , _esOptions :: Expression
      -- ^ The expression that is evaluated for sorting or filtering while
      -- processing a search request.
    } deriving (Generic)

instance FromXML ExpressionStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExpressionStatus"

-- | Configuration information for a field in the index, including its name,
-- type, and options. The supported options depend on the IndexFieldType.
data IndexField = IndexField
    { _ifDoubleArrayOptions :: Maybe DoubleArrayOptions
      -- ^ Options for a field that contains an array of double-precision
      -- 64-bit floating point values. Present if IndexFieldType specifies
      -- the field is of type double-array. All options are enabled by
      -- default.
    , _ifDateOptions :: Maybe DateOptions
      -- ^ Options for a date field. Dates and times are specified in UTC
      -- (Coordinated Universal Time) according to IETF RFC3339:
      -- yyyy-mm-ddT00:00:00Z. Present if IndexFieldType specifies the
      -- field is of type date. All options are enabled by default.
    , _ifTextArrayOptions :: Maybe TextArrayOptions
      -- ^ Options for a field that contains an array of text strings.
      -- Present if IndexFieldType specifies the field is of type
      -- text-array. A text-array field is always searchable. All options
      -- are enabled by default.
    , _ifDoubleOptions :: Maybe DoubleOptions
      -- ^ Options for a double-precision 64-bit floating point field.
      -- Present if IndexFieldType specifies the field is of type double.
      -- All options are enabled by default.
    , _ifTextOptions :: Maybe TextOptions
      -- ^ Options for text field. Present if IndexFieldType specifies the
      -- field is of type text. A text field is always searchable. All
      -- options are enabled by default.
    , _ifIndexFieldType :: IndexFieldType
      -- ^ The type of field. The valid options for a field depend on the
      -- field type. For more information about the supported field types,
      -- see Configuring Index Fields in the Amazon CloudSearch Developer
      -- Guide.
    , _ifLatLonOptions :: Maybe LatLonOptions
      -- ^ Options for a latlon field. A latlon field contains a location
      -- stored as a latitude and longitude value pair. Present if
      -- IndexFieldType specifies the field is of type latlon. All options
      -- are enabled by default.
    , _ifLiteralArrayOptions :: Maybe LiteralArrayOptions
      -- ^ Options for a field that contains an array of literal strings.
      -- Present if IndexFieldType specifies the field is of type
      -- literal-array. All options are enabled by default.
    , _ifIntArrayOptions :: Maybe IntArrayOptions
      -- ^ Options for a field that contains an array of 64-bit signed
      -- integers. Present if IndexFieldType specifies the field is of
      -- type int-array. All options are enabled by default.
    , _ifDateArrayOptions :: Maybe DateArrayOptions
      -- ^ Options for a field that contains an array of dates. Present if
      -- IndexFieldType specifies the field is of type date-array. All
      -- options are enabled by default.
    , _ifIntOptions :: Maybe IntOptions
      -- ^ Options for a 64-bit signed integer field. Present if
      -- IndexFieldType specifies the field is of type int. All options
      -- are enabled by default.
    , _ifLiteralOptions :: Maybe LiteralOptions
      -- ^ Options for literal field. Present if IndexFieldType specifies
      -- the field is of type literal. All options are enabled by default.
    , _ifIndexFieldName :: Text
      -- ^ The name of a field in the search index. Field names must begin
      -- with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). Uppercase letters and
      -- hyphens are not allowed. The name "score" is reserved and cannot
      -- be specified as field or expression name.
    } deriving (Generic)

instance FromXML IndexField where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexField"

instance ToQuery IndexField where
    toQuery = genericToQuery def

-- | The value of an IndexField and its current status.
data IndexFieldStatus = IndexFieldStatus
    { _ifsStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    , _ifsOptions :: IndexField
      -- ^ Configuration information for a field in the index, including its
      -- name, type, and options. The supported options depend on the
      -- IndexFieldType.
    } deriving (Generic)

instance FromXML IndexFieldStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexFieldStatus"

-- | Options for a field that contains an array of 64-bit signed integers.
-- Present if IndexFieldType specifies the field is of type int-array. All
-- options are enabled by default.
data IntArrayOptions = IntArrayOptions
    { _iaoSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _iaoReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _iaoFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _iaoSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _iaoDefaultValue :: Maybe Integer
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML IntArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IntArrayOptions"

instance ToQuery IntArrayOptions where
    toQuery = genericToQuery def

-- | Options for a 64-bit signed integer field. Present if IndexFieldType
-- specifies the field is of type int. All options are enabled by default.
data IntOptions = IntOptions
    { _ioSourceField :: Maybe Text
      -- ^ The name of the source field to map to the field.
    , _ioReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _ioFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _ioSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _ioSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    , _ioDefaultValue :: Maybe Integer
      -- ^ A value to use for the field if the field isn't specified for a
      -- document. This can be important if you are using the field in an
      -- expression and that field is not present in every document.
    } deriving (Generic)

instance FromXML IntOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IntOptions"

instance ToQuery IntOptions where
    toQuery = genericToQuery def

-- | Options for a latlon field. A latlon field contains a location stored as a
-- latitude and longitude value pair. Present if IndexFieldType specifies the
-- field is of type latlon. All options are enabled by default.
data LatLonOptions = LatLonOptions
    { _lloSourceField :: Maybe Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _lloReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _lloFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _lloSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _lloSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    , _lloDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML LatLonOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LatLonOptions"

instance ToQuery LatLonOptions where
    toQuery = genericToQuery def

-- | Options for a field that contains an array of literal strings. Present if
-- IndexFieldType specifies the field is of type literal-array. All options
-- are enabled by default.
data LiteralArrayOptions = LiteralArrayOptions
    { _laoSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _laoReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _laoFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _laoSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _laoDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML LiteralArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LiteralArrayOptions"

instance ToQuery LiteralArrayOptions where
    toQuery = genericToQuery def

-- | Options for literal field. Present if IndexFieldType specifies the field is
-- of type literal. All options are enabled by default.
data LiteralOptions = LiteralOptions
    { _loSourceField :: Maybe Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _loReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _loFacetEnabled :: Maybe Bool
      -- ^ Whether facet information can be returned for the field.
    , _loSearchEnabled :: Maybe Bool
      -- ^ Whether the contents of the field are searchable.
    , _loSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    , _loDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML LiteralOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LiteralOptions"

instance ToQuery LiteralOptions where
    toQuery = genericToQuery def

-- | The status of domain configuration option.
data OptionStatus = OptionStatus
    { _osState :: OptionState
      -- ^ The state of processing a change to an option. Possible values:
      -- RequiresIndexDocuments: the option's latest value will not be
      -- deployed until IndexDocuments has been called and indexing is
      -- complete. Processing: the option's latest value is in the process
      -- of being activated. Active: the option's latest value is
      -- completely deployed. FailedToValidate: the option value is not
      -- compatible with the domain's data and cannot be used to index the
      -- data. You must either modify the option value or update or remove
      -- the incompatible documents.
    , _osUpdateDate :: ISO8601
      -- ^ A timestamp for when this option was last updated.
    , _osPendingDeletion :: Maybe Bool
      -- ^ Indicates that the option will be deleted once processing is
      -- complete.
    , _osCreationDate :: ISO8601
      -- ^ A timestamp for when this option was created.
    , _osUpdateVersion :: Maybe Integer
      -- ^ A unique integer that indicates when this option was last
      -- updated.
    } deriving (Generic)

instance FromXML OptionStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionStatus"

instance ToQuery OptionStatus where
    toQuery = genericToQuery def

-- | The desired instance type and desired number of replicas of each index
-- partition.
data ScalingParameters = ScalingParameters
    { _spDesiredInstanceType :: Maybe PartitionInstanceType
      -- ^ The instance type that you want to preconfigure for your domain.
      -- For example, search.m1.small.
    , _spDesiredReplicationCount :: Maybe Integer
      -- ^ The number of replicas you want to preconfigure for each index
      -- partition.
    , _spDesiredPartitionCount :: Maybe Integer
      -- ^ The number of partitions you want to preconfigure for your
      -- domain. Only valid when you select m2.2xlarge as the desired
      -- instance type.
    } deriving (Generic)

instance FromXML ScalingParameters where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingParameters"

instance ToQuery ScalingParameters where
    toQuery = genericToQuery def

-- | The status and configuration of a search domain's scaling parameters.
data ScalingParametersStatus = ScalingParametersStatus
    { _spsStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    , _spsOptions :: ScalingParameters
      -- ^ The desired instance type and desired number of replicas of each
      -- index partition.
    } deriving (Generic)

instance FromXML ScalingParametersStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingParametersStatus"

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for suggestions.
-- The following options can be configured for a suggester: FuzzyMatching,
-- SortExpression.
data Suggester = Suggester
    { _sDocumentSuggesterOptions :: DocumentSuggesterOptions
      -- ^ Options for a search suggester.
    , _sSuggesterName :: Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    } deriving (Generic)

instance FromXML Suggester where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Suggester"

instance ToQuery Suggester where
    toQuery = genericToQuery def

-- | The value of a Suggester and its current status.
data SuggesterStatus = SuggesterStatus
    { _ssStatus :: OptionStatus
      -- ^ The status of domain configuration option.
    , _ssOptions :: Suggester
      -- ^ Configuration information for a search suggester. Each suggester
      -- has a unique name and specifies the text field you want to use
      -- for suggestions. The following options can be configured for a
      -- suggester: FuzzyMatching, SortExpression.
    } deriving (Generic)

instance FromXML SuggesterStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuggesterStatus"

-- | Options for a field that contains an array of text strings. Present if
-- IndexFieldType specifies the field is of type text-array. A text-array
-- field is always searchable. All options are enabled by default.
data TextArrayOptions = TextArrayOptions
    { _taoSourceFields :: Maybe Text
      -- ^ A list of source fields to map to the field.
    , _taoReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _taoAnalysisScheme :: Maybe Text
      -- ^ The name of an analysis scheme for a text-array field.
    , _taoHighlightEnabled :: Maybe Bool
      -- ^ Whether highlights can be returned for the field.
    , _taoDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML TextArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TextArrayOptions"

instance ToQuery TextArrayOptions where
    toQuery = genericToQuery def

-- | Options for text field. Present if IndexFieldType specifies the field is of
-- type text. A text field is always searchable. All options are enabled by
-- default.
data TextOptions = TextOptions
    { _toSourceField :: Maybe Text
      -- ^ A string that represents the name of an index field. Field names
      -- begin with a letter and can contain the following characters: a-z
      -- (lowercase), 0-9, and _ (underscore). The name "score" is
      -- reserved and cannot be used as a field name. To reference a
      -- document's ID, you can use the name _id.
    , _toReturnEnabled :: Maybe Bool
      -- ^ Whether the contents of the field can be returned in the search
      -- results.
    , _toAnalysisScheme :: Maybe Text
      -- ^ The name of an analysis scheme for a text field.
    , _toHighlightEnabled :: Maybe Bool
      -- ^ Whether highlights can be returned for the field.
    , _toSortEnabled :: Maybe Bool
      -- ^ Whether the field can be used to sort the search results.
    , _toDefaultValue :: Maybe Text
      -- ^ A value to use for the field if the field isn't specified for a
      -- document.
    } deriving (Generic)

instance FromXML TextOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TextOptions"

instance ToQuery TextOptions where
    toQuery = genericToQuery def
