{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.Types
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
module Network.AWS.CloudSearch.Types
    (
    -- * Service
      CloudSearch
    -- ** Errors
    , CloudSearchError (..)
    , _BaseException
    , _CloudSearchClient
    , _CloudSearchSerializer
    , _CloudSearchService
    , _DisabledOperationException
    , _InternalException
    , _InvalidTypeException
    , _LimitExceededException
    , _ResourceNotFoundException
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
    , ServiceEndpoint
    , serviceEndpoint
    , seEndpoint

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- * AnalysisOptions
    , AnalysisOptions
    , analysisOptions
    , aoSynonyms
    , aoStopwords
    , aoStemmingDictionary
    , aoAlgorithmicStemming

    -- * AnalysisScheme
    , AnalysisScheme
    , analysisScheme
    , asAnalysisSchemeName
    , asAnalysisSchemeLanguage
    , asAnalysisOptions

    -- * AnalysisSchemeStatus
    , AnalysisSchemeStatus
    , analysisSchemeStatus
    , assOptions
    , assStatus

    -- * AvailabilityOptionsStatus
    , AvailabilityOptionsStatus
    , availabilityOptionsStatus
    , aosOptions
    , aosStatus

    -- * DateArrayOptions
    , DateArrayOptions
    , dateArrayOptions
    , dao1DefaultValue
    , dao1SourceFields
    , dao1FacetEnabled
    , dao1SearchEnabled
    , dao1ReturnEnabled

    -- * DateOptions
    , DateOptions
    , dateOptions
    , do1DefaultValue
    , do1SourceField
    , do1FacetEnabled
    , do1SearchEnabled
    , do1ReturnEnabled
    , do1SortEnabled

    -- * DocumentSuggesterOptions
    , DocumentSuggesterOptions
    , documentSuggesterOptions
    , dsoSourceField
    , dsoFuzzyMatching
    , dsoSortExpression

    -- * DomainStatus
    , DomainStatus
    , domainStatus
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
    , DoubleArrayOptions
    , doubleArrayOptions
    , daoDefaultValue
    , daoSourceFields
    , daoFacetEnabled
    , daoSearchEnabled
    , daoReturnEnabled

    -- * DoubleOptions
    , DoubleOptions
    , doubleOptions
    , doDefaultValue
    , doSourceField
    , doFacetEnabled
    , doSearchEnabled
    , doReturnEnabled
    , doSortEnabled

    -- * Expression
    , Expression
    , expression
    , eExpressionName
    , eExpressionValue

    -- * ExpressionStatus
    , ExpressionStatus
    , expressionStatus
    , esOptions
    , esStatus

    -- * IndexField
    , IndexField
    , indexField
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
    , IndexFieldStatus
    , indexFieldStatus
    , ifsOptions
    , ifsStatus

    -- * IntArrayOptions
    , IntArrayOptions
    , intArrayOptions
    , iaoDefaultValue
    , iaoSourceFields
    , iaoFacetEnabled
    , iaoSearchEnabled
    , iaoReturnEnabled

    -- * IntOptions
    , IntOptions
    , intOptions
    , ioDefaultValue
    , ioSourceField
    , ioFacetEnabled
    , ioSearchEnabled
    , ioReturnEnabled
    , ioSortEnabled

    -- * LatLonOptions
    , LatLonOptions
    , latLonOptions
    , lloDefaultValue
    , lloSourceField
    , lloFacetEnabled
    , lloSearchEnabled
    , lloReturnEnabled
    , lloSortEnabled

    -- * LiteralArrayOptions
    , LiteralArrayOptions
    , literalArrayOptions
    , laoDefaultValue
    , laoSourceFields
    , laoFacetEnabled
    , laoSearchEnabled
    , laoReturnEnabled

    -- * LiteralOptions
    , LiteralOptions
    , literalOptions
    , loDefaultValue
    , loSourceField
    , loFacetEnabled
    , loSearchEnabled
    , loReturnEnabled
    , loSortEnabled

    -- * OptionStatus
    , OptionStatus
    , optionStatus
    , osCreationDate
    , osUpdateDate
    , osUpdateVersion
    , osState
    , osPendingDeletion

    -- * ScalingParameters
    , ScalingParameters
    , scalingParameters
    , spDesiredInstanceType
    , spDesiredReplicationCount
    , spDesiredPartitionCount

    -- * ScalingParametersStatus
    , ScalingParametersStatus
    , scalingParametersStatus
    , spsOptions
    , spsStatus

    -- * Suggester
    , Suggester
    , suggester
    , sSuggesterName
    , sDocumentSuggesterOptions

    -- * SuggesterStatus
    , SuggesterStatus
    , suggesterStatus
    , ssOptions
    , ssStatus

    -- * TextArrayOptions
    , TextArrayOptions
    , textArrayOptions
    , taoDefaultValue
    , taoSourceFields
    , taoReturnEnabled
    , taoHighlightEnabled
    , taoAnalysisScheme

    -- * TextOptions
    , TextOptions
    , textOptions
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
    type Er CloudSearch = CloudSearchError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudsearch"
        , _svcVersion  = "2013-01-01"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'CloudSearch' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data CloudSearchError
      -- | An error occurred while processing the request.
    = BaseException
        { _beCode :: Maybe Text
        , _beMessage :: Maybe Text
        }
    | CloudSearchClient HttpException
    | CloudSearchSerializer String
    | CloudSearchService String
      -- | The request was rejected because it attempted an operation which
      -- is not enabled.
    | DisabledOperationException
      -- | An internal error occurred while processing the request. If this
      -- problem persists, report an issue from the Service Health
      -- Dashboard.
    | InternalException
      -- | The request was rejected because it specified an invalid type
      -- definition.
    | InvalidTypeException
      -- | The request was rejected because a resource limit has already
      -- been met.
    | LimitExceededException
      -- | The request was rejected because it attempted to reference a
      -- resource that does not exist.
    | ResourceNotFoundException
      deriving (Show, Typeable, Generic)

instance AWSError CloudSearchError where
    awsError = const "CloudSearchError"

instance AWSServiceError CloudSearchError where
    serviceError    = CloudSearchService
    clientError     = CloudSearchClient
    serializerError = CloudSearchSerializer

instance Exception CloudSearchError

-- | An error occurred while processing the request.
--
-- See: 'BaseException'
_BaseException :: Prism' CloudSearchError (Maybe Text, Maybe Text)
_BaseException = prism
    (\(p1, p2) -> BaseException p1 p2)
    (\case
        BaseException p1 p2 -> Right (p1, p2)
        x -> Left x)

-- | See: 'CloudSearchClient'
_CloudSearchClient :: Prism' CloudSearchError HttpException
_CloudSearchClient = prism
    CloudSearchClient
    (\case
        CloudSearchClient p1 -> Right p1
        x -> Left x)

-- | See: 'CloudSearchSerializer'
_CloudSearchSerializer :: Prism' CloudSearchError String
_CloudSearchSerializer = prism
    CloudSearchSerializer
    (\case
        CloudSearchSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'CloudSearchService'
_CloudSearchService :: Prism' CloudSearchError String
_CloudSearchService = prism
    CloudSearchService
    (\case
        CloudSearchService p1 -> Right p1
        x -> Left x)

-- | The request was rejected because it attempted an operation which is not
-- enabled.
--
-- See: 'DisabledOperationException'
_DisabledOperationException :: Prism' CloudSearchError ()
_DisabledOperationException = prism
    (const DisabledOperationException)
    (\case
        DisabledOperationException -> Right ()
        x -> Left x)

-- | An internal error occurred while processing the request. If this problem
-- persists, report an issue from the Service Health Dashboard.
--
-- See: 'InternalException'
_InternalException :: Prism' CloudSearchError ()
_InternalException = prism
    (const InternalException)
    (\case
        InternalException -> Right ()
        x -> Left x)

-- | The request was rejected because it specified an invalid type definition.
--
-- See: 'InvalidTypeException'
_InvalidTypeException :: Prism' CloudSearchError ()
_InvalidTypeException = prism
    (const InvalidTypeException)
    (\case
        InvalidTypeException -> Right ()
        x -> Left x)

-- | The request was rejected because a resource limit has already been met.
--
-- See: 'LimitExceededException'
_LimitExceededException :: Prism' CloudSearchError ()
_LimitExceededException = prism
    (const LimitExceededException)
    (\case
        LimitExceededException -> Right ()
        x -> Left x)

-- | The request was rejected because it attempted to reference a resource that
-- does not exist.
--
-- See: 'ResourceNotFoundException'
_ResourceNotFoundException :: Prism' CloudSearchError ()
_ResourceNotFoundException = prism
    (const ResourceNotFoundException)
    (\case
        ResourceNotFoundException -> Right ()
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ServiceEndpoint' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Endpoint ::@ @Maybe Text@
--
serviceEndpoint :: ServiceEndpoint
serviceEndpoint = ServiceEndpoint
    { _seEndpoint = Nothing
    }

-- | The endpoint to which service requests can be submitted. For example,
-- search-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.cloudsearch.amazonaws.com
-- or
-- doc-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.cloudsearch.amazonaws.com.
-- 
seEndpoint :: Lens' ServiceEndpoint (Maybe Text)
seEndpoint = lens _seEndpoint (\s a -> s { _seEndpoint = a })

instance FromXML ServiceEndpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServiceEndpoint"

instance ToQuery ServiceEndpoint where
    toQuery = genericQuery def

-- | The access rules configured for the domain specified in the request.
data AccessPoliciesStatus = AccessPoliciesStatus
    { _apsOptions :: Text
    , _apsStatus :: OptionStatus
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccessPoliciesStatus' data type.
--
-- 'AccessPoliciesStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Options ::@ @Text@
--
-- * @Status ::@ @OptionStatus@
--
accessPoliciesStatus :: Text -- ^ 'apsOptions'
                     -> OptionStatus -- ^ 'apsStatus'
                     -> AccessPoliciesStatus
accessPoliciesStatus p1 p2 = AccessPoliciesStatus
    { _apsOptions = p1
    , _apsStatus = p2
    }

-- | Access rules for a domain's document or search service endpoints. For more
-- information, see Configuring Access for a Search Domain in the Amazon
-- CloudSearch Developer Guide. The maximum size of a policy document is 100
-- KB.
apsOptions :: Lens' AccessPoliciesStatus Text
apsOptions = lens _apsOptions (\s a -> s { _apsOptions = a })

-- | The status of domain configuration option.
apsStatus :: Lens' AccessPoliciesStatus OptionStatus
apsStatus = lens _apsStatus (\s a -> s { _apsStatus = a })

instance FromXML AccessPoliciesStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessPoliciesStatus"

-- | Synonyms, stopwords, and stemming options for an analysis scheme.
data AnalysisOptions = AnalysisOptions
    { _aoSynonyms :: Maybe Text
    , _aoStopwords :: Maybe Text
    , _aoStemmingDictionary :: Maybe Text
    , _aoAlgorithmicStemming :: Maybe AlgorithmicStemming
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AnalysisOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Synonyms ::@ @Maybe Text@
--
-- * @Stopwords ::@ @Maybe Text@
--
-- * @StemmingDictionary ::@ @Maybe Text@
--
-- * @AlgorithmicStemming ::@ @Maybe AlgorithmicStemming@
--
analysisOptions :: AnalysisOptions
analysisOptions = AnalysisOptions
    { _aoSynonyms = Nothing
    , _aoStopwords = Nothing
    , _aoStemmingDictionary = Nothing
    , _aoAlgorithmicStemming = Nothing
    }

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
aoSynonyms = lens _aoSynonyms (\s a -> s { _aoSynonyms = a })

-- | A JSON array of terms to ignore during indexing and searching. For example,
-- ["a", "an", "the", "of"]. The stopwords dictionary must explicitly list
-- each word you want to ignore. Wildcards and regular expressions are not
-- supported.
aoStopwords :: Lens' AnalysisOptions (Maybe Text)
aoStopwords = lens _aoStopwords (\s a -> s { _aoStopwords = a })

-- | A JSON object that contains a collection of string:value pairs that each
-- map a term to its stem. For example, {"term1": "stem1", "term2": "stem2",
-- "term3": "stem3"}. The stemming dictionary is applied in addition to any
-- algorithmic stemming. This enables you to override the results of the
-- algorithmic stemming to correct specific cases of overstemming or
-- understemming. The maximum size of a stemming dictionary is 500 KB.
aoStemmingDictionary :: Lens' AnalysisOptions (Maybe Text)
aoStemmingDictionary =
    lens _aoStemmingDictionary (\s a -> s { _aoStemmingDictionary = a })

-- | The level of algorithmic stemming to perform: none, minimal, light, or
-- full. The available levels vary depending on the language. For more
-- information, see Language Specific Text Processing Settings in the Amazon
-- CloudSearch Developer Guide.
aoAlgorithmicStemming :: Lens' AnalysisOptions (Maybe AlgorithmicStemming)
aoAlgorithmicStemming =
    lens _aoAlgorithmicStemming (\s a -> s { _aoAlgorithmicStemming = a })

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
    , _asAnalysisSchemeLanguage :: AnalysisSchemeLanguage
    , _asAnalysisOptions :: Maybe AnalysisOptions
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AnalysisScheme' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AnalysisSchemeName ::@ @Text@
--
-- * @AnalysisSchemeLanguage ::@ @AnalysisSchemeLanguage@
--
-- * @AnalysisOptions ::@ @Maybe AnalysisOptions@
--
analysisScheme :: Text -- ^ 'asAnalysisSchemeName'
               -> AnalysisSchemeLanguage -- ^ 'asAnalysisSchemeLanguage'
               -> AnalysisScheme
analysisScheme p1 p2 = AnalysisScheme
    { _asAnalysisSchemeName = p1
    , _asAnalysisSchemeLanguage = p2
    , _asAnalysisOptions = Nothing
    }

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
asAnalysisSchemeName :: Lens' AnalysisScheme Text
asAnalysisSchemeName =
    lens _asAnalysisSchemeName (\s a -> s { _asAnalysisSchemeName = a })

-- | An IETF RFC 4646 language code or mul for multiple languages.
asAnalysisSchemeLanguage :: Lens' AnalysisScheme AnalysisSchemeLanguage
asAnalysisSchemeLanguage =
    lens _asAnalysisSchemeLanguage
         (\s a -> s { _asAnalysisSchemeLanguage = a })

-- | Synonyms, stopwords, and stemming options for an analysis scheme.
asAnalysisOptions :: Lens' AnalysisScheme (Maybe AnalysisOptions)
asAnalysisOptions =
    lens _asAnalysisOptions (\s a -> s { _asAnalysisOptions = a })

instance FromXML AnalysisScheme where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisScheme"

instance ToQuery AnalysisScheme where
    toQuery = genericQuery def

-- | The status and configuration of an AnalysisScheme.
data AnalysisSchemeStatus = AnalysisSchemeStatus
    { _assOptions :: AnalysisScheme
    , _assStatus :: OptionStatus
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AnalysisSchemeStatus' data type.
--
-- 'AnalysisSchemeStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Options ::@ @AnalysisScheme@
--
-- * @Status ::@ @OptionStatus@
--
analysisSchemeStatus :: AnalysisScheme -- ^ 'assOptions'
                     -> OptionStatus -- ^ 'assStatus'
                     -> AnalysisSchemeStatus
analysisSchemeStatus p1 p2 = AnalysisSchemeStatus
    { _assOptions = p1
    , _assStatus = p2
    }

-- | Configuration information for an analysis scheme. Each analysis scheme has
-- a unique name and specifies the language of the text to be processed. The
-- following options can be configured for an analysis scheme: Synonyms,
-- Stopwords, StemmingDictionary, and AlgorithmicStemming.
assOptions :: Lens' AnalysisSchemeStatus AnalysisScheme
assOptions = lens _assOptions (\s a -> s { _assOptions = a })

-- | The status of domain configuration option.
assStatus :: Lens' AnalysisSchemeStatus OptionStatus
assStatus = lens _assStatus (\s a -> s { _assStatus = a })

instance FromXML AnalysisSchemeStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisSchemeStatus"

-- | The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
data AvailabilityOptionsStatus = AvailabilityOptionsStatus
    { _aosOptions :: !Bool
    , _aosStatus :: OptionStatus
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityOptionsStatus' data type.
--
-- 'AvailabilityOptionsStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Options ::@ @Bool@
--
-- * @Status ::@ @OptionStatus@
--
availabilityOptionsStatus :: Bool -- ^ 'aosOptions'
                          -> OptionStatus -- ^ 'aosStatus'
                          -> AvailabilityOptionsStatus
availabilityOptionsStatus p1 p2 = AvailabilityOptionsStatus
    { _aosOptions = p1
    , _aosStatus = p2
    }

-- | The availability options configured for the domain.
aosOptions :: Lens' AvailabilityOptionsStatus Bool
aosOptions = lens _aosOptions (\s a -> s { _aosOptions = a })

-- | The status of domain configuration option.
aosStatus :: Lens' AvailabilityOptionsStatus OptionStatus
aosStatus = lens _aosStatus (\s a -> s { _aosStatus = a })

instance FromXML AvailabilityOptionsStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityOptionsStatus"

-- | Options for a field that contains an array of dates. Present if
-- IndexFieldType specifies the field is of type date-array. All options are
-- enabled by default.
data DateArrayOptions = DateArrayOptions
    { _dao1DefaultValue :: Maybe Text
    , _dao1SourceFields :: Maybe Text
    , _dao1FacetEnabled :: Maybe Bool
    , _dao1SearchEnabled :: Maybe Bool
    , _dao1ReturnEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DateArrayOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @SourceFields ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
dateArrayOptions :: DateArrayOptions
dateArrayOptions = DateArrayOptions
    { _dao1DefaultValue = Nothing
    , _dao1SourceFields = Nothing
    , _dao1FacetEnabled = Nothing
    , _dao1SearchEnabled = Nothing
    , _dao1ReturnEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
dao1DefaultValue :: Lens' DateArrayOptions (Maybe Text)
dao1DefaultValue =
    lens _dao1DefaultValue (\s a -> s { _dao1DefaultValue = a })

-- | A list of source fields to map to the field.
dao1SourceFields :: Lens' DateArrayOptions (Maybe Text)
dao1SourceFields =
    lens _dao1SourceFields (\s a -> s { _dao1SourceFields = a })

-- | Whether facet information can be returned for the field.
dao1FacetEnabled :: Lens' DateArrayOptions (Maybe Bool)
dao1FacetEnabled =
    lens _dao1FacetEnabled (\s a -> s { _dao1FacetEnabled = a })

-- | Whether the contents of the field are searchable.
dao1SearchEnabled :: Lens' DateArrayOptions (Maybe Bool)
dao1SearchEnabled =
    lens _dao1SearchEnabled (\s a -> s { _dao1SearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
dao1ReturnEnabled :: Lens' DateArrayOptions (Maybe Bool)
dao1ReturnEnabled =
    lens _dao1ReturnEnabled (\s a -> s { _dao1ReturnEnabled = a })

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
    { _do1DefaultValue :: Maybe Text
    , _do1SourceField :: Maybe Text
    , _do1FacetEnabled :: Maybe Bool
    , _do1SearchEnabled :: Maybe Bool
    , _do1ReturnEnabled :: Maybe Bool
    , _do1SortEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DateOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @SourceField ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
-- * @SortEnabled ::@ @Maybe Bool@
--
dateOptions :: DateOptions
dateOptions = DateOptions
    { _do1DefaultValue = Nothing
    , _do1SourceField = Nothing
    , _do1FacetEnabled = Nothing
    , _do1SearchEnabled = Nothing
    , _do1ReturnEnabled = Nothing
    , _do1SortEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
do1DefaultValue :: Lens' DateOptions (Maybe Text)
do1DefaultValue = lens _do1DefaultValue (\s a -> s { _do1DefaultValue = a })

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
do1SourceField :: Lens' DateOptions (Maybe Text)
do1SourceField = lens _do1SourceField (\s a -> s { _do1SourceField = a })

-- | Whether facet information can be returned for the field.
do1FacetEnabled :: Lens' DateOptions (Maybe Bool)
do1FacetEnabled = lens _do1FacetEnabled (\s a -> s { _do1FacetEnabled = a })

-- | Whether the contents of the field are searchable.
do1SearchEnabled :: Lens' DateOptions (Maybe Bool)
do1SearchEnabled =
    lens _do1SearchEnabled (\s a -> s { _do1SearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
do1ReturnEnabled :: Lens' DateOptions (Maybe Bool)
do1ReturnEnabled =
    lens _do1ReturnEnabled (\s a -> s { _do1ReturnEnabled = a })

-- | Whether the field can be used to sort the search results.
do1SortEnabled :: Lens' DateOptions (Maybe Bool)
do1SortEnabled = lens _do1SortEnabled (\s a -> s { _do1SortEnabled = a })

instance FromXML DateOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DateOptions"

instance ToQuery DateOptions where
    toQuery = genericQuery def

-- | Options for a search suggester.
data DocumentSuggesterOptions = DocumentSuggesterOptions
    { _dsoSourceField :: Text
    , _dsoFuzzyMatching :: Maybe SuggesterFuzzyMatching
    , _dsoSortExpression :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DocumentSuggesterOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceField ::@ @Text@
--
-- * @FuzzyMatching ::@ @Maybe SuggesterFuzzyMatching@
--
-- * @SortExpression ::@ @Maybe Text@
--
documentSuggesterOptions :: Text -- ^ 'dsoSourceField'
                         -> DocumentSuggesterOptions
documentSuggesterOptions p1 = DocumentSuggesterOptions
    { _dsoSourceField = p1
    , _dsoFuzzyMatching = Nothing
    , _dsoSortExpression = Nothing
    }

-- | The name of the index field you want to use for suggestions.
dsoSourceField :: Lens' DocumentSuggesterOptions Text
dsoSourceField = lens _dsoSourceField (\s a -> s { _dsoSourceField = a })

-- | The level of fuzziness allowed when suggesting matches for a string: none,
-- low, or high. With none, the specified string is treated as an exact
-- prefix. With low, suggestions must differ from the specified string by no
-- more than one character. With high, suggestions can differ by up to two
-- characters. The default is none.
dsoFuzzyMatching :: Lens' DocumentSuggesterOptions (Maybe SuggesterFuzzyMatching)
dsoFuzzyMatching =
    lens _dsoFuzzyMatching (\s a -> s { _dsoFuzzyMatching = a })

-- | An expression that computes a score for each suggestion to control how they
-- are sorted.
dsoSortExpression :: Lens' DocumentSuggesterOptions (Maybe Text)
dsoSortExpression =
    lens _dsoSortExpression (\s a -> s { _dsoSortExpression = a })

instance FromXML DocumentSuggesterOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DocumentSuggesterOptions"

instance ToQuery DocumentSuggesterOptions where
    toQuery = genericQuery def

-- | The current status of the search domain.
data DomainStatus = DomainStatus
    { _dsDomainId :: Text
    , _dsDomainName :: Text
    , _dsARN :: Maybe Text
    , _dsCreated :: Maybe Bool
    , _dsDeleted :: Maybe Bool
    , _dsDocService :: Maybe ServiceEndpoint
    , _dsSearchService :: Maybe ServiceEndpoint
    , _dsRequiresIndexDocuments :: !Bool
    , _dsProcessing :: Maybe Bool
    , _dsSearchInstanceType :: Maybe Text
    , _dsSearchPartitionCount :: Maybe Integer
    , _dsSearchInstanceCount :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DomainStatus' data type.
--
-- 'DomainStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainId ::@ @Text@
--
-- * @DomainName ::@ @Text@
--
-- * @ARN ::@ @Maybe Text@
--
-- * @Created ::@ @Maybe Bool@
--
-- * @Deleted ::@ @Maybe Bool@
--
-- * @DocService ::@ @Maybe ServiceEndpoint@
--
-- * @SearchService ::@ @Maybe ServiceEndpoint@
--
-- * @RequiresIndexDocuments ::@ @Bool@
--
-- * @Processing ::@ @Maybe Bool@
--
-- * @SearchInstanceType ::@ @Maybe Text@
--
-- * @SearchPartitionCount ::@ @Maybe Integer@
--
-- * @SearchInstanceCount ::@ @Maybe Integer@
--
domainStatus :: Text -- ^ 'dsDomainId'
             -> Text -- ^ 'dsDomainName'
             -> Bool -- ^ 'dsRequiresIndexDocuments'
             -> DomainStatus
domainStatus p1 p2 p8 = DomainStatus
    { _dsDomainId = p1
    , _dsDomainName = p2
    , _dsARN = Nothing
    , _dsCreated = Nothing
    , _dsDeleted = Nothing
    , _dsDocService = Nothing
    , _dsSearchService = Nothing
    , _dsRequiresIndexDocuments = p8
    , _dsProcessing = Nothing
    , _dsSearchInstanceType = Nothing
    , _dsSearchPartitionCount = Nothing
    , _dsSearchInstanceCount = Nothing
    }

-- | An internally generated unique identifier for a domain.
dsDomainId :: Lens' DomainStatus Text
dsDomainId = lens _dsDomainId (\s a -> s { _dsDomainId = a })

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dsDomainName :: Lens' DomainStatus Text
dsDomainName = lens _dsDomainName (\s a -> s { _dsDomainName = a })

-- | The Amazon Resource Name (ARN) of the search domain. See Identifiers for
-- IAM Entities in Using AWS Identity and Access Management for more
-- information.
dsARN :: Lens' DomainStatus (Maybe Text)
dsARN = lens _dsARN (\s a -> s { _dsARN = a })

-- | True if the search domain is created. It can take several minutes to
-- initialize a domain when CreateDomain is called. Newly created search
-- domains are returned from DescribeDomains with a false value for Created
-- until domain creation is complete.
dsCreated :: Lens' DomainStatus (Maybe Bool)
dsCreated = lens _dsCreated (\s a -> s { _dsCreated = a })

-- | True if the search domain has been deleted. The system must clean up
-- resources dedicated to the search domain when DeleteDomain is called. Newly
-- deleted search domains are returned from DescribeDomains with a true value
-- for IsDeleted for several minutes until resource cleanup is complete.
dsDeleted :: Lens' DomainStatus (Maybe Bool)
dsDeleted = lens _dsDeleted (\s a -> s { _dsDeleted = a })

-- | The service endpoint for updating documents in a search domain.
dsDocService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsDocService = lens _dsDocService (\s a -> s { _dsDocService = a })

-- | The service endpoint for requesting search results from a search domain.
dsSearchService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsSearchService = lens _dsSearchService (\s a -> s { _dsSearchService = a })

-- | True if IndexDocuments needs to be called to activate the current domain
-- configuration.
dsRequiresIndexDocuments :: Lens' DomainStatus Bool
dsRequiresIndexDocuments =
    lens _dsRequiresIndexDocuments
         (\s a -> s { _dsRequiresIndexDocuments = a })

-- | True if processing is being done to activate the current domain
-- configuration.
dsProcessing :: Lens' DomainStatus (Maybe Bool)
dsProcessing = lens _dsProcessing (\s a -> s { _dsProcessing = a })

-- | The instance type that is being used to process search requests.
dsSearchInstanceType :: Lens' DomainStatus (Maybe Text)
dsSearchInstanceType =
    lens _dsSearchInstanceType (\s a -> s { _dsSearchInstanceType = a })

-- | The number of partitions across which the search index is spread.
dsSearchPartitionCount :: Lens' DomainStatus (Maybe Integer)
dsSearchPartitionCount =
    lens _dsSearchPartitionCount (\s a -> s { _dsSearchPartitionCount = a })

-- | The number of search instances that are available to process search
-- requests.
dsSearchInstanceCount :: Lens' DomainStatus (Maybe Integer)
dsSearchInstanceCount =
    lens _dsSearchInstanceCount (\s a -> s { _dsSearchInstanceCount = a })

instance FromXML DomainStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DomainStatus"

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if IndexFieldType specifies the field is of
-- type double-array. All options are enabled by default.
data DoubleArrayOptions = DoubleArrayOptions
    { _daoDefaultValue :: Maybe Double
    , _daoSourceFields :: Maybe Text
    , _daoFacetEnabled :: Maybe Bool
    , _daoSearchEnabled :: Maybe Bool
    , _daoReturnEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DoubleArrayOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Double@
--
-- * @SourceFields ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
doubleArrayOptions :: DoubleArrayOptions
doubleArrayOptions = DoubleArrayOptions
    { _daoDefaultValue = Nothing
    , _daoSourceFields = Nothing
    , _daoFacetEnabled = Nothing
    , _daoSearchEnabled = Nothing
    , _daoReturnEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
daoDefaultValue :: Lens' DoubleArrayOptions (Maybe Double)
daoDefaultValue = lens _daoDefaultValue (\s a -> s { _daoDefaultValue = a })

-- | A list of source fields to map to the field.
daoSourceFields :: Lens' DoubleArrayOptions (Maybe Text)
daoSourceFields = lens _daoSourceFields (\s a -> s { _daoSourceFields = a })

-- | Whether facet information can be returned for the field.
daoFacetEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoFacetEnabled = lens _daoFacetEnabled (\s a -> s { _daoFacetEnabled = a })

-- | Whether the contents of the field are searchable.
daoSearchEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoSearchEnabled =
    lens _daoSearchEnabled (\s a -> s { _daoSearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
daoReturnEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoReturnEnabled =
    lens _daoReturnEnabled (\s a -> s { _daoReturnEnabled = a })

instance FromXML DoubleArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DoubleArrayOptions"

instance ToQuery DoubleArrayOptions where
    toQuery = genericQuery def

-- | Options for a double-precision 64-bit floating point field. Present if
-- IndexFieldType specifies the field is of type double. All options are
-- enabled by default.
data DoubleOptions = DoubleOptions
    { _doDefaultValue :: Maybe Double
    , _doSourceField :: Maybe Text
    , _doFacetEnabled :: Maybe Bool
    , _doSearchEnabled :: Maybe Bool
    , _doReturnEnabled :: Maybe Bool
    , _doSortEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DoubleOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Double@
--
-- * @SourceField ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
-- * @SortEnabled ::@ @Maybe Bool@
--
doubleOptions :: DoubleOptions
doubleOptions = DoubleOptions
    { _doDefaultValue = Nothing
    , _doSourceField = Nothing
    , _doFacetEnabled = Nothing
    , _doSearchEnabled = Nothing
    , _doReturnEnabled = Nothing
    , _doSortEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
-- This can be important if you are using the field in an expression and that
-- field is not present in every document.
doDefaultValue :: Lens' DoubleOptions (Maybe Double)
doDefaultValue = lens _doDefaultValue (\s a -> s { _doDefaultValue = a })

-- | The name of the source field to map to the field.
doSourceField :: Lens' DoubleOptions (Maybe Text)
doSourceField = lens _doSourceField (\s a -> s { _doSourceField = a })

-- | Whether facet information can be returned for the field.
doFacetEnabled :: Lens' DoubleOptions (Maybe Bool)
doFacetEnabled = lens _doFacetEnabled (\s a -> s { _doFacetEnabled = a })

-- | Whether the contents of the field are searchable.
doSearchEnabled :: Lens' DoubleOptions (Maybe Bool)
doSearchEnabled = lens _doSearchEnabled (\s a -> s { _doSearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
doReturnEnabled :: Lens' DoubleOptions (Maybe Bool)
doReturnEnabled = lens _doReturnEnabled (\s a -> s { _doReturnEnabled = a })

-- | Whether the field can be used to sort the search results.
doSortEnabled :: Lens' DoubleOptions (Maybe Bool)
doSortEnabled = lens _doSortEnabled (\s a -> s { _doSortEnabled = a })

instance FromXML DoubleOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DoubleOptions"

instance ToQuery DoubleOptions where
    toQuery = genericQuery def

-- | A named expression that can be evaluated at search time. Can be used for
-- sorting and filtering search results and constructing other expressions.
data Expression = Expression
    { _eExpressionName :: Text
    , _eExpressionValue :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Expression' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ExpressionName ::@ @Text@
--
-- * @ExpressionValue ::@ @Text@
--
expression :: Text -- ^ 'eExpressionName'
           -> Text -- ^ 'eExpressionValue'
           -> Expression
expression p1 p2 = Expression
    { _eExpressionName = p1
    , _eExpressionValue = p2
    }

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
eExpressionName :: Lens' Expression Text
eExpressionName = lens _eExpressionName (\s a -> s { _eExpressionName = a })

-- | The expression to evaluate for sorting while processing a search request.
-- The Expression syntax is based on JavaScript expressions. For more
-- information, see Configuring Expressions in the Amazon CloudSearch
-- Developer Guide.
eExpressionValue :: Lens' Expression Text
eExpressionValue =
    lens _eExpressionValue (\s a -> s { _eExpressionValue = a })

instance FromXML Expression where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Expression"

instance ToQuery Expression where
    toQuery = genericQuery def

-- | The value of an Expression and its current status.
data ExpressionStatus = ExpressionStatus
    { _esOptions :: Expression
    , _esStatus :: OptionStatus
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExpressionStatus' data type.
--
-- 'ExpressionStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Options ::@ @Expression@
--
-- * @Status ::@ @OptionStatus@
--
expressionStatus :: Expression -- ^ 'esOptions'
                 -> OptionStatus -- ^ 'esStatus'
                 -> ExpressionStatus
expressionStatus p1 p2 = ExpressionStatus
    { _esOptions = p1
    , _esStatus = p2
    }

-- | The expression that is evaluated for sorting or filtering while processing
-- a search request.
esOptions :: Lens' ExpressionStatus Expression
esOptions = lens _esOptions (\s a -> s { _esOptions = a })

-- | The status of domain configuration option.
esStatus :: Lens' ExpressionStatus OptionStatus
esStatus = lens _esStatus (\s a -> s { _esStatus = a })

instance FromXML ExpressionStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExpressionStatus"

-- | The index field and field options you want to configure.
data IndexField = IndexField
    { _ifIndexFieldName :: Text
    , _ifIndexFieldType :: IndexFieldType
    , _ifIntOptions :: Maybe IntOptions
    , _ifDoubleOptions :: Maybe DoubleOptions
    , _ifLiteralOptions :: Maybe LiteralOptions
    , _ifTextOptions :: Maybe TextOptions
    , _ifDateOptions :: Maybe DateOptions
    , _ifLatLonOptions :: Maybe LatLonOptions
    , _ifIntArrayOptions :: Maybe IntArrayOptions
    , _ifDoubleArrayOptions :: Maybe DoubleArrayOptions
    , _ifLiteralArrayOptions :: Maybe LiteralArrayOptions
    , _ifTextArrayOptions :: Maybe TextArrayOptions
    , _ifDateArrayOptions :: Maybe DateArrayOptions
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IndexField' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IndexFieldName ::@ @Text@
--
-- * @IndexFieldType ::@ @IndexFieldType@
--
-- * @IntOptions ::@ @Maybe IntOptions@
--
-- * @DoubleOptions ::@ @Maybe DoubleOptions@
--
-- * @LiteralOptions ::@ @Maybe LiteralOptions@
--
-- * @TextOptions ::@ @Maybe TextOptions@
--
-- * @DateOptions ::@ @Maybe DateOptions@
--
-- * @LatLonOptions ::@ @Maybe LatLonOptions@
--
-- * @IntArrayOptions ::@ @Maybe IntArrayOptions@
--
-- * @DoubleArrayOptions ::@ @Maybe DoubleArrayOptions@
--
-- * @LiteralArrayOptions ::@ @Maybe LiteralArrayOptions@
--
-- * @TextArrayOptions ::@ @Maybe TextArrayOptions@
--
-- * @DateArrayOptions ::@ @Maybe DateArrayOptions@
--
indexField :: Text -- ^ 'ifIndexFieldName'
           -> IndexFieldType -- ^ 'ifIndexFieldType'
           -> IndexField
indexField p1 p2 = IndexField
    { _ifIndexFieldName = p1
    , _ifIndexFieldType = p2
    , _ifIntOptions = Nothing
    , _ifDoubleOptions = Nothing
    , _ifLiteralOptions = Nothing
    , _ifTextOptions = Nothing
    , _ifDateOptions = Nothing
    , _ifLatLonOptions = Nothing
    , _ifIntArrayOptions = Nothing
    , _ifDoubleArrayOptions = Nothing
    , _ifLiteralArrayOptions = Nothing
    , _ifTextArrayOptions = Nothing
    , _ifDateArrayOptions = Nothing
    }

-- | The name of a field in the search index. Field names must begin with a
-- letter and can contain the following characters: a-z (lowercase), 0-9, and
-- _ (underscore). Uppercase letters and hyphens are not allowed. The name
-- "score" is reserved and cannot be specified as field or expression name.
ifIndexFieldName :: Lens' IndexField Text
ifIndexFieldName =
    lens _ifIndexFieldName (\s a -> s { _ifIndexFieldName = a })

-- | The type of field. The valid options for a field depend on the field type.
-- For more information about the supported field types, see Configuring Index
-- Fields in the Amazon CloudSearch Developer Guide.
ifIndexFieldType :: Lens' IndexField IndexFieldType
ifIndexFieldType =
    lens _ifIndexFieldType (\s a -> s { _ifIndexFieldType = a })

-- | Options for a 64-bit signed integer field. Present if IndexFieldType
-- specifies the field is of type int. All options are enabled by default.
ifIntOptions :: Lens' IndexField (Maybe IntOptions)
ifIntOptions = lens _ifIntOptions (\s a -> s { _ifIntOptions = a })

-- | Options for a double-precision 64-bit floating point field. Present if
-- IndexFieldType specifies the field is of type double. All options are
-- enabled by default.
ifDoubleOptions :: Lens' IndexField (Maybe DoubleOptions)
ifDoubleOptions = lens _ifDoubleOptions (\s a -> s { _ifDoubleOptions = a })

-- | Options for literal field. Present if IndexFieldType specifies the field is
-- of type literal. All options are enabled by default.
ifLiteralOptions :: Lens' IndexField (Maybe LiteralOptions)
ifLiteralOptions =
    lens _ifLiteralOptions (\s a -> s { _ifLiteralOptions = a })

-- | Options for text field. Present if IndexFieldType specifies the field is of
-- type text. A text field is always searchable. All options are enabled by
-- default.
ifTextOptions :: Lens' IndexField (Maybe TextOptions)
ifTextOptions = lens _ifTextOptions (\s a -> s { _ifTextOptions = a })

-- | Options for a date field. Dates and times are specified in UTC (Coordinated
-- Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if
-- IndexFieldType specifies the field is of type date. All options are enabled
-- by default.
ifDateOptions :: Lens' IndexField (Maybe DateOptions)
ifDateOptions = lens _ifDateOptions (\s a -> s { _ifDateOptions = a })

-- | Options for a latlon field. A latlon field contains a location stored as a
-- latitude and longitude value pair. Present if IndexFieldType specifies the
-- field is of type latlon. All options are enabled by default.
ifLatLonOptions :: Lens' IndexField (Maybe LatLonOptions)
ifLatLonOptions = lens _ifLatLonOptions (\s a -> s { _ifLatLonOptions = a })

-- | Options for a field that contains an array of 64-bit signed integers.
-- Present if IndexFieldType specifies the field is of type int-array. All
-- options are enabled by default.
ifIntArrayOptions :: Lens' IndexField (Maybe IntArrayOptions)
ifIntArrayOptions =
    lens _ifIntArrayOptions (\s a -> s { _ifIntArrayOptions = a })

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if IndexFieldType specifies the field is of
-- type double-array. All options are enabled by default.
ifDoubleArrayOptions :: Lens' IndexField (Maybe DoubleArrayOptions)
ifDoubleArrayOptions =
    lens _ifDoubleArrayOptions (\s a -> s { _ifDoubleArrayOptions = a })

-- | Options for a field that contains an array of literal strings. Present if
-- IndexFieldType specifies the field is of type literal-array. All options
-- are enabled by default.
ifLiteralArrayOptions :: Lens' IndexField (Maybe LiteralArrayOptions)
ifLiteralArrayOptions =
    lens _ifLiteralArrayOptions (\s a -> s { _ifLiteralArrayOptions = a })

-- | Options for a field that contains an array of text strings. Present if
-- IndexFieldType specifies the field is of type text-array. A text-array
-- field is always searchable. All options are enabled by default.
ifTextArrayOptions :: Lens' IndexField (Maybe TextArrayOptions)
ifTextArrayOptions =
    lens _ifTextArrayOptions (\s a -> s { _ifTextArrayOptions = a })

-- | Options for a field that contains an array of dates. Present if
-- IndexFieldType specifies the field is of type date-array. All options are
-- enabled by default.
ifDateArrayOptions :: Lens' IndexField (Maybe DateArrayOptions)
ifDateArrayOptions =
    lens _ifDateArrayOptions (\s a -> s { _ifDateArrayOptions = a })

instance FromXML IndexField where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexField"

instance ToQuery IndexField where
    toQuery = genericQuery def

-- | The value of an IndexField and its current status.
data IndexFieldStatus = IndexFieldStatus
    { _ifsOptions :: IndexField
    , _ifsStatus :: OptionStatus
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IndexFieldStatus' data type.
--
-- 'IndexFieldStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Options ::@ @IndexField@
--
-- * @Status ::@ @OptionStatus@
--
indexFieldStatus :: IndexField -- ^ 'ifsOptions'
                 -> OptionStatus -- ^ 'ifsStatus'
                 -> IndexFieldStatus
indexFieldStatus p1 p2 = IndexFieldStatus
    { _ifsOptions = p1
    , _ifsStatus = p2
    }

-- | Configuration information for a field in the index, including its name,
-- type, and options. The supported options depend on the IndexFieldType.
ifsOptions :: Lens' IndexFieldStatus IndexField
ifsOptions = lens _ifsOptions (\s a -> s { _ifsOptions = a })

-- | The status of domain configuration option.
ifsStatus :: Lens' IndexFieldStatus OptionStatus
ifsStatus = lens _ifsStatus (\s a -> s { _ifsStatus = a })

instance FromXML IndexFieldStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexFieldStatus"

-- | Options for a field that contains an array of 64-bit signed integers.
-- Present if IndexFieldType specifies the field is of type int-array. All
-- options are enabled by default.
data IntArrayOptions = IntArrayOptions
    { _iaoDefaultValue :: Maybe Integer
    , _iaoSourceFields :: Maybe Text
    , _iaoFacetEnabled :: Maybe Bool
    , _iaoSearchEnabled :: Maybe Bool
    , _iaoReturnEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IntArrayOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Integer@
--
-- * @SourceFields ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
intArrayOptions :: IntArrayOptions
intArrayOptions = IntArrayOptions
    { _iaoDefaultValue = Nothing
    , _iaoSourceFields = Nothing
    , _iaoFacetEnabled = Nothing
    , _iaoSearchEnabled = Nothing
    , _iaoReturnEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
iaoDefaultValue :: Lens' IntArrayOptions (Maybe Integer)
iaoDefaultValue = lens _iaoDefaultValue (\s a -> s { _iaoDefaultValue = a })

-- | A list of source fields to map to the field.
iaoSourceFields :: Lens' IntArrayOptions (Maybe Text)
iaoSourceFields = lens _iaoSourceFields (\s a -> s { _iaoSourceFields = a })

-- | Whether facet information can be returned for the field.
iaoFacetEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoFacetEnabled = lens _iaoFacetEnabled (\s a -> s { _iaoFacetEnabled = a })

-- | Whether the contents of the field are searchable.
iaoSearchEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoSearchEnabled =
    lens _iaoSearchEnabled (\s a -> s { _iaoSearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
iaoReturnEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoReturnEnabled =
    lens _iaoReturnEnabled (\s a -> s { _iaoReturnEnabled = a })

instance FromXML IntArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IntArrayOptions"

instance ToQuery IntArrayOptions where
    toQuery = genericQuery def

-- | Options for a 64-bit signed integer field. Present if IndexFieldType
-- specifies the field is of type int. All options are enabled by default.
data IntOptions = IntOptions
    { _ioDefaultValue :: Maybe Integer
    , _ioSourceField :: Maybe Text
    , _ioFacetEnabled :: Maybe Bool
    , _ioSearchEnabled :: Maybe Bool
    , _ioReturnEnabled :: Maybe Bool
    , _ioSortEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IntOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Integer@
--
-- * @SourceField ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
-- * @SortEnabled ::@ @Maybe Bool@
--
intOptions :: IntOptions
intOptions = IntOptions
    { _ioDefaultValue = Nothing
    , _ioSourceField = Nothing
    , _ioFacetEnabled = Nothing
    , _ioSearchEnabled = Nothing
    , _ioReturnEnabled = Nothing
    , _ioSortEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
-- This can be important if you are using the field in an expression and that
-- field is not present in every document.
ioDefaultValue :: Lens' IntOptions (Maybe Integer)
ioDefaultValue = lens _ioDefaultValue (\s a -> s { _ioDefaultValue = a })

-- | The name of the source field to map to the field.
ioSourceField :: Lens' IntOptions (Maybe Text)
ioSourceField = lens _ioSourceField (\s a -> s { _ioSourceField = a })

-- | Whether facet information can be returned for the field.
ioFacetEnabled :: Lens' IntOptions (Maybe Bool)
ioFacetEnabled = lens _ioFacetEnabled (\s a -> s { _ioFacetEnabled = a })

-- | Whether the contents of the field are searchable.
ioSearchEnabled :: Lens' IntOptions (Maybe Bool)
ioSearchEnabled = lens _ioSearchEnabled (\s a -> s { _ioSearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
ioReturnEnabled :: Lens' IntOptions (Maybe Bool)
ioReturnEnabled = lens _ioReturnEnabled (\s a -> s { _ioReturnEnabled = a })

-- | Whether the field can be used to sort the search results.
ioSortEnabled :: Lens' IntOptions (Maybe Bool)
ioSortEnabled = lens _ioSortEnabled (\s a -> s { _ioSortEnabled = a })

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
    , _lloSourceField :: Maybe Text
    , _lloFacetEnabled :: Maybe Bool
    , _lloSearchEnabled :: Maybe Bool
    , _lloReturnEnabled :: Maybe Bool
    , _lloSortEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LatLonOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @SourceField ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
-- * @SortEnabled ::@ @Maybe Bool@
--
latLonOptions :: LatLonOptions
latLonOptions = LatLonOptions
    { _lloDefaultValue = Nothing
    , _lloSourceField = Nothing
    , _lloFacetEnabled = Nothing
    , _lloSearchEnabled = Nothing
    , _lloReturnEnabled = Nothing
    , _lloSortEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
lloDefaultValue :: Lens' LatLonOptions (Maybe Text)
lloDefaultValue = lens _lloDefaultValue (\s a -> s { _lloDefaultValue = a })

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
lloSourceField :: Lens' LatLonOptions (Maybe Text)
lloSourceField = lens _lloSourceField (\s a -> s { _lloSourceField = a })

-- | Whether facet information can be returned for the field.
lloFacetEnabled :: Lens' LatLonOptions (Maybe Bool)
lloFacetEnabled = lens _lloFacetEnabled (\s a -> s { _lloFacetEnabled = a })

-- | Whether the contents of the field are searchable.
lloSearchEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSearchEnabled =
    lens _lloSearchEnabled (\s a -> s { _lloSearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
lloReturnEnabled :: Lens' LatLonOptions (Maybe Bool)
lloReturnEnabled =
    lens _lloReturnEnabled (\s a -> s { _lloReturnEnabled = a })

-- | Whether the field can be used to sort the search results.
lloSortEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSortEnabled = lens _lloSortEnabled (\s a -> s { _lloSortEnabled = a })

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
    , _laoSourceFields :: Maybe Text
    , _laoFacetEnabled :: Maybe Bool
    , _laoSearchEnabled :: Maybe Bool
    , _laoReturnEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LiteralArrayOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @SourceFields ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
literalArrayOptions :: LiteralArrayOptions
literalArrayOptions = LiteralArrayOptions
    { _laoDefaultValue = Nothing
    , _laoSourceFields = Nothing
    , _laoFacetEnabled = Nothing
    , _laoSearchEnabled = Nothing
    , _laoReturnEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
laoDefaultValue :: Lens' LiteralArrayOptions (Maybe Text)
laoDefaultValue = lens _laoDefaultValue (\s a -> s { _laoDefaultValue = a })

-- | A list of source fields to map to the field.
laoSourceFields :: Lens' LiteralArrayOptions (Maybe Text)
laoSourceFields = lens _laoSourceFields (\s a -> s { _laoSourceFields = a })

-- | Whether facet information can be returned for the field.
laoFacetEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoFacetEnabled = lens _laoFacetEnabled (\s a -> s { _laoFacetEnabled = a })

-- | Whether the contents of the field are searchable.
laoSearchEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoSearchEnabled =
    lens _laoSearchEnabled (\s a -> s { _laoSearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
laoReturnEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoReturnEnabled =
    lens _laoReturnEnabled (\s a -> s { _laoReturnEnabled = a })

instance FromXML LiteralArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LiteralArrayOptions"

instance ToQuery LiteralArrayOptions where
    toQuery = genericQuery def

-- | Options for literal field. Present if IndexFieldType specifies the field is
-- of type literal. All options are enabled by default.
data LiteralOptions = LiteralOptions
    { _loDefaultValue :: Maybe Text
    , _loSourceField :: Maybe Text
    , _loFacetEnabled :: Maybe Bool
    , _loSearchEnabled :: Maybe Bool
    , _loReturnEnabled :: Maybe Bool
    , _loSortEnabled :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LiteralOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @SourceField ::@ @Maybe Text@
--
-- * @FacetEnabled ::@ @Maybe Bool@
--
-- * @SearchEnabled ::@ @Maybe Bool@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
-- * @SortEnabled ::@ @Maybe Bool@
--
literalOptions :: LiteralOptions
literalOptions = LiteralOptions
    { _loDefaultValue = Nothing
    , _loSourceField = Nothing
    , _loFacetEnabled = Nothing
    , _loSearchEnabled = Nothing
    , _loReturnEnabled = Nothing
    , _loSortEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
loDefaultValue :: Lens' LiteralOptions (Maybe Text)
loDefaultValue = lens _loDefaultValue (\s a -> s { _loDefaultValue = a })

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
loSourceField :: Lens' LiteralOptions (Maybe Text)
loSourceField = lens _loSourceField (\s a -> s { _loSourceField = a })

-- | Whether facet information can be returned for the field.
loFacetEnabled :: Lens' LiteralOptions (Maybe Bool)
loFacetEnabled = lens _loFacetEnabled (\s a -> s { _loFacetEnabled = a })

-- | Whether the contents of the field are searchable.
loSearchEnabled :: Lens' LiteralOptions (Maybe Bool)
loSearchEnabled = lens _loSearchEnabled (\s a -> s { _loSearchEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
loReturnEnabled :: Lens' LiteralOptions (Maybe Bool)
loReturnEnabled = lens _loReturnEnabled (\s a -> s { _loReturnEnabled = a })

-- | Whether the field can be used to sort the search results.
loSortEnabled :: Lens' LiteralOptions (Maybe Bool)
loSortEnabled = lens _loSortEnabled (\s a -> s { _loSortEnabled = a })

instance FromXML LiteralOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LiteralOptions"

instance ToQuery LiteralOptions where
    toQuery = genericQuery def

-- | The status of domain configuration option.
data OptionStatus = OptionStatus
    { _osCreationDate :: ISO8601
    , _osUpdateDate :: ISO8601
    , _osUpdateVersion :: Maybe Integer
    , _osState :: OptionState
    , _osPendingDeletion :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CreationDate ::@ @ISO8601@
--
-- * @UpdateDate ::@ @ISO8601@
--
-- * @UpdateVersion ::@ @Maybe Integer@
--
-- * @State ::@ @OptionState@
--
-- * @PendingDeletion ::@ @Maybe Bool@
--
optionStatus :: ISO8601 -- ^ 'osCreationDate'
             -> ISO8601 -- ^ 'osUpdateDate'
             -> OptionState -- ^ 'osState'
             -> OptionStatus
optionStatus p1 p2 p4 = OptionStatus
    { _osCreationDate = p1
    , _osUpdateDate = p2
    , _osUpdateVersion = Nothing
    , _osState = p4
    , _osPendingDeletion = Nothing
    }

-- | A timestamp for when this option was created.
osCreationDate :: Lens' OptionStatus ISO8601
osCreationDate = lens _osCreationDate (\s a -> s { _osCreationDate = a })

-- | A timestamp for when this option was last updated.
osUpdateDate :: Lens' OptionStatus ISO8601
osUpdateDate = lens _osUpdateDate (\s a -> s { _osUpdateDate = a })

-- | A unique integer that indicates when this option was last updated.
osUpdateVersion :: Lens' OptionStatus (Maybe Integer)
osUpdateVersion = lens _osUpdateVersion (\s a -> s { _osUpdateVersion = a })

-- | The state of processing a change to an option. Possible values:
-- RequiresIndexDocuments: the option's latest value will not be deployed
-- until IndexDocuments has been called and indexing is complete. Processing:
-- the option's latest value is in the process of being activated. Active: the
-- option's latest value is completely deployed. FailedToValidate: the option
-- value is not compatible with the domain's data and cannot be used to index
-- the data. You must either modify the option value or update or remove the
-- incompatible documents.
osState :: Lens' OptionStatus OptionState
osState = lens _osState (\s a -> s { _osState = a })

-- | Indicates that the option will be deleted once processing is complete.
osPendingDeletion :: Lens' OptionStatus (Maybe Bool)
osPendingDeletion =
    lens _osPendingDeletion (\s a -> s { _osPendingDeletion = a })

instance FromXML OptionStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionStatus"

instance ToQuery OptionStatus where
    toQuery = genericQuery def

-- | The desired instance type and desired number of replicas of each index
-- partition.
data ScalingParameters = ScalingParameters
    { _spDesiredInstanceType :: Maybe PartitionInstanceType
    , _spDesiredReplicationCount :: Maybe Integer
    , _spDesiredPartitionCount :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ScalingParameters' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DesiredInstanceType ::@ @Maybe PartitionInstanceType@
--
-- * @DesiredReplicationCount ::@ @Maybe Integer@
--
-- * @DesiredPartitionCount ::@ @Maybe Integer@
--
scalingParameters :: ScalingParameters
scalingParameters = ScalingParameters
    { _spDesiredInstanceType = Nothing
    , _spDesiredReplicationCount = Nothing
    , _spDesiredPartitionCount = Nothing
    }

-- | The instance type that you want to preconfigure for your domain. For
-- example, search.m1.small.
spDesiredInstanceType :: Lens' ScalingParameters (Maybe PartitionInstanceType)
spDesiredInstanceType =
    lens _spDesiredInstanceType (\s a -> s { _spDesiredInstanceType = a })

-- | The number of replicas you want to preconfigure for each index partition.
spDesiredReplicationCount :: Lens' ScalingParameters (Maybe Integer)
spDesiredReplicationCount =
    lens _spDesiredReplicationCount
         (\s a -> s { _spDesiredReplicationCount = a })

-- | The number of partitions you want to preconfigure for your domain. Only
-- valid when you select m2.2xlarge as the desired instance type.
spDesiredPartitionCount :: Lens' ScalingParameters (Maybe Integer)
spDesiredPartitionCount =
    lens _spDesiredPartitionCount
         (\s a -> s { _spDesiredPartitionCount = a })

instance FromXML ScalingParameters where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingParameters"

instance ToQuery ScalingParameters where
    toQuery = genericQuery def

-- | The status and configuration of a search domain's scaling parameters.
data ScalingParametersStatus = ScalingParametersStatus
    { _spsOptions :: ScalingParameters
    , _spsStatus :: OptionStatus
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ScalingParametersStatus' data type.
--
-- 'ScalingParametersStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Options ::@ @ScalingParameters@
--
-- * @Status ::@ @OptionStatus@
--
scalingParametersStatus :: ScalingParameters -- ^ 'spsOptions'
                        -> OptionStatus -- ^ 'spsStatus'
                        -> ScalingParametersStatus
scalingParametersStatus p1 p2 = ScalingParametersStatus
    { _spsOptions = p1
    , _spsStatus = p2
    }

-- | The desired instance type and desired number of replicas of each index
-- partition.
spsOptions :: Lens' ScalingParametersStatus ScalingParameters
spsOptions = lens _spsOptions (\s a -> s { _spsOptions = a })

-- | The status of domain configuration option.
spsStatus :: Lens' ScalingParametersStatus OptionStatus
spsStatus = lens _spsStatus (\s a -> s { _spsStatus = a })

instance FromXML ScalingParametersStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingParametersStatus"

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for suggestions.
-- The following options can be configured for a suggester: FuzzyMatching,
-- SortExpression.
data Suggester = Suggester
    { _sSuggesterName :: Text
    , _sDocumentSuggesterOptions :: DocumentSuggesterOptions
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Suggester' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SuggesterName ::@ @Text@
--
-- * @DocumentSuggesterOptions ::@ @DocumentSuggesterOptions@
--
suggester :: Text -- ^ 'sSuggesterName'
          -> DocumentSuggesterOptions -- ^ 'sDocumentSuggesterOptions'
          -> Suggester
suggester p1 p2 = Suggester
    { _sSuggesterName = p1
    , _sDocumentSuggesterOptions = p2
    }

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
sSuggesterName :: Lens' Suggester Text
sSuggesterName = lens _sSuggesterName (\s a -> s { _sSuggesterName = a })

-- | Options for a search suggester.
sDocumentSuggesterOptions :: Lens' Suggester DocumentSuggesterOptions
sDocumentSuggesterOptions =
    lens _sDocumentSuggesterOptions
         (\s a -> s { _sDocumentSuggesterOptions = a })

instance FromXML Suggester where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Suggester"

instance ToQuery Suggester where
    toQuery = genericQuery def

-- | The value of a Suggester and its current status.
data SuggesterStatus = SuggesterStatus
    { _ssOptions :: Suggester
    , _ssStatus :: OptionStatus
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SuggesterStatus' data type.
--
-- 'SuggesterStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Options ::@ @Suggester@
--
-- * @Status ::@ @OptionStatus@
--
suggesterStatus :: Suggester -- ^ 'ssOptions'
                -> OptionStatus -- ^ 'ssStatus'
                -> SuggesterStatus
suggesterStatus p1 p2 = SuggesterStatus
    { _ssOptions = p1
    , _ssStatus = p2
    }

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for suggestions.
-- The following options can be configured for a suggester: FuzzyMatching,
-- SortExpression.
ssOptions :: Lens' SuggesterStatus Suggester
ssOptions = lens _ssOptions (\s a -> s { _ssOptions = a })

-- | The status of domain configuration option.
ssStatus :: Lens' SuggesterStatus OptionStatus
ssStatus = lens _ssStatus (\s a -> s { _ssStatus = a })

instance FromXML SuggesterStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuggesterStatus"

-- | Options for a field that contains an array of text strings. Present if
-- IndexFieldType specifies the field is of type text-array. A text-array
-- field is always searchable. All options are enabled by default.
data TextArrayOptions = TextArrayOptions
    { _taoDefaultValue :: Maybe Text
    , _taoSourceFields :: Maybe Text
    , _taoReturnEnabled :: Maybe Bool
    , _taoHighlightEnabled :: Maybe Bool
    , _taoAnalysisScheme :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TextArrayOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @SourceFields ::@ @Maybe Text@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
-- * @HighlightEnabled ::@ @Maybe Bool@
--
-- * @AnalysisScheme ::@ @Maybe Text@
--
textArrayOptions :: TextArrayOptions
textArrayOptions = TextArrayOptions
    { _taoDefaultValue = Nothing
    , _taoSourceFields = Nothing
    , _taoReturnEnabled = Nothing
    , _taoHighlightEnabled = Nothing
    , _taoAnalysisScheme = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
taoDefaultValue :: Lens' TextArrayOptions (Maybe Text)
taoDefaultValue = lens _taoDefaultValue (\s a -> s { _taoDefaultValue = a })

-- | A list of source fields to map to the field.
taoSourceFields :: Lens' TextArrayOptions (Maybe Text)
taoSourceFields = lens _taoSourceFields (\s a -> s { _taoSourceFields = a })

-- | Whether the contents of the field can be returned in the search results.
taoReturnEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoReturnEnabled =
    lens _taoReturnEnabled (\s a -> s { _taoReturnEnabled = a })

-- | Whether highlights can be returned for the field.
taoHighlightEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoHighlightEnabled =
    lens _taoHighlightEnabled (\s a -> s { _taoHighlightEnabled = a })

-- | The name of an analysis scheme for a text-array field.
taoAnalysisScheme :: Lens' TextArrayOptions (Maybe Text)
taoAnalysisScheme =
    lens _taoAnalysisScheme (\s a -> s { _taoAnalysisScheme = a })

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
    , _toSourceField :: Maybe Text
    , _toReturnEnabled :: Maybe Bool
    , _toSortEnabled :: Maybe Bool
    , _toHighlightEnabled :: Maybe Bool
    , _toAnalysisScheme :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TextOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @SourceField ::@ @Maybe Text@
--
-- * @ReturnEnabled ::@ @Maybe Bool@
--
-- * @SortEnabled ::@ @Maybe Bool@
--
-- * @HighlightEnabled ::@ @Maybe Bool@
--
-- * @AnalysisScheme ::@ @Maybe Text@
--
textOptions :: TextOptions
textOptions = TextOptions
    { _toDefaultValue = Nothing
    , _toSourceField = Nothing
    , _toReturnEnabled = Nothing
    , _toSortEnabled = Nothing
    , _toHighlightEnabled = Nothing
    , _toAnalysisScheme = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
toDefaultValue :: Lens' TextOptions (Maybe Text)
toDefaultValue = lens _toDefaultValue (\s a -> s { _toDefaultValue = a })

-- | A string that represents the name of an index field. Field names begin with
-- a letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). The name "score" is reserved and cannot be used as a
-- field name. To reference a document's ID, you can use the name _id.
toSourceField :: Lens' TextOptions (Maybe Text)
toSourceField = lens _toSourceField (\s a -> s { _toSourceField = a })

-- | Whether the contents of the field can be returned in the search results.
toReturnEnabled :: Lens' TextOptions (Maybe Bool)
toReturnEnabled = lens _toReturnEnabled (\s a -> s { _toReturnEnabled = a })

-- | Whether the field can be used to sort the search results.
toSortEnabled :: Lens' TextOptions (Maybe Bool)
toSortEnabled = lens _toSortEnabled (\s a -> s { _toSortEnabled = a })

-- | Whether highlights can be returned for the field.
toHighlightEnabled :: Lens' TextOptions (Maybe Bool)
toHighlightEnabled =
    lens _toHighlightEnabled (\s a -> s { _toHighlightEnabled = a })

-- | The name of an analysis scheme for a text field.
toAnalysisScheme :: Lens' TextOptions (Maybe Text)
toAnalysisScheme =
    lens _toAnalysisScheme (\s a -> s { _toAnalysisScheme = a })

instance FromXML TextOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TextOptions"

instance ToQuery TextOptions where
    toQuery = genericQuery def
