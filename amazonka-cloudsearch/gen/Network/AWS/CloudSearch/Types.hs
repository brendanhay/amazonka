{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudSearch.Types
    (
    -- * Service
      CloudSearch
    -- ** Error
    , RESTError
    -- ** XML
    , xmlOptions

    -- * DomainStatus
    , DomainStatus
    , domainStatus
    , dsARN
    , dsCreated
    , dsDeleted
    , dsDocService
    , dsDomainId
    , dsDomainName
    , dsLimits
    , dsProcessing
    , dsRequiresIndexDocuments
    , dsSearchInstanceCount
    , dsSearchInstanceType
    , dsSearchPartitionCount
    , dsSearchService

    -- * DocumentSuggesterOptions
    , DocumentSuggesterOptions
    , documentSuggesterOptions
    , dsoFuzzyMatching
    , dsoSortExpression
    , dsoSourceField

    -- * DoubleArrayOptions
    , DoubleArrayOptions
    , doubleArrayOptions
    , daoDefaultValue
    , daoFacetEnabled
    , daoReturnEnabled
    , daoSearchEnabled
    , daoSourceFields

    -- * IndexField
    , IndexField
    , indexField
    , ifDateArrayOptions
    , ifDateOptions
    , ifDoubleArrayOptions
    , ifDoubleOptions
    , ifIndexFieldName
    , ifIndexFieldType
    , ifIntArrayOptions
    , ifIntOptions
    , ifLatLonOptions
    , ifLiteralArrayOptions
    , ifLiteralOptions
    , ifTextArrayOptions
    , ifTextOptions

    -- * DateOptions
    , DateOptions
    , dateOptions
    , doDefaultValue
    , doFacetEnabled
    , doReturnEnabled
    , doSearchEnabled
    , doSortEnabled
    , doSourceField

    -- * OptionState
    , OptionState (..)

    -- * TextArrayOptions
    , TextArrayOptions
    , textArrayOptions
    , taoAnalysisScheme
    , taoDefaultValue
    , taoHighlightEnabled
    , taoReturnEnabled
    , taoSourceFields

    -- * AlgorithmicStemming
    , AlgorithmicStemming (..)

    -- * AnalysisScheme
    , AnalysisScheme
    , analysisScheme
    , asAnalysisOptions
    , asAnalysisSchemeLanguage
    , asAnalysisSchemeName

    -- * ScalingParameters
    , ScalingParameters
    , scalingParameters
    , spDesiredInstanceType
    , spDesiredPartitionCount
    , spDesiredReplicationCount

    -- * AnalysisOptions
    , AnalysisOptions
    , analysisOptions
    , aoAlgorithmicStemming
    , aoJapaneseTokenizationDictionary
    , aoStemmingDictionary
    , aoStopwords
    , aoSynonyms

    -- * DoubleOptions
    , DoubleOptions
    , doubleOptions
    , do1DefaultValue
    , do1FacetEnabled
    , do1ReturnEnabled
    , do1SearchEnabled
    , do1SortEnabled
    , do1SourceField

    -- * TextOptions
    , TextOptions
    , textOptions
    , toAnalysisScheme
    , toDefaultValue
    , toHighlightEnabled
    , toReturnEnabled
    , toSortEnabled
    , toSourceField

    -- * AvailabilityOptionsStatus
    , AvailabilityOptionsStatus
    , availabilityOptionsStatus
    , aosOptions
    , aosStatus

    -- * IndexFieldStatus
    , IndexFieldStatus
    , indexFieldStatus
    , ifsOptions
    , ifsStatus

    -- * ScalingParametersStatus
    , ScalingParametersStatus
    , scalingParametersStatus
    , spsOptions
    , spsStatus

    -- * AnalysisSchemeStatus
    , AnalysisSchemeStatus
    , analysisSchemeStatus
    , assOptions
    , assStatus

    -- * ServiceEndpoint
    , ServiceEndpoint
    , serviceEndpoint
    , seEndpoint

    -- * Limits
    , Limits
    , limits
    , lMaximumPartitionCount
    , lMaximumReplicationCount

    -- * ExpressionStatus
    , ExpressionStatus
    , expressionStatus
    , esOptions
    , esStatus

    -- * IndexFieldType
    , IndexFieldType (..)

    -- * LatLonOptions
    , LatLonOptions
    , latLonOptions
    , lloDefaultValue
    , lloFacetEnabled
    , lloReturnEnabled
    , lloSearchEnabled
    , lloSortEnabled
    , lloSourceField

    -- * SuggesterStatus
    , SuggesterStatus
    , suggesterStatus
    , ssOptions
    , ssStatus

    -- * OptionStatus
    , OptionStatus
    , optionStatus
    , osCreationDate
    , osPendingDeletion
    , osState
    , osUpdateDate
    , osUpdateVersion

    -- * LiteralArrayOptions
    , LiteralArrayOptions
    , literalArrayOptions
    , laoDefaultValue
    , laoFacetEnabled
    , laoReturnEnabled
    , laoSearchEnabled
    , laoSourceFields

    -- * IntArrayOptions
    , IntArrayOptions
    , intArrayOptions
    , iaoDefaultValue
    , iaoFacetEnabled
    , iaoReturnEnabled
    , iaoSearchEnabled
    , iaoSourceFields

    -- * Expression
    , Expression
    , expression
    , eExpressionName
    , eExpressionValue

    -- * SuggesterFuzzyMatching
    , SuggesterFuzzyMatching (..)

    -- * DateArrayOptions
    , DateArrayOptions
    , dateArrayOptions
    , dao1DefaultValue
    , dao1FacetEnabled
    , dao1ReturnEnabled
    , dao1SearchEnabled
    , dao1SourceFields

    -- * AnalysisSchemeLanguage
    , AnalysisSchemeLanguage (..)

    -- * PartitionInstanceType
    , PartitionInstanceType (..)

    -- * Suggester
    , Suggester
    , suggester
    , sDocumentSuggesterOptions
    , sSuggesterName

    -- * IntOptions
    , IntOptions
    , intOptions
    , ioDefaultValue
    , ioFacetEnabled
    , ioReturnEnabled
    , ioSearchEnabled
    , ioSortEnabled
    , ioSourceField

    -- * LiteralOptions
    , LiteralOptions
    , literalOptions
    , loDefaultValue
    , loFacetEnabled
    , loReturnEnabled
    , loSearchEnabled
    , loSortEnabled
    , loSourceField

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-01-01@) of the Amazon CloudSearch.
data CloudSearch deriving (Typeable)

instance AWSService CloudSearch where
    type Sg CloudSearch = V4
    type Er CloudSearch = RESTError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "CloudSearch"
        , _svcPrefix   = "cloudsearch"
        , _svcVersion  = "2013-01-01"
        , _svcTarget   = Nothing
        }

    handle = xmlError alwaysFail

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://cloudsearch.amazonaws.com/doc/2013-01-01/"
    }

data DomainStatus = DomainStatus
    { _dsARN                    :: Maybe Text
    , _dsCreated                :: Maybe Bool
    , _dsDeleted                :: Maybe Bool
    , _dsDocService             :: Maybe ServiceEndpoint
    , _dsDomainId               :: Text
    , _dsDomainName             :: Text
    , _dsLimits                 :: Maybe Limits
    , _dsProcessing             :: Maybe Bool
    , _dsRequiresIndexDocuments :: Bool
    , _dsSearchInstanceCount    :: Maybe Natural
    , _dsSearchInstanceType     :: Maybe Text
    , _dsSearchPartitionCount   :: Maybe Natural
    , _dsSearchService          :: Maybe ServiceEndpoint
    } (Eq, Show, Generic)

-- | 'DomainStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsARN' @::@ 'Maybe' 'Text'
--
-- * 'dsCreated' @::@ 'Maybe' 'Bool'
--
-- * 'dsDeleted' @::@ 'Maybe' 'Bool'
--
-- * 'dsDocService' @::@ 'Maybe' 'ServiceEndpoint'
--
-- * 'dsDomainId' @::@ 'Text'
--
-- * 'dsDomainName' @::@ 'Text'
--
-- * 'dsLimits' @::@ 'Maybe' 'Limits'
--
-- * 'dsProcessing' @::@ 'Maybe' 'Bool'
--
-- * 'dsRequiresIndexDocuments' @::@ 'Bool'
--
-- * 'dsSearchInstanceCount' @::@ 'Maybe' 'Natural'
--
-- * 'dsSearchInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'dsSearchPartitionCount' @::@ 'Maybe' 'Natural'
--
-- * 'dsSearchService' @::@ 'Maybe' 'ServiceEndpoint'
--
domainStatus :: Text -- ^ 'dsDomainId'
             -> Text -- ^ 'dsDomainName'
             -> Bool -- ^ 'dsRequiresIndexDocuments'
             -> DomainStatus
domainStatus p1 p2 p3 = DomainStatus
    { _dsDomainId               = p1
    , _dsDomainName             = p2
    , _dsRequiresIndexDocuments = p3
    , _dsARN                    = Nothing
    , _dsCreated                = Nothing
    , _dsDeleted                = Nothing
    , _dsDocService             = Nothing
    , _dsSearchService          = Nothing
    , _dsProcessing             = Nothing
    , _dsSearchInstanceType     = Nothing
    , _dsSearchPartitionCount   = Nothing
    , _dsSearchInstanceCount    = Nothing
    , _dsLimits                 = Nothing
    }

dsARN :: Lens' DomainStatus (Maybe Text)
dsARN = lens _dsARN (\s a -> s { _dsARN = a })

-- | True if the search domain is created. It can take several minutes to
-- initialize a domain when CreateDomain is called. Newly created search
-- domains are returned from DescribeDomains with a false value for Created
-- until domain creation is complete.
dsCreated :: Lens' DomainStatus (Maybe Bool)
dsCreated = lens _dsCreated (\s a -> s { _dsCreated = a })

-- | True if the search domain has been deleted. The system must clean up
-- resources dedicated to the search domain when DeleteDomain is called.
-- Newly deleted search domains are returned from DescribeDomains with a
-- true value for IsDeleted for several minutes until resource cleanup is
-- complete.
dsDeleted :: Lens' DomainStatus (Maybe Bool)
dsDeleted = lens _dsDeleted (\s a -> s { _dsDeleted = a })

-- | The service endpoint for updating documents in a search domain.
dsDocService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsDocService = lens _dsDocService (\s a -> s { _dsDocService = a })

dsDomainId :: Lens' DomainStatus Text
dsDomainId = lens _dsDomainId (\s a -> s { _dsDomainId = a })

dsDomainName :: Lens' DomainStatus Text
dsDomainName = lens _dsDomainName (\s a -> s { _dsDomainName = a })

dsLimits :: Lens' DomainStatus (Maybe Limits)
dsLimits = lens _dsLimits (\s a -> s { _dsLimits = a })

-- | True if processing is being done to activate the current domain
-- configuration.
dsProcessing :: Lens' DomainStatus (Maybe Bool)
dsProcessing = lens _dsProcessing (\s a -> s { _dsProcessing = a })

-- | True if IndexDocuments needs to be called to activate the current domain
-- configuration.
dsRequiresIndexDocuments :: Lens' DomainStatus Bool
dsRequiresIndexDocuments =
    lens _dsRequiresIndexDocuments
        (\s a -> s { _dsRequiresIndexDocuments = a })

-- | The number of search instances that are available to process search
-- requests.
dsSearchInstanceCount :: Lens' DomainStatus (Maybe Natural)
dsSearchInstanceCount =
    lens _dsSearchInstanceCount (\s a -> s { _dsSearchInstanceCount = a })

-- | The instance type that is being used to process search requests.
dsSearchInstanceType :: Lens' DomainStatus (Maybe Text)
dsSearchInstanceType =
    lens _dsSearchInstanceType (\s a -> s { _dsSearchInstanceType = a })

-- | The number of partitions across which the search index is spread.
dsSearchPartitionCount :: Lens' DomainStatus (Maybe Natural)
dsSearchPartitionCount =
    lens _dsSearchPartitionCount (\s a -> s { _dsSearchPartitionCount = a })

-- | The service endpoint for requesting search results from a search domain.
dsSearchService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsSearchService = lens _dsSearchService (\s a -> s { _dsSearchService = a })

instance FromXML DomainStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DomainStatus"

instance ToQuery DomainStatus

data DocumentSuggesterOptions = DocumentSuggesterOptions
    { _dsoFuzzyMatching  :: Maybe Text
    , _dsoSortExpression :: Maybe Text
    , _dsoSourceField    :: Text
    } (Eq, Ord, Show, Generic)

-- | 'DocumentSuggesterOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsoFuzzyMatching' @::@ 'Maybe' 'Text'
--
-- * 'dsoSortExpression' @::@ 'Maybe' 'Text'
--
-- * 'dsoSourceField' @::@ 'Text'
--
documentSuggesterOptions :: Text -- ^ 'dsoSourceField'
                         -> DocumentSuggesterOptions
documentSuggesterOptions p1 = DocumentSuggesterOptions
    { _dsoSourceField    = p1
    , _dsoFuzzyMatching  = Nothing
    , _dsoSortExpression = Nothing
    }

-- | The level of fuzziness allowed when suggesting matches for a string:
-- none, low, or high. With none, the specified string is treated as an
-- exact prefix. With low, suggestions must differ from the specified string
-- by no more than one character. With high, suggestions can differ by up to
-- two characters. The default is none.
dsoFuzzyMatching :: Lens' DocumentSuggesterOptions (Maybe Text)
dsoFuzzyMatching = lens _dsoFuzzyMatching (\s a -> s { _dsoFuzzyMatching = a })

-- | An expression that computes a score for each suggestion to control how
-- they are sorted. The scores are rounded to the nearest integer, with a
-- floor of 0 and a ceiling of 2^31-1. A document's relevance score is not
-- computed for suggestions, so sort expressions cannot reference the _score
-- value. To sort suggestions using a numeric field or existing expression,
-- simply specify the name of the field or expression. If no expression is
-- configured for the suggester, the suggestions are sorted with the closest
-- matches listed first.
dsoSortExpression :: Lens' DocumentSuggesterOptions (Maybe Text)
dsoSortExpression =
    lens _dsoSortExpression (\s a -> s { _dsoSortExpression = a })

-- | The name of the index field you want to use for suggestions.
dsoSourceField :: Lens' DocumentSuggesterOptions Text
dsoSourceField = lens _dsoSourceField (\s a -> s { _dsoSourceField = a })

instance FromXML DocumentSuggesterOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DocumentSuggesterOptions"

instance ToQuery DocumentSuggesterOptions

data DoubleArrayOptions = DoubleArrayOptions
    { _daoDefaultValue  :: Maybe Double
    , _daoFacetEnabled  :: Maybe Bool
    , _daoReturnEnabled :: Maybe Bool
    , _daoSearchEnabled :: Maybe Bool
    , _daoSourceFields  :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'DoubleArrayOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daoDefaultValue' @::@ 'Maybe' 'Double'
--
-- * 'daoFacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'daoReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'daoSearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'daoSourceFields' @::@ 'Maybe' 'Text'
--
doubleArrayOptions :: DoubleArrayOptions
doubleArrayOptions = DoubleArrayOptions
    { _daoDefaultValue  = Nothing
    , _daoSourceFields  = Nothing
    , _daoFacetEnabled  = Nothing
    , _daoSearchEnabled = Nothing
    , _daoReturnEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
daoDefaultValue :: Lens' DoubleArrayOptions (Maybe Double)
daoDefaultValue = lens _daoDefaultValue (\s a -> s { _daoDefaultValue = a })

-- | Whether facet information can be returned for the field.
daoFacetEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoFacetEnabled = lens _daoFacetEnabled (\s a -> s { _daoFacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
daoReturnEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoReturnEnabled = lens _daoReturnEnabled (\s a -> s { _daoReturnEnabled = a })

-- | Whether the contents of the field are searchable.
daoSearchEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoSearchEnabled = lens _daoSearchEnabled (\s a -> s { _daoSearchEnabled = a })

-- | A list of source fields to map to the field.
daoSourceFields :: Lens' DoubleArrayOptions (Maybe Text)
daoSourceFields = lens _daoSourceFields (\s a -> s { _daoSourceFields = a })

instance FromXML DoubleArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DoubleArrayOptions"

instance ToQuery DoubleArrayOptions

data IndexField = IndexField
    { _ifDateArrayOptions    :: Maybe DateArrayOptions
    , _ifDateOptions         :: Maybe DateOptions
    , _ifDoubleArrayOptions  :: Maybe DoubleArrayOptions
    , _ifDoubleOptions       :: Maybe DoubleOptions
    , _ifIndexFieldName      :: Text
    , _ifIndexFieldType      :: Text
    , _ifIntArrayOptions     :: Maybe IntArrayOptions
    , _ifIntOptions          :: Maybe IntOptions
    , _ifLatLonOptions       :: Maybe LatLonOptions
    , _ifLiteralArrayOptions :: Maybe LiteralArrayOptions
    , _ifLiteralOptions      :: Maybe LiteralOptions
    , _ifTextArrayOptions    :: Maybe TextArrayOptions
    , _ifTextOptions         :: Maybe TextOptions
    } (Eq, Show, Generic)

-- | 'IndexField' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ifDateArrayOptions' @::@ 'Maybe' 'DateArrayOptions'
--
-- * 'ifDateOptions' @::@ 'Maybe' 'DateOptions'
--
-- * 'ifDoubleArrayOptions' @::@ 'Maybe' 'DoubleArrayOptions'
--
-- * 'ifDoubleOptions' @::@ 'Maybe' 'DoubleOptions'
--
-- * 'ifIndexFieldName' @::@ 'Text'
--
-- * 'ifIndexFieldType' @::@ 'Text'
--
-- * 'ifIntArrayOptions' @::@ 'Maybe' 'IntArrayOptions'
--
-- * 'ifIntOptions' @::@ 'Maybe' 'IntOptions'
--
-- * 'ifLatLonOptions' @::@ 'Maybe' 'LatLonOptions'
--
-- * 'ifLiteralArrayOptions' @::@ 'Maybe' 'LiteralArrayOptions'
--
-- * 'ifLiteralOptions' @::@ 'Maybe' 'LiteralOptions'
--
-- * 'ifTextArrayOptions' @::@ 'Maybe' 'TextArrayOptions'
--
-- * 'ifTextOptions' @::@ 'Maybe' 'TextOptions'
--
indexField :: Text -- ^ 'ifIndexFieldName'
           -> Text -- ^ 'ifIndexFieldType'
           -> IndexField
indexField p1 p2 = IndexField
    { _ifIndexFieldName      = p1
    , _ifIndexFieldType      = p2
    , _ifIntOptions          = Nothing
    , _ifDoubleOptions       = Nothing
    , _ifLiteralOptions      = Nothing
    , _ifTextOptions         = Nothing
    , _ifDateOptions         = Nothing
    , _ifLatLonOptions       = Nothing
    , _ifIntArrayOptions     = Nothing
    , _ifDoubleArrayOptions  = Nothing
    , _ifLiteralArrayOptions = Nothing
    , _ifTextArrayOptions    = Nothing
    , _ifDateArrayOptions    = Nothing
    }

ifDateArrayOptions :: Lens' IndexField (Maybe DateArrayOptions)
ifDateArrayOptions =
    lens _ifDateArrayOptions (\s a -> s { _ifDateArrayOptions = a })

ifDateOptions :: Lens' IndexField (Maybe DateOptions)
ifDateOptions = lens _ifDateOptions (\s a -> s { _ifDateOptions = a })

ifDoubleArrayOptions :: Lens' IndexField (Maybe DoubleArrayOptions)
ifDoubleArrayOptions =
    lens _ifDoubleArrayOptions (\s a -> s { _ifDoubleArrayOptions = a })

ifDoubleOptions :: Lens' IndexField (Maybe DoubleOptions)
ifDoubleOptions = lens _ifDoubleOptions (\s a -> s { _ifDoubleOptions = a })

-- | The name of a field in the search index. Field names must begin with a
-- letter and can contain the following characters: a-z (lowercase), 0-9,
-- and _ (underscore). Uppercase letters and hyphens are not allowed. The
-- name "score" is reserved and cannot be specified as field or expression
-- name.
ifIndexFieldName :: Lens' IndexField Text
ifIndexFieldName = lens _ifIndexFieldName (\s a -> s { _ifIndexFieldName = a })

ifIndexFieldType :: Lens' IndexField Text
ifIndexFieldType = lens _ifIndexFieldType (\s a -> s { _ifIndexFieldType = a })

ifIntArrayOptions :: Lens' IndexField (Maybe IntArrayOptions)
ifIntArrayOptions =
    lens _ifIntArrayOptions (\s a -> s { _ifIntArrayOptions = a })

ifIntOptions :: Lens' IndexField (Maybe IntOptions)
ifIntOptions = lens _ifIntOptions (\s a -> s { _ifIntOptions = a })

ifLatLonOptions :: Lens' IndexField (Maybe LatLonOptions)
ifLatLonOptions = lens _ifLatLonOptions (\s a -> s { _ifLatLonOptions = a })

ifLiteralArrayOptions :: Lens' IndexField (Maybe LiteralArrayOptions)
ifLiteralArrayOptions =
    lens _ifLiteralArrayOptions (\s a -> s { _ifLiteralArrayOptions = a })

ifLiteralOptions :: Lens' IndexField (Maybe LiteralOptions)
ifLiteralOptions = lens _ifLiteralOptions (\s a -> s { _ifLiteralOptions = a })

ifTextArrayOptions :: Lens' IndexField (Maybe TextArrayOptions)
ifTextArrayOptions =
    lens _ifTextArrayOptions (\s a -> s { _ifTextArrayOptions = a })

ifTextOptions :: Lens' IndexField (Maybe TextOptions)
ifTextOptions = lens _ifTextOptions (\s a -> s { _ifTextOptions = a })

instance FromXML IndexField where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexField"

instance ToQuery IndexField

data DateOptions = DateOptions
    { _doDefaultValue  :: Maybe Text
    , _doFacetEnabled  :: Maybe Bool
    , _doReturnEnabled :: Maybe Bool
    , _doSearchEnabled :: Maybe Bool
    , _doSortEnabled   :: Maybe Bool
    , _doSourceField   :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'DateOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'doFacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'doReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'doSearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'doSortEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'doSourceField' @::@ 'Maybe' 'Text'
--
dateOptions :: DateOptions
dateOptions = DateOptions
    { _doDefaultValue  = Nothing
    , _doSourceField   = Nothing
    , _doFacetEnabled  = Nothing
    , _doSearchEnabled = Nothing
    , _doReturnEnabled = Nothing
    , _doSortEnabled   = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
doDefaultValue :: Lens' DateOptions (Maybe Text)
doDefaultValue = lens _doDefaultValue (\s a -> s { _doDefaultValue = a })

-- | Whether facet information can be returned for the field.
doFacetEnabled :: Lens' DateOptions (Maybe Bool)
doFacetEnabled = lens _doFacetEnabled (\s a -> s { _doFacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
doReturnEnabled :: Lens' DateOptions (Maybe Bool)
doReturnEnabled = lens _doReturnEnabled (\s a -> s { _doReturnEnabled = a })

-- | Whether the contents of the field are searchable.
doSearchEnabled :: Lens' DateOptions (Maybe Bool)
doSearchEnabled = lens _doSearchEnabled (\s a -> s { _doSearchEnabled = a })

-- | Whether the field can be used to sort the search results.
doSortEnabled :: Lens' DateOptions (Maybe Bool)
doSortEnabled = lens _doSortEnabled (\s a -> s { _doSortEnabled = a })

doSourceField :: Lens' DateOptions (Maybe Text)
doSourceField = lens _doSourceField (\s a -> s { _doSourceField = a })

instance FromXML DateOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DateOptions"

instance ToQuery DateOptions

data OptionState
    = Active                 -- ^ Active
    | FailedToValidate       -- ^ FailedToValidate
    | Processing             -- ^ Processing
    | RequiresIndexDocuments -- ^ RequiresIndexDocuments
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable OptionState

instance FromText OptionState where
    parser = match "Active"                 Active
         <|> match "FailedToValidate"       FailedToValidate
         <|> match "Processing"             Processing
         <|> match "RequiresIndexDocuments" RequiresIndexDocuments

instance ToText OptionState where
    toText = \case
        Active                 -> "Active"
        FailedToValidate       -> "FailedToValidate"
        Processing             -> "Processing"
        RequiresIndexDocuments -> "RequiresIndexDocuments"

instance FromXML OptionState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionState"

instance ToQuery OptionState

data TextArrayOptions = TextArrayOptions
    { _taoAnalysisScheme   :: Maybe Text
    , _taoDefaultValue     :: Maybe Text
    , _taoHighlightEnabled :: Maybe Bool
    , _taoReturnEnabled    :: Maybe Bool
    , _taoSourceFields     :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'TextArrayOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'taoAnalysisScheme' @::@ 'Maybe' 'Text'
--
-- * 'taoDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'taoHighlightEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'taoReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'taoSourceFields' @::@ 'Maybe' 'Text'
--
textArrayOptions :: TextArrayOptions
textArrayOptions = TextArrayOptions
    { _taoDefaultValue     = Nothing
    , _taoSourceFields     = Nothing
    , _taoReturnEnabled    = Nothing
    , _taoHighlightEnabled = Nothing
    , _taoAnalysisScheme   = Nothing
    }

-- | The name of an analysis scheme for a text-array field.
taoAnalysisScheme :: Lens' TextArrayOptions (Maybe Text)
taoAnalysisScheme =
    lens _taoAnalysisScheme (\s a -> s { _taoAnalysisScheme = a })

-- | A value to use for the field if the field isn't specified for a document.
taoDefaultValue :: Lens' TextArrayOptions (Maybe Text)
taoDefaultValue = lens _taoDefaultValue (\s a -> s { _taoDefaultValue = a })

-- | Whether highlights can be returned for the field.
taoHighlightEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoHighlightEnabled =
    lens _taoHighlightEnabled (\s a -> s { _taoHighlightEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
taoReturnEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoReturnEnabled = lens _taoReturnEnabled (\s a -> s { _taoReturnEnabled = a })

-- | A list of source fields to map to the field.
taoSourceFields :: Lens' TextArrayOptions (Maybe Text)
taoSourceFields = lens _taoSourceFields (\s a -> s { _taoSourceFields = a })

instance FromXML TextArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TextArrayOptions"

instance ToQuery TextArrayOptions

data AlgorithmicStemming
    = Full    -- ^ full
    | Light   -- ^ light
    | Minimal -- ^ minimal
    | None    -- ^ none
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable AlgorithmicStemming

instance FromText AlgorithmicStemming where
    parser = match "full"    Full
         <|> match "light"   Light
         <|> match "minimal" Minimal
         <|> match "none"    None

instance ToText AlgorithmicStemming where
    toText = \case
        Full    -> "full"
        Light   -> "light"
        Minimal -> "minimal"
        None    -> "none"

instance FromXML AlgorithmicStemming where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AlgorithmicStemming"

instance ToQuery AlgorithmicStemming

data AnalysisScheme = AnalysisScheme
    { _asAnalysisOptions        :: Maybe AnalysisOptions
    , _asAnalysisSchemeLanguage :: Text
    , _asAnalysisSchemeName     :: Text
    } (Eq, Show, Generic)

-- | 'AnalysisScheme' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asAnalysisOptions' @::@ 'Maybe' 'AnalysisOptions'
--
-- * 'asAnalysisSchemeLanguage' @::@ 'Text'
--
-- * 'asAnalysisSchemeName' @::@ 'Text'
--
analysisScheme :: Text -- ^ 'asAnalysisSchemeName'
               -> Text -- ^ 'asAnalysisSchemeLanguage'
               -> AnalysisScheme
analysisScheme p1 p2 = AnalysisScheme
    { _asAnalysisSchemeName     = p1
    , _asAnalysisSchemeLanguage = p2
    , _asAnalysisOptions        = Nothing
    }

asAnalysisOptions :: Lens' AnalysisScheme (Maybe AnalysisOptions)
asAnalysisOptions =
    lens _asAnalysisOptions (\s a -> s { _asAnalysisOptions = a })

asAnalysisSchemeLanguage :: Lens' AnalysisScheme Text
asAnalysisSchemeLanguage =
    lens _asAnalysisSchemeLanguage
        (\s a -> s { _asAnalysisSchemeLanguage = a })

asAnalysisSchemeName :: Lens' AnalysisScheme Text
asAnalysisSchemeName =
    lens _asAnalysisSchemeName (\s a -> s { _asAnalysisSchemeName = a })

instance FromXML AnalysisScheme where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisScheme"

instance ToQuery AnalysisScheme

data ScalingParameters = ScalingParameters
    { _spDesiredInstanceType     :: Maybe Text
    , _spDesiredPartitionCount   :: Maybe Natural
    , _spDesiredReplicationCount :: Maybe Natural
    } (Eq, Ord, Show, Generic)

-- | 'ScalingParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spDesiredInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'spDesiredPartitionCount' @::@ 'Maybe' 'Natural'
--
-- * 'spDesiredReplicationCount' @::@ 'Maybe' 'Natural'
--
scalingParameters :: ScalingParameters
scalingParameters = ScalingParameters
    { _spDesiredInstanceType     = Nothing
    , _spDesiredReplicationCount = Nothing
    , _spDesiredPartitionCount   = Nothing
    }

-- | The instance type that you want to preconfigure for your domain. For
-- example, search.m1.small.
spDesiredInstanceType :: Lens' ScalingParameters (Maybe Text)
spDesiredInstanceType =
    lens _spDesiredInstanceType (\s a -> s { _spDesiredInstanceType = a })

-- | The number of partitions you want to preconfigure for your domain. Only
-- valid when you select m2.2xlarge as the desired instance type.
spDesiredPartitionCount :: Lens' ScalingParameters (Maybe Natural)
spDesiredPartitionCount =
    lens _spDesiredPartitionCount (\s a -> s { _spDesiredPartitionCount = a })

-- | The number of replicas you want to preconfigure for each index partition.
spDesiredReplicationCount :: Lens' ScalingParameters (Maybe Natural)
spDesiredReplicationCount =
    lens _spDesiredReplicationCount
        (\s a -> s { _spDesiredReplicationCount = a })

instance FromXML ScalingParameters where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingParameters"

instance ToQuery ScalingParameters

data AnalysisOptions = AnalysisOptions
    { _aoAlgorithmicStemming            :: Maybe Text
    , _aoJapaneseTokenizationDictionary :: Maybe Text
    , _aoStemmingDictionary             :: Maybe Text
    , _aoStopwords                      :: Maybe Text
    , _aoSynonyms                       :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'AnalysisOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aoAlgorithmicStemming' @::@ 'Maybe' 'Text'
--
-- * 'aoJapaneseTokenizationDictionary' @::@ 'Maybe' 'Text'
--
-- * 'aoStemmingDictionary' @::@ 'Maybe' 'Text'
--
-- * 'aoStopwords' @::@ 'Maybe' 'Text'
--
-- * 'aoSynonyms' @::@ 'Maybe' 'Text'
--
analysisOptions :: AnalysisOptions
analysisOptions = AnalysisOptions
    { _aoSynonyms                       = Nothing
    , _aoStopwords                      = Nothing
    , _aoStemmingDictionary             = Nothing
    , _aoJapaneseTokenizationDictionary = Nothing
    , _aoAlgorithmicStemming            = Nothing
    }

-- | The level of algorithmic stemming to perform: none, minimal, light, or
-- full. The available levels vary depending on the language. For more
-- information, see Language Specific Text Processing Settings in the Amazon
-- CloudSearch Developer Guide.
aoAlgorithmicStemming :: Lens' AnalysisOptions (Maybe Text)
aoAlgorithmicStemming =
    lens _aoAlgorithmicStemming (\s a -> s { _aoAlgorithmicStemming = a })

-- | A JSON array that contains a collection of terms, tokens, readings and
-- part of speech for Japanese Tokenizaiton. The Japanese tokenization
-- dictionary enables you to override the default tokenization for selected
-- terms. This is only valid for Japanese language fields.
aoJapaneseTokenizationDictionary :: Lens' AnalysisOptions (Maybe Text)
aoJapaneseTokenizationDictionary =
    lens _aoJapaneseTokenizationDictionary
        (\s a -> s { _aoJapaneseTokenizationDictionary = a })

-- | A JSON object that contains a collection of string:value pairs that each
-- map a term to its stem. For example, {"term1": "stem1", "term2": "stem2",
-- "term3": "stem3"}. The stemming dictionary is applied in addition to any
-- algorithmic stemming. This enables you to override the results of the
-- algorithmic stemming to correct specific cases of overstemming or
-- understemming. The maximum size of a stemming dictionary is 500 KB.
aoStemmingDictionary :: Lens' AnalysisOptions (Maybe Text)
aoStemmingDictionary =
    lens _aoStemmingDictionary (\s a -> s { _aoStemmingDictionary = a })

-- | A JSON array of terms to ignore during indexing and searching. For
-- example, ["a", "an", "the", "of"]. The stopwords dictionary must
-- explicitly list each word you want to ignore. Wildcards and regular
-- expressions are not supported.
aoStopwords :: Lens' AnalysisOptions (Maybe Text)
aoStopwords = lens _aoStopwords (\s a -> s { _aoStopwords = a })

-- | A JSON object that defines synonym groups and aliases. A synonym group is
-- an array of arrays, where each sub-array is a group of terms where each
-- term in the group is considered a synonym of every other term in the
-- group. The aliases value is an object that contains a collection of
-- string:value pairs where the string specifies a term and the array of
-- values specifies each of the aliases for that term. An alias is
-- considered a synonym of the specified term, but the term is not
-- considered a synonym of the alias. For more information about specifying
-- synonyms, see Synonyms in the Amazon CloudSearch Developer Guide.
aoSynonyms :: Lens' AnalysisOptions (Maybe Text)
aoSynonyms = lens _aoSynonyms (\s a -> s { _aoSynonyms = a })

instance FromXML AnalysisOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisOptions"

instance ToQuery AnalysisOptions

data DoubleOptions = DoubleOptions
    { _do1DefaultValue  :: Maybe Double
    , _do1FacetEnabled  :: Maybe Bool
    , _do1ReturnEnabled :: Maybe Bool
    , _do1SearchEnabled :: Maybe Bool
    , _do1SortEnabled   :: Maybe Bool
    , _do1SourceField   :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'DoubleOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'do1DefaultValue' @::@ 'Maybe' 'Double'
--
-- * 'do1FacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'do1ReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'do1SearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'do1SortEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'do1SourceField' @::@ 'Maybe' 'Text'
--
doubleOptions :: DoubleOptions
doubleOptions = DoubleOptions
    { _do1DefaultValue  = Nothing
    , _do1SourceField   = Nothing
    , _do1FacetEnabled  = Nothing
    , _do1SearchEnabled = Nothing
    , _do1ReturnEnabled = Nothing
    , _do1SortEnabled   = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
-- This can be important if you are using the field in an expression and
-- that field is not present in every document.
do1DefaultValue :: Lens' DoubleOptions (Maybe Double)
do1DefaultValue = lens _do1DefaultValue (\s a -> s { _do1DefaultValue = a })

-- | Whether facet information can be returned for the field.
do1FacetEnabled :: Lens' DoubleOptions (Maybe Bool)
do1FacetEnabled = lens _do1FacetEnabled (\s a -> s { _do1FacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
do1ReturnEnabled :: Lens' DoubleOptions (Maybe Bool)
do1ReturnEnabled = lens _do1ReturnEnabled (\s a -> s { _do1ReturnEnabled = a })

-- | Whether the contents of the field are searchable.
do1SearchEnabled :: Lens' DoubleOptions (Maybe Bool)
do1SearchEnabled = lens _do1SearchEnabled (\s a -> s { _do1SearchEnabled = a })

-- | Whether the field can be used to sort the search results.
do1SortEnabled :: Lens' DoubleOptions (Maybe Bool)
do1SortEnabled = lens _do1SortEnabled (\s a -> s { _do1SortEnabled = a })

-- | The name of the source field to map to the field.
do1SourceField :: Lens' DoubleOptions (Maybe Text)
do1SourceField = lens _do1SourceField (\s a -> s { _do1SourceField = a })

instance FromXML DoubleOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DoubleOptions"

instance ToQuery DoubleOptions

data TextOptions = TextOptions
    { _toAnalysisScheme   :: Maybe Text
    , _toDefaultValue     :: Maybe Text
    , _toHighlightEnabled :: Maybe Bool
    , _toReturnEnabled    :: Maybe Bool
    , _toSortEnabled      :: Maybe Bool
    , _toSourceField      :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'TextOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'toAnalysisScheme' @::@ 'Maybe' 'Text'
--
-- * 'toDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'toHighlightEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'toReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'toSortEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'toSourceField' @::@ 'Maybe' 'Text'
--
textOptions :: TextOptions
textOptions = TextOptions
    { _toDefaultValue     = Nothing
    , _toSourceField      = Nothing
    , _toReturnEnabled    = Nothing
    , _toSortEnabled      = Nothing
    , _toHighlightEnabled = Nothing
    , _toAnalysisScheme   = Nothing
    }

-- | The name of an analysis scheme for a text field.
toAnalysisScheme :: Lens' TextOptions (Maybe Text)
toAnalysisScheme = lens _toAnalysisScheme (\s a -> s { _toAnalysisScheme = a })

-- | A value to use for the field if the field isn't specified for a document.
toDefaultValue :: Lens' TextOptions (Maybe Text)
toDefaultValue = lens _toDefaultValue (\s a -> s { _toDefaultValue = a })

-- | Whether highlights can be returned for the field.
toHighlightEnabled :: Lens' TextOptions (Maybe Bool)
toHighlightEnabled =
    lens _toHighlightEnabled (\s a -> s { _toHighlightEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
toReturnEnabled :: Lens' TextOptions (Maybe Bool)
toReturnEnabled = lens _toReturnEnabled (\s a -> s { _toReturnEnabled = a })

-- | Whether the field can be used to sort the search results.
toSortEnabled :: Lens' TextOptions (Maybe Bool)
toSortEnabled = lens _toSortEnabled (\s a -> s { _toSortEnabled = a })

toSourceField :: Lens' TextOptions (Maybe Text)
toSourceField = lens _toSourceField (\s a -> s { _toSourceField = a })

instance FromXML TextOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TextOptions"

instance ToQuery TextOptions

data AvailabilityOptionsStatus = AvailabilityOptionsStatus
    { _aosOptions :: Bool
    , _aosStatus  :: OptionStatus
    } (Eq, Show, Generic)

-- | 'AvailabilityOptionsStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aosOptions' @::@ 'Bool'
--
-- * 'aosStatus' @::@ 'OptionStatus'
--
availabilityOptionsStatus :: Bool -- ^ 'aosOptions'
                          -> OptionStatus -- ^ 'aosStatus'
                          -> AvailabilityOptionsStatus
availabilityOptionsStatus p1 p2 = AvailabilityOptionsStatus
    { _aosOptions = p1
    , _aosStatus  = p2
    }

-- | The availability options configured for the domain.
aosOptions :: Lens' AvailabilityOptionsStatus Bool
aosOptions = lens _aosOptions (\s a -> s { _aosOptions = a })

aosStatus :: Lens' AvailabilityOptionsStatus OptionStatus
aosStatus = lens _aosStatus (\s a -> s { _aosStatus = a })

instance FromXML AvailabilityOptionsStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityOptionsStatus"

instance ToQuery AvailabilityOptionsStatus

data IndexFieldStatus = IndexFieldStatus
    { _ifsOptions :: IndexField
    , _ifsStatus  :: OptionStatus
    } (Eq, Show, Generic)

-- | 'IndexFieldStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ifsOptions' @::@ 'IndexField'
--
-- * 'ifsStatus' @::@ 'OptionStatus'
--
indexFieldStatus :: IndexField -- ^ 'ifsOptions'
                 -> OptionStatus -- ^ 'ifsStatus'
                 -> IndexFieldStatus
indexFieldStatus p1 p2 = IndexFieldStatus
    { _ifsOptions = p1
    , _ifsStatus  = p2
    }

ifsOptions :: Lens' IndexFieldStatus IndexField
ifsOptions = lens _ifsOptions (\s a -> s { _ifsOptions = a })

ifsStatus :: Lens' IndexFieldStatus OptionStatus
ifsStatus = lens _ifsStatus (\s a -> s { _ifsStatus = a })

instance FromXML IndexFieldStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexFieldStatus"

instance ToQuery IndexFieldStatus

data ScalingParametersStatus = ScalingParametersStatus
    { _spsOptions :: ScalingParameters
    , _spsStatus  :: OptionStatus
    } (Eq, Show, Generic)

-- | 'ScalingParametersStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spsOptions' @::@ 'ScalingParameters'
--
-- * 'spsStatus' @::@ 'OptionStatus'
--
scalingParametersStatus :: ScalingParameters -- ^ 'spsOptions'
                        -> OptionStatus -- ^ 'spsStatus'
                        -> ScalingParametersStatus
scalingParametersStatus p1 p2 = ScalingParametersStatus
    { _spsOptions = p1
    , _spsStatus  = p2
    }

spsOptions :: Lens' ScalingParametersStatus ScalingParameters
spsOptions = lens _spsOptions (\s a -> s { _spsOptions = a })

spsStatus :: Lens' ScalingParametersStatus OptionStatus
spsStatus = lens _spsStatus (\s a -> s { _spsStatus = a })

instance FromXML ScalingParametersStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingParametersStatus"

instance ToQuery ScalingParametersStatus

data AnalysisSchemeStatus = AnalysisSchemeStatus
    { _assOptions :: AnalysisScheme
    , _assStatus  :: OptionStatus
    } (Eq, Show, Generic)

-- | 'AnalysisSchemeStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'assOptions' @::@ 'AnalysisScheme'
--
-- * 'assStatus' @::@ 'OptionStatus'
--
analysisSchemeStatus :: AnalysisScheme -- ^ 'assOptions'
                     -> OptionStatus -- ^ 'assStatus'
                     -> AnalysisSchemeStatus
analysisSchemeStatus p1 p2 = AnalysisSchemeStatus
    { _assOptions = p1
    , _assStatus  = p2
    }

assOptions :: Lens' AnalysisSchemeStatus AnalysisScheme
assOptions = lens _assOptions (\s a -> s { _assOptions = a })

assStatus :: Lens' AnalysisSchemeStatus OptionStatus
assStatus = lens _assStatus (\s a -> s { _assStatus = a })

instance FromXML AnalysisSchemeStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisSchemeStatus"

instance ToQuery AnalysisSchemeStatus

newtype ServiceEndpoint = ServiceEndpoint
    { _seEndpoint :: Maybe Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'ServiceEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seEndpoint' @::@ 'Maybe' 'Text'
--
serviceEndpoint :: ServiceEndpoint
serviceEndpoint = ServiceEndpoint
    { _seEndpoint = Nothing
    }

seEndpoint :: Lens' ServiceEndpoint (Maybe Text)
seEndpoint = lens _seEndpoint (\s a -> s { _seEndpoint = a })

instance FromXML ServiceEndpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServiceEndpoint"

instance ToQuery ServiceEndpoint

data Limits = Limits
    { _lMaximumPartitionCount   :: Natural
    , _lMaximumReplicationCount :: Natural
    } (Eq, Ord, Show, Generic)

-- | 'Limits' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lMaximumPartitionCount' @::@ 'Natural'
--
-- * 'lMaximumReplicationCount' @::@ 'Natural'
--
limits :: Natural -- ^ 'lMaximumReplicationCount'
       -> Natural -- ^ 'lMaximumPartitionCount'
       -> Limits
limits p1 p2 = Limits
    { _lMaximumReplicationCount = p1
    , _lMaximumPartitionCount   = p2
    }

lMaximumPartitionCount :: Lens' Limits Natural
lMaximumPartitionCount =
    lens _lMaximumPartitionCount (\s a -> s { _lMaximumPartitionCount = a })

lMaximumReplicationCount :: Lens' Limits Natural
lMaximumReplicationCount =
    lens _lMaximumReplicationCount
        (\s a -> s { _lMaximumReplicationCount = a })

instance FromXML Limits where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Limits"

instance ToQuery Limits

data ExpressionStatus = ExpressionStatus
    { _esOptions :: Expression
    , _esStatus  :: OptionStatus
    } (Eq, Show, Generic)

-- | 'ExpressionStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esOptions' @::@ 'Expression'
--
-- * 'esStatus' @::@ 'OptionStatus'
--
expressionStatus :: Expression -- ^ 'esOptions'
                 -> OptionStatus -- ^ 'esStatus'
                 -> ExpressionStatus
expressionStatus p1 p2 = ExpressionStatus
    { _esOptions = p1
    , _esStatus  = p2
    }

-- | The expression that is evaluated for sorting while processing a search
-- request.
esOptions :: Lens' ExpressionStatus Expression
esOptions = lens _esOptions (\s a -> s { _esOptions = a })

esStatus :: Lens' ExpressionStatus OptionStatus
esStatus = lens _esStatus (\s a -> s { _esStatus = a })

instance FromXML ExpressionStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExpressionStatus"

instance ToQuery ExpressionStatus

data IndexFieldType
    = IFTDate         -- ^ date
    | IFTDateArray    -- ^ date-array
    | IFTDouble       -- ^ double
    | IFTDoubleArray  -- ^ double-array
    | IFTInt          -- ^ int
    | IFTIntArray     -- ^ int-array
    | IFTLatlon       -- ^ latlon
    | IFTLiteral      -- ^ literal
    | IFTLiteralArray -- ^ literal-array
    | IFTText         -- ^ text
    | IFTTextArray    -- ^ text-array
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable IndexFieldType

instance FromText IndexFieldType where
    parser = match "date"          IFTDate
         <|> match "date-array"    IFTDateArray
         <|> match "double"        IFTDouble
         <|> match "double-array"  IFTDoubleArray
         <|> match "int"           IFTInt
         <|> match "int-array"     IFTIntArray
         <|> match "latlon"        IFTLatlon
         <|> match "literal"       IFTLiteral
         <|> match "literal-array" IFTLiteralArray
         <|> match "text"          IFTText
         <|> match "text-array"    IFTTextArray

instance ToText IndexFieldType where
    toText = \case
        IFTDate         -> "date"
        IFTDateArray    -> "date-array"
        IFTDouble       -> "double"
        IFTDoubleArray  -> "double-array"
        IFTInt          -> "int"
        IFTIntArray     -> "int-array"
        IFTLatlon       -> "latlon"
        IFTLiteral      -> "literal"
        IFTLiteralArray -> "literal-array"
        IFTText         -> "text"
        IFTTextArray    -> "text-array"

instance FromXML IndexFieldType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexFieldType"

instance ToQuery IndexFieldType

data LatLonOptions = LatLonOptions
    { _lloDefaultValue  :: Maybe Text
    , _lloFacetEnabled  :: Maybe Bool
    , _lloReturnEnabled :: Maybe Bool
    , _lloSearchEnabled :: Maybe Bool
    , _lloSortEnabled   :: Maybe Bool
    , _lloSourceField   :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'LatLonOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lloDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'lloFacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'lloReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'lloSearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'lloSortEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'lloSourceField' @::@ 'Maybe' 'Text'
--
latLonOptions :: LatLonOptions
latLonOptions = LatLonOptions
    { _lloDefaultValue  = Nothing
    , _lloSourceField   = Nothing
    , _lloFacetEnabled  = Nothing
    , _lloSearchEnabled = Nothing
    , _lloReturnEnabled = Nothing
    , _lloSortEnabled   = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
lloDefaultValue :: Lens' LatLonOptions (Maybe Text)
lloDefaultValue = lens _lloDefaultValue (\s a -> s { _lloDefaultValue = a })

-- | Whether facet information can be returned for the field.
lloFacetEnabled :: Lens' LatLonOptions (Maybe Bool)
lloFacetEnabled = lens _lloFacetEnabled (\s a -> s { _lloFacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
lloReturnEnabled :: Lens' LatLonOptions (Maybe Bool)
lloReturnEnabled = lens _lloReturnEnabled (\s a -> s { _lloReturnEnabled = a })

-- | Whether the contents of the field are searchable.
lloSearchEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSearchEnabled = lens _lloSearchEnabled (\s a -> s { _lloSearchEnabled = a })

-- | Whether the field can be used to sort the search results.
lloSortEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSortEnabled = lens _lloSortEnabled (\s a -> s { _lloSortEnabled = a })

lloSourceField :: Lens' LatLonOptions (Maybe Text)
lloSourceField = lens _lloSourceField (\s a -> s { _lloSourceField = a })

instance FromXML LatLonOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LatLonOptions"

instance ToQuery LatLonOptions

data SuggesterStatus = SuggesterStatus
    { _ssOptions :: Suggester
    , _ssStatus  :: OptionStatus
    } (Eq, Show, Generic)

-- | 'SuggesterStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssOptions' @::@ 'Suggester'
--
-- * 'ssStatus' @::@ 'OptionStatus'
--
suggesterStatus :: Suggester -- ^ 'ssOptions'
                -> OptionStatus -- ^ 'ssStatus'
                -> SuggesterStatus
suggesterStatus p1 p2 = SuggesterStatus
    { _ssOptions = p1
    , _ssStatus  = p2
    }

ssOptions :: Lens' SuggesterStatus Suggester
ssOptions = lens _ssOptions (\s a -> s { _ssOptions = a })

ssStatus :: Lens' SuggesterStatus OptionStatus
ssStatus = lens _ssStatus (\s a -> s { _ssStatus = a })

instance FromXML SuggesterStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuggesterStatus"

instance ToQuery SuggesterStatus

data OptionStatus = OptionStatus
    { _osCreationDate    :: RFC822
    , _osPendingDeletion :: Maybe Bool
    , _osState           :: Text
    , _osUpdateDate      :: RFC822
    , _osUpdateVersion   :: Maybe Natural
    } (Eq, Ord, Show, Generic)

-- | 'OptionStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osCreationDate' @::@ 'UTCTime'
--
-- * 'osPendingDeletion' @::@ 'Maybe' 'Bool'
--
-- * 'osState' @::@ 'Text'
--
-- * 'osUpdateDate' @::@ 'UTCTime'
--
-- * 'osUpdateVersion' @::@ 'Maybe' 'Natural'
--
optionStatus :: UTCTime -- ^ 'osCreationDate'
             -> UTCTime -- ^ 'osUpdateDate'
             -> Text -- ^ 'osState'
             -> OptionStatus
optionStatus p1 p2 p3 = OptionStatus
    { _osCreationDate    = withIso _Time (const id) p1
    , _osUpdateDate      = withIso _Time (const id) p2
    , _osState           = p3
    , _osUpdateVersion   = Nothing
    , _osPendingDeletion = Nothing
    }

-- | A timestamp for when this option was created.
osCreationDate :: Lens' OptionStatus UTCTime
osCreationDate = lens _osCreationDate (\s a -> s { _osCreationDate = a })
    . _Time

-- | Indicates that the option will be deleted once processing is complete.
osPendingDeletion :: Lens' OptionStatus (Maybe Bool)
osPendingDeletion =
    lens _osPendingDeletion (\s a -> s { _osPendingDeletion = a })

-- | The state of processing a change to an option. Possible values:
-- RequiresIndexDocuments: the option's latest value will not be deployed
-- until IndexDocuments has been called and indexing is complete.
-- Processing: the option's latest value is in the process of being
-- activated. Active: the option's latest value is completely deployed.
-- FailedToValidate: the option value is not compatible with the domain's
-- data and cannot be used to index the data. You must either modify the
-- option value or update or remove the incompatible documents.
osState :: Lens' OptionStatus Text
osState = lens _osState (\s a -> s { _osState = a })

-- | A timestamp for when this option was last updated.
osUpdateDate :: Lens' OptionStatus UTCTime
osUpdateDate = lens _osUpdateDate (\s a -> s { _osUpdateDate = a })
    . _Time

-- | A unique integer that indicates when this option was last updated.
osUpdateVersion :: Lens' OptionStatus (Maybe Natural)
osUpdateVersion = lens _osUpdateVersion (\s a -> s { _osUpdateVersion = a })

instance FromXML OptionStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionStatus"

instance ToQuery OptionStatus

data LiteralArrayOptions = LiteralArrayOptions
    { _laoDefaultValue  :: Maybe Text
    , _laoFacetEnabled  :: Maybe Bool
    , _laoReturnEnabled :: Maybe Bool
    , _laoSearchEnabled :: Maybe Bool
    , _laoSourceFields  :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'LiteralArrayOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laoDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'laoFacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'laoReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'laoSearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'laoSourceFields' @::@ 'Maybe' 'Text'
--
literalArrayOptions :: LiteralArrayOptions
literalArrayOptions = LiteralArrayOptions
    { _laoDefaultValue  = Nothing
    , _laoSourceFields  = Nothing
    , _laoFacetEnabled  = Nothing
    , _laoSearchEnabled = Nothing
    , _laoReturnEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
laoDefaultValue :: Lens' LiteralArrayOptions (Maybe Text)
laoDefaultValue = lens _laoDefaultValue (\s a -> s { _laoDefaultValue = a })

-- | Whether facet information can be returned for the field.
laoFacetEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoFacetEnabled = lens _laoFacetEnabled (\s a -> s { _laoFacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
laoReturnEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoReturnEnabled = lens _laoReturnEnabled (\s a -> s { _laoReturnEnabled = a })

-- | Whether the contents of the field are searchable.
laoSearchEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoSearchEnabled = lens _laoSearchEnabled (\s a -> s { _laoSearchEnabled = a })

-- | A list of source fields to map to the field.
laoSourceFields :: Lens' LiteralArrayOptions (Maybe Text)
laoSourceFields = lens _laoSourceFields (\s a -> s { _laoSourceFields = a })

instance FromXML LiteralArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LiteralArrayOptions"

instance ToQuery LiteralArrayOptions

data IntArrayOptions = IntArrayOptions
    { _iaoDefaultValue  :: Maybe Integer
    , _iaoFacetEnabled  :: Maybe Bool
    , _iaoReturnEnabled :: Maybe Bool
    , _iaoSearchEnabled :: Maybe Bool
    , _iaoSourceFields  :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'IntArrayOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iaoDefaultValue' @::@ 'Maybe' 'Integer'
--
-- * 'iaoFacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'iaoReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'iaoSearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'iaoSourceFields' @::@ 'Maybe' 'Text'
--
intArrayOptions :: IntArrayOptions
intArrayOptions = IntArrayOptions
    { _iaoDefaultValue  = Nothing
    , _iaoSourceFields  = Nothing
    , _iaoFacetEnabled  = Nothing
    , _iaoSearchEnabled = Nothing
    , _iaoReturnEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
iaoDefaultValue :: Lens' IntArrayOptions (Maybe Integer)
iaoDefaultValue = lens _iaoDefaultValue (\s a -> s { _iaoDefaultValue = a })

-- | Whether facet information can be returned for the field.
iaoFacetEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoFacetEnabled = lens _iaoFacetEnabled (\s a -> s { _iaoFacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
iaoReturnEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoReturnEnabled = lens _iaoReturnEnabled (\s a -> s { _iaoReturnEnabled = a })

-- | Whether the contents of the field are searchable.
iaoSearchEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoSearchEnabled = lens _iaoSearchEnabled (\s a -> s { _iaoSearchEnabled = a })

-- | A list of source fields to map to the field.
iaoSourceFields :: Lens' IntArrayOptions (Maybe Text)
iaoSourceFields = lens _iaoSourceFields (\s a -> s { _iaoSourceFields = a })

instance FromXML IntArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IntArrayOptions"

instance ToQuery IntArrayOptions

data Expression = Expression
    { _eExpressionName  :: Text
    , _eExpressionValue :: Text
    } (Eq, Ord, Show, Generic)

-- | 'Expression' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eExpressionName' @::@ 'Text'
--
-- * 'eExpressionValue' @::@ 'Text'
--
expression :: Text -- ^ 'eExpressionName'
           -> Text -- ^ 'eExpressionValue'
           -> Expression
expression p1 p2 = Expression
    { _eExpressionName  = p1
    , _eExpressionValue = p2
    }

eExpressionName :: Lens' Expression Text
eExpressionName = lens _eExpressionName (\s a -> s { _eExpressionName = a })

eExpressionValue :: Lens' Expression Text
eExpressionValue = lens _eExpressionValue (\s a -> s { _eExpressionValue = a })

instance FromXML Expression where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Expression"

instance ToQuery Expression

data SuggesterFuzzyMatching
    = SFMHigh -- ^ high
    | SFMLow  -- ^ low
    | SFMNone -- ^ none
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable SuggesterFuzzyMatching

instance FromText SuggesterFuzzyMatching where
    parser = match "high" SFMHigh
         <|> match "low"  SFMLow
         <|> match "none" SFMNone

instance ToText SuggesterFuzzyMatching where
    toText = \case
        SFMHigh -> "high"
        SFMLow  -> "low"
        SFMNone -> "none"

instance FromXML SuggesterFuzzyMatching where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuggesterFuzzyMatching"

instance ToQuery SuggesterFuzzyMatching

data DateArrayOptions = DateArrayOptions
    { _dao1DefaultValue  :: Maybe Text
    , _dao1FacetEnabled  :: Maybe Bool
    , _dao1ReturnEnabled :: Maybe Bool
    , _dao1SearchEnabled :: Maybe Bool
    , _dao1SourceFields  :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'DateArrayOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dao1DefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'dao1FacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'dao1ReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'dao1SearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'dao1SourceFields' @::@ 'Maybe' 'Text'
--
dateArrayOptions :: DateArrayOptions
dateArrayOptions = DateArrayOptions
    { _dao1DefaultValue  = Nothing
    , _dao1SourceFields  = Nothing
    , _dao1FacetEnabled  = Nothing
    , _dao1SearchEnabled = Nothing
    , _dao1ReturnEnabled = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
dao1DefaultValue :: Lens' DateArrayOptions (Maybe Text)
dao1DefaultValue = lens _dao1DefaultValue (\s a -> s { _dao1DefaultValue = a })

-- | Whether facet information can be returned for the field.
dao1FacetEnabled :: Lens' DateArrayOptions (Maybe Bool)
dao1FacetEnabled = lens _dao1FacetEnabled (\s a -> s { _dao1FacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
dao1ReturnEnabled :: Lens' DateArrayOptions (Maybe Bool)
dao1ReturnEnabled =
    lens _dao1ReturnEnabled (\s a -> s { _dao1ReturnEnabled = a })

-- | Whether the contents of the field are searchable.
dao1SearchEnabled :: Lens' DateArrayOptions (Maybe Bool)
dao1SearchEnabled =
    lens _dao1SearchEnabled (\s a -> s { _dao1SearchEnabled = a })

-- | A list of source fields to map to the field.
dao1SourceFields :: Lens' DateArrayOptions (Maybe Text)
dao1SourceFields = lens _dao1SourceFields (\s a -> s { _dao1SourceFields = a })

instance FromXML DateArrayOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DateArrayOptions"

instance ToQuery DateArrayOptions

data AnalysisSchemeLanguage
    = Ar     -- ^ ar
    | Bg     -- ^ bg
    | Ca     -- ^ ca
    | Cs     -- ^ cs
    | Da     -- ^ da
    | De     -- ^ de
    | El     -- ^ el
    | En     -- ^ en
    | Es     -- ^ es
    | Eu     -- ^ eu
    | Fa     -- ^ fa
    | Fi     -- ^ fi
    | Fr     -- ^ fr
    | Ga     -- ^ ga
    | Gl     -- ^ gl
    | He     -- ^ he
    | Hi     -- ^ hi
    | Hu     -- ^ hu
    | Hy     -- ^ hy
    | Id     -- ^ id
    | It     -- ^ it
    | Ja     -- ^ ja
    | Ko     -- ^ ko
    | Lv     -- ^ lv
    | Mul    -- ^ mul
    | Nl     -- ^ nl
    | No     -- ^ no
    | Pt     -- ^ pt
    | Ro     -- ^ ro
    | Ru     -- ^ ru
    | Sv     -- ^ sv
    | Th     -- ^ th
    | Tr     -- ^ tr
    | ZhHans -- ^ zh-Hans
    | ZhHant -- ^ zh-Hant
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable AnalysisSchemeLanguage

instance FromText AnalysisSchemeLanguage where
    parser = match "ar"      Ar
         <|> match "bg"      Bg
         <|> match "ca"      Ca
         <|> match "cs"      Cs
         <|> match "da"      Da
         <|> match "de"      De
         <|> match "el"      El
         <|> match "en"      En
         <|> match "es"      Es
         <|> match "eu"      Eu
         <|> match "fa"      Fa
         <|> match "fi"      Fi
         <|> match "fr"      Fr
         <|> match "ga"      Ga
         <|> match "gl"      Gl
         <|> match "he"      He
         <|> match "hi"      Hi
         <|> match "hu"      Hu
         <|> match "hy"      Hy
         <|> match "id"      Id
         <|> match "it"      It
         <|> match "ja"      Ja
         <|> match "ko"      Ko
         <|> match "lv"      Lv
         <|> match "mul"     Mul
         <|> match "nl"      Nl
         <|> match "no"      No
         <|> match "pt"      Pt
         <|> match "ro"      Ro
         <|> match "ru"      Ru
         <|> match "sv"      Sv
         <|> match "th"      Th
         <|> match "tr"      Tr
         <|> match "zh-Hans" ZhHans
         <|> match "zh-Hant" ZhHant

instance ToText AnalysisSchemeLanguage where
    toText = \case
        Ar     -> "ar"
        Bg     -> "bg"
        Ca     -> "ca"
        Cs     -> "cs"
        Da     -> "da"
        De     -> "de"
        El     -> "el"
        En     -> "en"
        Es     -> "es"
        Eu     -> "eu"
        Fa     -> "fa"
        Fi     -> "fi"
        Fr     -> "fr"
        Ga     -> "ga"
        Gl     -> "gl"
        He     -> "he"
        Hi     -> "hi"
        Hu     -> "hu"
        Hy     -> "hy"
        Id     -> "id"
        It     -> "it"
        Ja     -> "ja"
        Ko     -> "ko"
        Lv     -> "lv"
        Mul    -> "mul"
        Nl     -> "nl"
        No     -> "no"
        Pt     -> "pt"
        Ro     -> "ro"
        Ru     -> "ru"
        Sv     -> "sv"
        Th     -> "th"
        Tr     -> "tr"
        ZhHans -> "zh-Hans"
        ZhHant -> "zh-Hant"

instance FromXML AnalysisSchemeLanguage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AnalysisSchemeLanguage"

instance ToQuery AnalysisSchemeLanguage

data PartitionInstanceType
    = SearchM1Large   -- ^ search.m1.large
    | SearchM1Small   -- ^ search.m1.small
    | SearchM22xlarge -- ^ search.m2.2xlarge
    | SearchM2Xlarge  -- ^ search.m2.xlarge
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable PartitionInstanceType

instance FromText PartitionInstanceType where
    parser = match "search.m1.large"   SearchM1Large
         <|> match "search.m1.small"   SearchM1Small
         <|> match "search.m2.2xlarge" SearchM22xlarge
         <|> match "search.m2.xlarge"  SearchM2Xlarge

instance ToText PartitionInstanceType where
    toText = \case
        SearchM1Large   -> "search.m1.large"
        SearchM1Small   -> "search.m1.small"
        SearchM22xlarge -> "search.m2.2xlarge"
        SearchM2Xlarge  -> "search.m2.xlarge"

instance FromXML PartitionInstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PartitionInstanceType"

instance ToQuery PartitionInstanceType

data Suggester = Suggester
    { _sDocumentSuggesterOptions :: DocumentSuggesterOptions
    , _sSuggesterName            :: Text
    } (Eq, Show, Generic)

-- | 'Suggester' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sDocumentSuggesterOptions' @::@ 'DocumentSuggesterOptions'
--
-- * 'sSuggesterName' @::@ 'Text'
--
suggester :: Text -- ^ 'sSuggesterName'
          -> DocumentSuggesterOptions -- ^ 'sDocumentSuggesterOptions'
          -> Suggester
suggester p1 p2 = Suggester
    { _sSuggesterName            = p1
    , _sDocumentSuggesterOptions = p2
    }

sDocumentSuggesterOptions :: Lens' Suggester DocumentSuggesterOptions
sDocumentSuggesterOptions =
    lens _sDocumentSuggesterOptions
        (\s a -> s { _sDocumentSuggesterOptions = a })

sSuggesterName :: Lens' Suggester Text
sSuggesterName = lens _sSuggesterName (\s a -> s { _sSuggesterName = a })

instance FromXML Suggester where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Suggester"

instance ToQuery Suggester

data IntOptions = IntOptions
    { _ioDefaultValue  :: Maybe Integer
    , _ioFacetEnabled  :: Maybe Bool
    , _ioReturnEnabled :: Maybe Bool
    , _ioSearchEnabled :: Maybe Bool
    , _ioSortEnabled   :: Maybe Bool
    , _ioSourceField   :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'IntOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ioDefaultValue' @::@ 'Maybe' 'Integer'
--
-- * 'ioFacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'ioReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'ioSearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'ioSortEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'ioSourceField' @::@ 'Maybe' 'Text'
--
intOptions :: IntOptions
intOptions = IntOptions
    { _ioDefaultValue  = Nothing
    , _ioSourceField   = Nothing
    , _ioFacetEnabled  = Nothing
    , _ioSearchEnabled = Nothing
    , _ioReturnEnabled = Nothing
    , _ioSortEnabled   = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
-- This can be important if you are using the field in an expression and
-- that field is not present in every document.
ioDefaultValue :: Lens' IntOptions (Maybe Integer)
ioDefaultValue = lens _ioDefaultValue (\s a -> s { _ioDefaultValue = a })

-- | Whether facet information can be returned for the field.
ioFacetEnabled :: Lens' IntOptions (Maybe Bool)
ioFacetEnabled = lens _ioFacetEnabled (\s a -> s { _ioFacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
ioReturnEnabled :: Lens' IntOptions (Maybe Bool)
ioReturnEnabled = lens _ioReturnEnabled (\s a -> s { _ioReturnEnabled = a })

-- | Whether the contents of the field are searchable.
ioSearchEnabled :: Lens' IntOptions (Maybe Bool)
ioSearchEnabled = lens _ioSearchEnabled (\s a -> s { _ioSearchEnabled = a })

-- | Whether the field can be used to sort the search results.
ioSortEnabled :: Lens' IntOptions (Maybe Bool)
ioSortEnabled = lens _ioSortEnabled (\s a -> s { _ioSortEnabled = a })

-- | The name of the source field to map to the field.
ioSourceField :: Lens' IntOptions (Maybe Text)
ioSourceField = lens _ioSourceField (\s a -> s { _ioSourceField = a })

instance FromXML IntOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IntOptions"

instance ToQuery IntOptions

data LiteralOptions = LiteralOptions
    { _loDefaultValue  :: Maybe Text
    , _loFacetEnabled  :: Maybe Bool
    , _loReturnEnabled :: Maybe Bool
    , _loSearchEnabled :: Maybe Bool
    , _loSortEnabled   :: Maybe Bool
    , _loSourceField   :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'LiteralOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'loFacetEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'loReturnEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'loSearchEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'loSortEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'loSourceField' @::@ 'Maybe' 'Text'
--
literalOptions :: LiteralOptions
literalOptions = LiteralOptions
    { _loDefaultValue  = Nothing
    , _loSourceField   = Nothing
    , _loFacetEnabled  = Nothing
    , _loSearchEnabled = Nothing
    , _loReturnEnabled = Nothing
    , _loSortEnabled   = Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
loDefaultValue :: Lens' LiteralOptions (Maybe Text)
loDefaultValue = lens _loDefaultValue (\s a -> s { _loDefaultValue = a })

-- | Whether facet information can be returned for the field.
loFacetEnabled :: Lens' LiteralOptions (Maybe Bool)
loFacetEnabled = lens _loFacetEnabled (\s a -> s { _loFacetEnabled = a })

-- | Whether the contents of the field can be returned in the search results.
loReturnEnabled :: Lens' LiteralOptions (Maybe Bool)
loReturnEnabled = lens _loReturnEnabled (\s a -> s { _loReturnEnabled = a })

-- | Whether the contents of the field are searchable.
loSearchEnabled :: Lens' LiteralOptions (Maybe Bool)
loSearchEnabled = lens _loSearchEnabled (\s a -> s { _loSearchEnabled = a })

-- | Whether the field can be used to sort the search results.
loSortEnabled :: Lens' LiteralOptions (Maybe Bool)
loSortEnabled = lens _loSortEnabled (\s a -> s { _loSortEnabled = a })

loSourceField :: Lens' LiteralOptions (Maybe Text)
loSourceField = lens _loSourceField (\s a -> s { _loSourceField = a })

instance FromXML LiteralOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LiteralOptions"

instance ToQuery LiteralOptions

data AccessPoliciesStatus = AccessPoliciesStatus
    { _apsOptions :: Text
    , _apsStatus  :: OptionStatus
    } (Eq, Show, Generic)

-- | 'AccessPoliciesStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apsOptions' @::@ 'Text'
--
-- * 'apsStatus' @::@ 'OptionStatus'
--
accessPoliciesStatus :: Text -- ^ 'apsOptions'
                     -> OptionStatus -- ^ 'apsStatus'
                     -> AccessPoliciesStatus
accessPoliciesStatus p1 p2 = AccessPoliciesStatus
    { _apsOptions = p1
    , _apsStatus  = p2
    }

apsOptions :: Lens' AccessPoliciesStatus Text
apsOptions = lens _apsOptions (\s a -> s { _apsOptions = a })

apsStatus :: Lens' AccessPoliciesStatus OptionStatus
apsStatus = lens _apsStatus (\s a -> s { _apsStatus = a })

instance FromXML AccessPoliciesStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessPoliciesStatus"

instance ToQuery AccessPoliciesStatus
