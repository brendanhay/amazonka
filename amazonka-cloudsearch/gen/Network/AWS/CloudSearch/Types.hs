{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.CloudSearch.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CloudSearch.Types
    (
    -- * Service
      CloudSearch

    -- * Errors
    , _BaseException
    , _DisabledOperationException
    , _InternalException
    , _InvalidTypeException
    , _ResourceNotFoundException
    , _LimitExceededException

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

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- * AnalysisOptions
    , AnalysisOptions
    , analysisOptions
    , aoAlgorithmicStemming
    , aoStopwords
    , aoStemmingDictionary
    , aoSynonyms
    , aoJapaneseTokenizationDictionary

    -- * AnalysisScheme
    , AnalysisScheme
    , analysisScheme
    , asAnalysisOptions
    , asAnalysisSchemeName
    , asAnalysisSchemeLanguage

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
    , datSourceFields
    , datReturnEnabled
    , datFacetEnabled
    , datSearchEnabled
    , datDefaultValue

    -- * DateOptions
    , DateOptions
    , dateOptions
    , doSourceField
    , doReturnEnabled
    , doFacetEnabled
    , doSearchEnabled
    , doSortEnabled
    , doDefaultValue

    -- * DocumentSuggesterOptions
    , DocumentSuggesterOptions
    , documentSuggesterOptions
    , dsoSortExpression
    , dsoFuzzyMatching
    , dsoSourceField

    -- * DomainStatus
    , DomainStatus
    , domainStatus
    , dsSearchInstanceCount
    , dsSearchInstanceType
    , dsARN
    , dsDocService
    , dsCreated
    , dsSearchService
    , dsLimits
    , dsSearchPartitionCount
    , dsDeleted
    , dsProcessing
    , dsDomainId
    , dsDomainName
    , dsRequiresIndexDocuments

    -- * DoubleArrayOptions
    , DoubleArrayOptions
    , doubleArrayOptions
    , daoSourceFields
    , daoReturnEnabled
    , daoFacetEnabled
    , daoSearchEnabled
    , daoDefaultValue

    -- * DoubleOptions
    , DoubleOptions
    , doubleOptions
    , douSourceField
    , douReturnEnabled
    , douFacetEnabled
    , douSearchEnabled
    , douSortEnabled
    , douDefaultValue

    -- * Expression
    , Expression
    , expression
    , expExpressionName
    , expExpressionValue

    -- * ExpressionStatus
    , ExpressionStatus
    , expressionStatus
    , esOptions
    , esStatus

    -- * IndexField
    , IndexField
    , indexField
    , ifDateOptions
    , ifTextArrayOptions
    , ifDoubleArrayOptions
    , ifDoubleOptions
    , ifTextOptions
    , ifLatLonOptions
    , ifIntArrayOptions
    , ifLiteralArrayOptions
    , ifDateArrayOptions
    , ifLiteralOptions
    , ifIntOptions
    , ifIndexFieldName
    , ifIndexFieldType

    -- * IndexFieldStatus
    , IndexFieldStatus
    , indexFieldStatus
    , ifsOptions
    , ifsStatus

    -- * IntArrayOptions
    , IntArrayOptions
    , intArrayOptions
    , iaoSourceFields
    , iaoReturnEnabled
    , iaoFacetEnabled
    , iaoSearchEnabled
    , iaoDefaultValue

    -- * IntOptions
    , IntOptions
    , intOptions
    , ioSourceField
    , ioReturnEnabled
    , ioFacetEnabled
    , ioSearchEnabled
    , ioSortEnabled
    , ioDefaultValue

    -- * LatLonOptions
    , LatLonOptions
    , latLonOptions
    , lloSourceField
    , lloReturnEnabled
    , lloFacetEnabled
    , lloSearchEnabled
    , lloSortEnabled
    , lloDefaultValue

    -- * Limits
    , Limits
    , limits
    , limMaximumReplicationCount
    , limMaximumPartitionCount

    -- * LiteralArrayOptions
    , LiteralArrayOptions
    , literalArrayOptions
    , laoSourceFields
    , laoReturnEnabled
    , laoFacetEnabled
    , laoSearchEnabled
    , laoDefaultValue

    -- * LiteralOptions
    , LiteralOptions
    , literalOptions
    , loSourceField
    , loReturnEnabled
    , loFacetEnabled
    , loSearchEnabled
    , loSortEnabled
    , loDefaultValue

    -- * OptionStatus
    , OptionStatus
    , optionStatus
    , osPendingDeletion
    , osUpdateVersion
    , osCreationDate
    , osUpdateDate
    , osState

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

    -- * ServiceEndpoint
    , ServiceEndpoint
    , serviceEndpoint
    , seEndpoint

    -- * Suggester
    , Suggester
    , suggester
    , sugSuggesterName
    , sugDocumentSuggesterOptions

    -- * SuggesterStatus
    , SuggesterStatus
    , suggesterStatus
    , ssOptions
    , ssStatus

    -- * TextArrayOptions
    , TextArrayOptions
    , textArrayOptions
    , taoSourceFields
    , taoReturnEnabled
    , taoAnalysisScheme
    , taoHighlightEnabled
    , taoDefaultValue

    -- * TextOptions
    , TextOptions
    , textOptions
    , toSourceField
    , toReturnEnabled
    , toAnalysisScheme
    , toHighlightEnabled
    , toSortEnabled
    , toDefaultValue

    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2013-01-01@ of the Amazon CloudSearch SDK.
data CloudSearch

instance AWSService CloudSearch where
    type Sg CloudSearch = V4

    service = const svc
      where
        svc :: Service CloudSearch
        svc = Service
            { _svcAbbrev   = "CloudSearch"
            , _svcPrefix   = "cloudsearch"
            , _svcVersion  = "2013-01-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout  = 80000000
            , _svcStatus   = statusSuccess
            , _svcError    = parseXMLError
            , _svcRetry    = retry
            }

        retry :: Retry
        retry = Exponential
            { _retryBase     = 0
            , _retryGrowth   = 0
            , _retryAttempts = 0
            , _retryCheck    = check
            }

        check :: ServiceError -> Bool
        check ServiceError'{..} = error "FIXME: Retry check not implemented."

-- | An error occurred while processing the request.
_BaseException :: AWSError a => Geting (First ServiceError) a ServiceError
_BaseException = _ServiceError . hasCode "BaseException";

-- | The request was rejected because it attempted an operation which is not
-- enabled.
_DisabledOperationException :: AWSError a => Geting (First ServiceError) a ServiceError
_DisabledOperationException = _ServiceError . hasCode "DisabledAction" . hasStatus 409;

-- | An internal error occurred while processing the request. If this problem
-- persists, report an issue from the
-- <http://status.aws.amazon.com/ Service Health Dashboard>.
_InternalException :: AWSError a => Geting (First ServiceError) a ServiceError
_InternalException = _ServiceError . hasCode "InternalException" . hasStatus 500;

-- | The request was rejected because it specified an invalid type
-- definition.
_InvalidTypeException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidTypeException = _ServiceError . hasCode "InvalidType" . hasStatus 409;

-- | The request was rejected because it attempted to reference a resource
-- that does not exist.
_ResourceNotFoundException :: AWSError a => Geting (First ServiceError) a ServiceError
_ResourceNotFoundException = _ServiceError . hasCode "ResourceNotFound" . hasStatus 409;

-- | The request was rejected because a resource limit has already been met.
_LimitExceededException :: AWSError a => Geting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceeded" . hasStatus 409;

data AlgorithmicStemming = ASLight | ASNone | ASMinimal | ASFull deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText AlgorithmicStemming where
    parser = takeLowerText >>= \case
        "full" -> pure ASFull
        "light" -> pure ASLight
        "minimal" -> pure ASMinimal
        "none" -> pure ASNone
        e -> fail ("Failure parsing AlgorithmicStemming from " ++ show e)

instance ToText AlgorithmicStemming where
    toText = \case
        ASFull -> "full"
        ASLight -> "light"
        ASMinimal -> "minimal"
        ASNone -> "none"

instance Hashable AlgorithmicStemming
instance ToQuery AlgorithmicStemming
instance ToHeader AlgorithmicStemming

instance FromXML AlgorithmicStemming where
    parseXML = parseXMLText "AlgorithmicStemming"

-- | An <http://tools.ietf.org/html/rfc4646 IETF RFC 4646> language code or
-- @mul@ for multiple languages.
data AnalysisSchemeLanguage = Mul | KO | TH | RU | FA | LV | CA | RO | SV | HE | HU | AR | JA | GA | ES | DE | HI | EL | HY | ZHHant | NO | PT | FR | FI | NL | BG | ID | IT | DA | EU | CS | GL | EN | TR | ZHHans deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText AnalysisSchemeLanguage where
    parser = takeLowerText >>= \case
        "ar" -> pure AR
        "bg" -> pure BG
        "ca" -> pure CA
        "cs" -> pure CS
        "da" -> pure DA
        "de" -> pure DE
        "el" -> pure EL
        "en" -> pure EN
        "es" -> pure ES
        "eu" -> pure EU
        "fa" -> pure FA
        "fi" -> pure FI
        "fr" -> pure FR
        "ga" -> pure GA
        "gl" -> pure GL
        "he" -> pure HE
        "hi" -> pure HI
        "hu" -> pure HU
        "hy" -> pure HY
        "id" -> pure ID
        "it" -> pure IT
        "ja" -> pure JA
        "ko" -> pure KO
        "lv" -> pure LV
        "mul" -> pure Mul
        "nl" -> pure NL
        "no" -> pure NO
        "pt" -> pure PT
        "ro" -> pure RO
        "ru" -> pure RU
        "sv" -> pure SV
        "th" -> pure TH
        "tr" -> pure TR
        "zh-Hans" -> pure ZHHans
        "zh-Hant" -> pure ZHHant
        e -> fail ("Failure parsing AnalysisSchemeLanguage from " ++ show e)

instance ToText AnalysisSchemeLanguage where
    toText = \case
        AR -> "ar"
        BG -> "bg"
        CA -> "ca"
        CS -> "cs"
        DA -> "da"
        DE -> "de"
        EL -> "el"
        EN -> "en"
        ES -> "es"
        EU -> "eu"
        FA -> "fa"
        FI -> "fi"
        FR -> "fr"
        GA -> "ga"
        GL -> "gl"
        HE -> "he"
        HI -> "hi"
        HU -> "hu"
        HY -> "hy"
        ID -> "id"
        IT -> "it"
        JA -> "ja"
        KO -> "ko"
        LV -> "lv"
        Mul -> "mul"
        NL -> "nl"
        NO -> "no"
        PT -> "pt"
        RO -> "ro"
        RU -> "ru"
        SV -> "sv"
        TH -> "th"
        TR -> "tr"
        ZHHans -> "zh-Hans"
        ZHHant -> "zh-Hant"

instance Hashable AnalysisSchemeLanguage
instance ToQuery AnalysisSchemeLanguage
instance ToHeader AnalysisSchemeLanguage

instance FromXML AnalysisSchemeLanguage where
    parseXML = parseXMLText "AnalysisSchemeLanguage"

-- | The type of field. The valid options for a field depend on the field
-- type. For more information about the supported field types, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
data IndexFieldType = Latlon | IntArray | LiteralArray | Double | Text | DoubleArray | Date | TextArray | DateArray | Int | Literal deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText IndexFieldType where
    parser = takeLowerText >>= \case
        "date" -> pure Date
        "date-array" -> pure DateArray
        "double" -> pure Double
        "double-array" -> pure DoubleArray
        "int" -> pure Int
        "int-array" -> pure IntArray
        "latlon" -> pure Latlon
        "literal" -> pure Literal
        "literal-array" -> pure LiteralArray
        "text" -> pure Text
        "text-array" -> pure TextArray
        e -> fail ("Failure parsing IndexFieldType from " ++ show e)

instance ToText IndexFieldType where
    toText = \case
        Date -> "date"
        DateArray -> "date-array"
        Double -> "double"
        DoubleArray -> "double-array"
        Int -> "int"
        IntArray -> "int-array"
        Latlon -> "latlon"
        Literal -> "literal"
        LiteralArray -> "literal-array"
        Text -> "text"
        TextArray -> "text-array"

instance Hashable IndexFieldType
instance ToQuery IndexFieldType
instance ToHeader IndexFieldType

instance FromXML IndexFieldType where
    parseXML = parseXMLText "IndexFieldType"

-- | The state of processing a change to an option. One of:
--
-- -   RequiresIndexDocuments: The option\'s latest value will not be
--     deployed until IndexDocuments has been called and indexing is
--     complete.
-- -   Processing: The option\'s latest value is in the process of being
--     activated.
-- -   Active: The option\'s latest value is fully deployed.
-- -   FailedToValidate: The option value is not compatible with the
--     domain\'s data and cannot be used to index the data. You must either
--     modify the option value or update or remove the incompatible
--     documents.
data OptionState = FailedToValidate | Active | RequiresIndexDocuments | Processing deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText OptionState where
    parser = takeLowerText >>= \case
        "Active" -> pure Active
        "FailedToValidate" -> pure FailedToValidate
        "Processing" -> pure Processing
        "RequiresIndexDocuments" -> pure RequiresIndexDocuments
        e -> fail ("Failure parsing OptionState from " ++ show e)

instance ToText OptionState where
    toText = \case
        Active -> "Active"
        FailedToValidate -> "FailedToValidate"
        Processing -> "Processing"
        RequiresIndexDocuments -> "RequiresIndexDocuments"

instance Hashable OptionState
instance ToQuery OptionState
instance ToHeader OptionState

instance FromXML OptionState where
    parseXML = parseXMLText "OptionState"

-- | The instance type (such as @search.m1.small@) on which an index
-- partition is hosted.
data PartitionInstanceType = SearchM32XLarge | SearchM3Large | SearchM3Medium | SearchM22XLarge | SearchM2XLarge | SearchM1Large | SearchM1Small | SearchM3XLarge deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText PartitionInstanceType where
    parser = takeLowerText >>= \case
        "search.m1.large" -> pure SearchM1Large
        "search.m1.small" -> pure SearchM1Small
        "search.m2.2xlarge" -> pure SearchM22XLarge
        "search.m2.xlarge" -> pure SearchM2XLarge
        "search.m3.2xlarge" -> pure SearchM32XLarge
        "search.m3.large" -> pure SearchM3Large
        "search.m3.medium" -> pure SearchM3Medium
        "search.m3.xlarge" -> pure SearchM3XLarge
        e -> fail ("Failure parsing PartitionInstanceType from " ++ show e)

instance ToText PartitionInstanceType where
    toText = \case
        SearchM1Large -> "search.m1.large"
        SearchM1Small -> "search.m1.small"
        SearchM22XLarge -> "search.m2.2xlarge"
        SearchM2XLarge -> "search.m2.xlarge"
        SearchM32XLarge -> "search.m3.2xlarge"
        SearchM3Large -> "search.m3.large"
        SearchM3Medium -> "search.m3.medium"
        SearchM3XLarge -> "search.m3.xlarge"

instance Hashable PartitionInstanceType
instance ToQuery PartitionInstanceType
instance ToHeader PartitionInstanceType

instance FromXML PartitionInstanceType where
    parseXML = parseXMLText "PartitionInstanceType"

data SuggesterFuzzyMatching = Low | None | High deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SuggesterFuzzyMatching where
    parser = takeLowerText >>= \case
        "high" -> pure High
        "low" -> pure Low
        "none" -> pure None
        e -> fail ("Failure parsing SuggesterFuzzyMatching from " ++ show e)

instance ToText SuggesterFuzzyMatching where
    toText = \case
        High -> "high"
        Low -> "low"
        None -> "none"

instance Hashable SuggesterFuzzyMatching
instance ToQuery SuggesterFuzzyMatching
instance ToHeader SuggesterFuzzyMatching

instance FromXML SuggesterFuzzyMatching where
    parseXML = parseXMLText "SuggesterFuzzyMatching"

-- | The configured access rules for the domain\'s document and search
-- endpoints, and the current status of those rules.
--
-- /See:/ 'accessPoliciesStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apsOptions'
--
-- * 'apsStatus'
data AccessPoliciesStatus = AccessPoliciesStatus'{_apsOptions :: Text, _apsStatus :: OptionStatus} deriving (Eq, Read, Show)

-- | 'AccessPoliciesStatus' smart constructor.
accessPoliciesStatus :: Text -> OptionStatus -> AccessPoliciesStatus
accessPoliciesStatus pOptions pStatus = AccessPoliciesStatus'{_apsOptions = pOptions, _apsStatus = pStatus};

-- | FIXME: Undocumented member.
apsOptions :: Lens' AccessPoliciesStatus Text
apsOptions = lens _apsOptions (\ s a -> s{_apsOptions = a});

-- | FIXME: Undocumented member.
apsStatus :: Lens' AccessPoliciesStatus OptionStatus
apsStatus = lens _apsStatus (\ s a -> s{_apsStatus = a});

instance FromXML AccessPoliciesStatus where
        parseXML x
          = AccessPoliciesStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

-- | Synonyms, stopwords, and stemming options for an analysis scheme.
-- Includes tokenization dictionary for Japanese.
--
-- /See:/ 'analysisOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aoAlgorithmicStemming'
--
-- * 'aoStopwords'
--
-- * 'aoStemmingDictionary'
--
-- * 'aoSynonyms'
--
-- * 'aoJapaneseTokenizationDictionary'
data AnalysisOptions = AnalysisOptions'{_aoAlgorithmicStemming :: Maybe AlgorithmicStemming, _aoStopwords :: Maybe Text, _aoStemmingDictionary :: Maybe Text, _aoSynonyms :: Maybe Text, _aoJapaneseTokenizationDictionary :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AnalysisOptions' smart constructor.
analysisOptions :: AnalysisOptions
analysisOptions = AnalysisOptions'{_aoAlgorithmicStemming = Nothing, _aoStopwords = Nothing, _aoStemmingDictionary = Nothing, _aoSynonyms = Nothing, _aoJapaneseTokenizationDictionary = Nothing};

-- | The level of algorithmic stemming to perform: @none@, @minimal@,
-- @light@, or @full@. The available levels vary depending on the language.
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings>
-- in the /Amazon CloudSearch Developer Guide/
aoAlgorithmicStemming :: Lens' AnalysisOptions (Maybe AlgorithmicStemming)
aoAlgorithmicStemming = lens _aoAlgorithmicStemming (\ s a -> s{_aoAlgorithmicStemming = a});

-- | A JSON array of terms to ignore during indexing and searching. For
-- example, @[\"a\", \"an\", \"the\", \"of\"]@. The stopwords dictionary
-- must explicitly list each word you want to ignore. Wildcards and regular
-- expressions are not supported.
aoStopwords :: Lens' AnalysisOptions (Maybe Text)
aoStopwords = lens _aoStopwords (\ s a -> s{_aoStopwords = a});

-- | A JSON object that contains a collection of string:value pairs that each
-- map a term to its stem. For example,
-- @{\"term1\": \"stem1\", \"term2\": \"stem2\", \"term3\": \"stem3\"}@.
-- The stemming dictionary is applied in addition to any algorithmic
-- stemming. This enables you to override the results of the algorithmic
-- stemming to correct specific cases of overstemming or understemming. The
-- maximum size of a stemming dictionary is 500 KB.
aoStemmingDictionary :: Lens' AnalysisOptions (Maybe Text)
aoStemmingDictionary = lens _aoStemmingDictionary (\ s a -> s{_aoStemmingDictionary = a});

-- | A JSON object that defines synonym groups and aliases. A synonym group
-- is an array of arrays, where each sub-array is a group of terms where
-- each term in the group is considered a synonym of every other term in
-- the group. The aliases value is an object that contains a collection of
-- string:value pairs where the string specifies a term and the array of
-- values specifies each of the aliases for that term. An alias is
-- considered a synonym of the specified term, but the term is not
-- considered a synonym of the alias. For more information about specifying
-- synonyms, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms>
-- in the /Amazon CloudSearch Developer Guide/.
aoSynonyms :: Lens' AnalysisOptions (Maybe Text)
aoSynonyms = lens _aoSynonyms (\ s a -> s{_aoSynonyms = a});

-- | A JSON array that contains a collection of terms, tokens, readings and
-- part of speech for Japanese Tokenizaiton. The Japanese tokenization
-- dictionary enables you to override the default tokenization for selected
-- terms. This is only valid for Japanese language fields.
aoJapaneseTokenizationDictionary :: Lens' AnalysisOptions (Maybe Text)
aoJapaneseTokenizationDictionary = lens _aoJapaneseTokenizationDictionary (\ s a -> s{_aoJapaneseTokenizationDictionary = a});

instance FromXML AnalysisOptions where
        parseXML x
          = AnalysisOptions' <$>
              (x .@? "AlgorithmicStemming") <*> (x .@? "Stopwords")
                <*> (x .@? "StemmingDictionary")
                <*> (x .@? "Synonyms")
                <*> (x .@? "JapaneseTokenizationDictionary")

instance ToQuery AnalysisOptions where
        toQuery AnalysisOptions'{..}
          = mconcat
              ["AlgorithmicStemming" =: _aoAlgorithmicStemming,
               "Stopwords" =: _aoStopwords,
               "StemmingDictionary" =: _aoStemmingDictionary,
               "Synonyms" =: _aoSynonyms,
               "JapaneseTokenizationDictionary" =:
                 _aoJapaneseTokenizationDictionary]

-- | Configuration information for an analysis scheme. Each analysis scheme
-- has a unique name and specifies the language of the text to be
-- processed. The following options can be configured for an analysis
-- scheme: @Synonyms@, @Stopwords@, @StemmingDictionary@,
-- @JapaneseTokenizationDictionary@ and @AlgorithmicStemming@.
--
-- /See:/ 'analysisScheme' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asAnalysisOptions'
--
-- * 'asAnalysisSchemeName'
--
-- * 'asAnalysisSchemeLanguage'
data AnalysisScheme = AnalysisScheme'{_asAnalysisOptions :: Maybe AnalysisOptions, _asAnalysisSchemeName :: Text, _asAnalysisSchemeLanguage :: AnalysisSchemeLanguage} deriving (Eq, Read, Show)

-- | 'AnalysisScheme' smart constructor.
analysisScheme :: Text -> AnalysisSchemeLanguage -> AnalysisScheme
analysisScheme pAnalysisSchemeName pAnalysisSchemeLanguage = AnalysisScheme'{_asAnalysisOptions = Nothing, _asAnalysisSchemeName = pAnalysisSchemeName, _asAnalysisSchemeLanguage = pAnalysisSchemeLanguage};

-- | FIXME: Undocumented member.
asAnalysisOptions :: Lens' AnalysisScheme (Maybe AnalysisOptions)
asAnalysisOptions = lens _asAnalysisOptions (\ s a -> s{_asAnalysisOptions = a});

-- | FIXME: Undocumented member.
asAnalysisSchemeName :: Lens' AnalysisScheme Text
asAnalysisSchemeName = lens _asAnalysisSchemeName (\ s a -> s{_asAnalysisSchemeName = a});

-- | FIXME: Undocumented member.
asAnalysisSchemeLanguage :: Lens' AnalysisScheme AnalysisSchemeLanguage
asAnalysisSchemeLanguage = lens _asAnalysisSchemeLanguage (\ s a -> s{_asAnalysisSchemeLanguage = a});

instance FromXML AnalysisScheme where
        parseXML x
          = AnalysisScheme' <$>
              (x .@? "AnalysisOptions") <*>
                (x .@ "AnalysisSchemeName")
                <*> (x .@ "AnalysisSchemeLanguage")

instance ToQuery AnalysisScheme where
        toQuery AnalysisScheme'{..}
          = mconcat
              ["AnalysisOptions" =: _asAnalysisOptions,
               "AnalysisSchemeName" =: _asAnalysisSchemeName,
               "AnalysisSchemeLanguage" =:
                 _asAnalysisSchemeLanguage]

-- | The status and configuration of an @AnalysisScheme@.
--
-- /See:/ 'analysisSchemeStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'assOptions'
--
-- * 'assStatus'
data AnalysisSchemeStatus = AnalysisSchemeStatus'{_assOptions :: AnalysisScheme, _assStatus :: OptionStatus} deriving (Eq, Read, Show)

-- | 'AnalysisSchemeStatus' smart constructor.
analysisSchemeStatus :: AnalysisScheme -> OptionStatus -> AnalysisSchemeStatus
analysisSchemeStatus pOptions pStatus = AnalysisSchemeStatus'{_assOptions = pOptions, _assStatus = pStatus};

-- | FIXME: Undocumented member.
assOptions :: Lens' AnalysisSchemeStatus AnalysisScheme
assOptions = lens _assOptions (\ s a -> s{_assOptions = a});

-- | FIXME: Undocumented member.
assStatus :: Lens' AnalysisSchemeStatus OptionStatus
assStatus = lens _assStatus (\ s a -> s{_assStatus = a});

instance FromXML AnalysisSchemeStatus where
        parseXML x
          = AnalysisSchemeStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

-- | The status and configuration of the domain\'s availability options.
--
-- /See:/ 'availabilityOptionsStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aosOptions'
--
-- * 'aosStatus'
data AvailabilityOptionsStatus = AvailabilityOptionsStatus'{_aosOptions :: Bool, _aosStatus :: OptionStatus} deriving (Eq, Read, Show)

-- | 'AvailabilityOptionsStatus' smart constructor.
availabilityOptionsStatus :: Bool -> OptionStatus -> AvailabilityOptionsStatus
availabilityOptionsStatus pOptions pStatus = AvailabilityOptionsStatus'{_aosOptions = pOptions, _aosStatus = pStatus};

-- | The availability options configured for the domain.
aosOptions :: Lens' AvailabilityOptionsStatus Bool
aosOptions = lens _aosOptions (\ s a -> s{_aosOptions = a});

-- | FIXME: Undocumented member.
aosStatus :: Lens' AvailabilityOptionsStatus OptionStatus
aosStatus = lens _aosStatus (\ s a -> s{_aosStatus = a});

instance FromXML AvailabilityOptionsStatus where
        parseXML x
          = AvailabilityOptionsStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

-- | Options for a field that contains an array of dates. Present if
-- @IndexFieldType@ specifies the field is of type @date-array@. All
-- options are enabled by default.
--
-- /See:/ 'dateArrayOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datSourceFields'
--
-- * 'datReturnEnabled'
--
-- * 'datFacetEnabled'
--
-- * 'datSearchEnabled'
--
-- * 'datDefaultValue'
data DateArrayOptions = DateArrayOptions'{_datSourceFields :: Maybe Text, _datReturnEnabled :: Maybe Bool, _datFacetEnabled :: Maybe Bool, _datSearchEnabled :: Maybe Bool, _datDefaultValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DateArrayOptions' smart constructor.
dateArrayOptions :: DateArrayOptions
dateArrayOptions = DateArrayOptions'{_datSourceFields = Nothing, _datReturnEnabled = Nothing, _datFacetEnabled = Nothing, _datSearchEnabled = Nothing, _datDefaultValue = Nothing};

-- | A list of source fields to map to the field.
datSourceFields :: Lens' DateArrayOptions (Maybe Text)
datSourceFields = lens _datSourceFields (\ s a -> s{_datSourceFields = a});

-- | Whether the contents of the field can be returned in the search results.
datReturnEnabled :: Lens' DateArrayOptions (Maybe Bool)
datReturnEnabled = lens _datReturnEnabled (\ s a -> s{_datReturnEnabled = a});

-- | Whether facet information can be returned for the field.
datFacetEnabled :: Lens' DateArrayOptions (Maybe Bool)
datFacetEnabled = lens _datFacetEnabled (\ s a -> s{_datFacetEnabled = a});

-- | Whether the contents of the field are searchable.
datSearchEnabled :: Lens' DateArrayOptions (Maybe Bool)
datSearchEnabled = lens _datSearchEnabled (\ s a -> s{_datSearchEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
datDefaultValue :: Lens' DateArrayOptions (Maybe Text)
datDefaultValue = lens _datDefaultValue (\ s a -> s{_datDefaultValue = a});

instance FromXML DateArrayOptions where
        parseXML x
          = DateArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery DateArrayOptions where
        toQuery DateArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _datSourceFields,
               "ReturnEnabled" =: _datReturnEnabled,
               "FacetEnabled" =: _datFacetEnabled,
               "SearchEnabled" =: _datSearchEnabled,
               "DefaultValue" =: _datDefaultValue]

-- | Options for a date field. Dates and times are specified in UTC
-- (Coordinated Universal Time) according to IETF RFC3339:
-- yyyy-mm-ddT00:00:00Z. Present if @IndexFieldType@ specifies the field is
-- of type @date@. All options are enabled by default.
--
-- /See:/ 'dateOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doSourceField'
--
-- * 'doReturnEnabled'
--
-- * 'doFacetEnabled'
--
-- * 'doSearchEnabled'
--
-- * 'doSortEnabled'
--
-- * 'doDefaultValue'
data DateOptions = DateOptions'{_doSourceField :: Maybe Text, _doReturnEnabled :: Maybe Bool, _doFacetEnabled :: Maybe Bool, _doSearchEnabled :: Maybe Bool, _doSortEnabled :: Maybe Bool, _doDefaultValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DateOptions' smart constructor.
dateOptions :: DateOptions
dateOptions = DateOptions'{_doSourceField = Nothing, _doReturnEnabled = Nothing, _doFacetEnabled = Nothing, _doSearchEnabled = Nothing, _doSortEnabled = Nothing, _doDefaultValue = Nothing};

-- | FIXME: Undocumented member.
doSourceField :: Lens' DateOptions (Maybe Text)
doSourceField = lens _doSourceField (\ s a -> s{_doSourceField = a});

-- | Whether the contents of the field can be returned in the search results.
doReturnEnabled :: Lens' DateOptions (Maybe Bool)
doReturnEnabled = lens _doReturnEnabled (\ s a -> s{_doReturnEnabled = a});

-- | Whether facet information can be returned for the field.
doFacetEnabled :: Lens' DateOptions (Maybe Bool)
doFacetEnabled = lens _doFacetEnabled (\ s a -> s{_doFacetEnabled = a});

-- | Whether the contents of the field are searchable.
doSearchEnabled :: Lens' DateOptions (Maybe Bool)
doSearchEnabled = lens _doSearchEnabled (\ s a -> s{_doSearchEnabled = a});

-- | Whether the field can be used to sort the search results.
doSortEnabled :: Lens' DateOptions (Maybe Bool)
doSortEnabled = lens _doSortEnabled (\ s a -> s{_doSortEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
doDefaultValue :: Lens' DateOptions (Maybe Text)
doDefaultValue = lens _doDefaultValue (\ s a -> s{_doDefaultValue = a});

instance FromXML DateOptions where
        parseXML x
          = DateOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery DateOptions where
        toQuery DateOptions'{..}
          = mconcat
              ["SourceField" =: _doSourceField,
               "ReturnEnabled" =: _doReturnEnabled,
               "FacetEnabled" =: _doFacetEnabled,
               "SearchEnabled" =: _doSearchEnabled,
               "SortEnabled" =: _doSortEnabled,
               "DefaultValue" =: _doDefaultValue]

-- | Options for a search suggester.
--
-- /See:/ 'documentSuggesterOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsoSortExpression'
--
-- * 'dsoFuzzyMatching'
--
-- * 'dsoSourceField'
data DocumentSuggesterOptions = DocumentSuggesterOptions'{_dsoSortExpression :: Maybe Text, _dsoFuzzyMatching :: Maybe SuggesterFuzzyMatching, _dsoSourceField :: Text} deriving (Eq, Read, Show)

-- | 'DocumentSuggesterOptions' smart constructor.
documentSuggesterOptions :: Text -> DocumentSuggesterOptions
documentSuggesterOptions pSourceField = DocumentSuggesterOptions'{_dsoSortExpression = Nothing, _dsoFuzzyMatching = Nothing, _dsoSourceField = pSourceField};

-- | An expression that computes a score for each suggestion to control how
-- they are sorted. The scores are rounded to the nearest integer, with a
-- floor of 0 and a ceiling of 2^31-1. A document\'s relevance score is not
-- computed for suggestions, so sort expressions cannot reference the
-- @_score@ value. To sort suggestions using a numeric field or existing
-- expression, simply specify the name of the field or expression. If no
-- expression is configured for the suggester, the suggestions are sorted
-- with the closest matches listed first.
dsoSortExpression :: Lens' DocumentSuggesterOptions (Maybe Text)
dsoSortExpression = lens _dsoSortExpression (\ s a -> s{_dsoSortExpression = a});

-- | The level of fuzziness allowed when suggesting matches for a string:
-- @none@, @low@, or @high@. With none, the specified string is treated as
-- an exact prefix. With low, suggestions must differ from the specified
-- string by no more than one character. With high, suggestions can differ
-- by up to two characters. The default is none.
dsoFuzzyMatching :: Lens' DocumentSuggesterOptions (Maybe SuggesterFuzzyMatching)
dsoFuzzyMatching = lens _dsoFuzzyMatching (\ s a -> s{_dsoFuzzyMatching = a});

-- | The name of the index field you want to use for suggestions.
dsoSourceField :: Lens' DocumentSuggesterOptions Text
dsoSourceField = lens _dsoSourceField (\ s a -> s{_dsoSourceField = a});

instance FromXML DocumentSuggesterOptions where
        parseXML x
          = DocumentSuggesterOptions' <$>
              (x .@? "SortExpression") <*> (x .@? "FuzzyMatching")
                <*> (x .@ "SourceField")

instance ToQuery DocumentSuggesterOptions where
        toQuery DocumentSuggesterOptions'{..}
          = mconcat
              ["SortExpression" =: _dsoSortExpression,
               "FuzzyMatching" =: _dsoFuzzyMatching,
               "SourceField" =: _dsoSourceField]

-- | The current status of the search domain.
--
-- /See:/ 'domainStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsSearchInstanceCount'
--
-- * 'dsSearchInstanceType'
--
-- * 'dsARN'
--
-- * 'dsDocService'
--
-- * 'dsCreated'
--
-- * 'dsSearchService'
--
-- * 'dsLimits'
--
-- * 'dsSearchPartitionCount'
--
-- * 'dsDeleted'
--
-- * 'dsProcessing'
--
-- * 'dsDomainId'
--
-- * 'dsDomainName'
--
-- * 'dsRequiresIndexDocuments'
data DomainStatus = DomainStatus'{_dsSearchInstanceCount :: Maybe Nat, _dsSearchInstanceType :: Maybe Text, _dsARN :: Maybe Text, _dsDocService :: Maybe ServiceEndpoint, _dsCreated :: Maybe Bool, _dsSearchService :: Maybe ServiceEndpoint, _dsLimits :: Maybe Limits, _dsSearchPartitionCount :: Maybe Nat, _dsDeleted :: Maybe Bool, _dsProcessing :: Maybe Bool, _dsDomainId :: Text, _dsDomainName :: Text, _dsRequiresIndexDocuments :: Bool} deriving (Eq, Read, Show)

-- | 'DomainStatus' smart constructor.
domainStatus :: Text -> Text -> Bool -> DomainStatus
domainStatus pDomainId pDomainName pRequiresIndexDocuments = DomainStatus'{_dsSearchInstanceCount = Nothing, _dsSearchInstanceType = Nothing, _dsARN = Nothing, _dsDocService = Nothing, _dsCreated = Nothing, _dsSearchService = Nothing, _dsLimits = Nothing, _dsSearchPartitionCount = Nothing, _dsDeleted = Nothing, _dsProcessing = Nothing, _dsDomainId = pDomainId, _dsDomainName = pDomainName, _dsRequiresIndexDocuments = pRequiresIndexDocuments};

-- | The number of search instances that are available to process search
-- requests.
dsSearchInstanceCount :: Lens' DomainStatus (Maybe Natural)
dsSearchInstanceCount = lens _dsSearchInstanceCount (\ s a -> s{_dsSearchInstanceCount = a}) . mapping _Nat;

-- | The instance type that is being used to process search requests.
dsSearchInstanceType :: Lens' DomainStatus (Maybe Text)
dsSearchInstanceType = lens _dsSearchInstanceType (\ s a -> s{_dsSearchInstanceType = a});

-- | FIXME: Undocumented member.
dsARN :: Lens' DomainStatus (Maybe Text)
dsARN = lens _dsARN (\ s a -> s{_dsARN = a});

-- | The service endpoint for updating documents in a search domain.
dsDocService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsDocService = lens _dsDocService (\ s a -> s{_dsDocService = a});

-- | True if the search domain is created. It can take several minutes to
-- initialize a domain when CreateDomain is called. Newly created search
-- domains are returned from DescribeDomains with a false value for Created
-- until domain creation is complete.
dsCreated :: Lens' DomainStatus (Maybe Bool)
dsCreated = lens _dsCreated (\ s a -> s{_dsCreated = a});

-- | The service endpoint for requesting search results from a search domain.
dsSearchService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsSearchService = lens _dsSearchService (\ s a -> s{_dsSearchService = a});

-- | FIXME: Undocumented member.
dsLimits :: Lens' DomainStatus (Maybe Limits)
dsLimits = lens _dsLimits (\ s a -> s{_dsLimits = a});

-- | The number of partitions across which the search index is spread.
dsSearchPartitionCount :: Lens' DomainStatus (Maybe Natural)
dsSearchPartitionCount = lens _dsSearchPartitionCount (\ s a -> s{_dsSearchPartitionCount = a}) . mapping _Nat;

-- | True if the search domain has been deleted. The system must clean up
-- resources dedicated to the search domain when DeleteDomain is called.
-- Newly deleted search domains are returned from DescribeDomains with a
-- true value for IsDeleted for several minutes until resource cleanup is
-- complete.
dsDeleted :: Lens' DomainStatus (Maybe Bool)
dsDeleted = lens _dsDeleted (\ s a -> s{_dsDeleted = a});

-- | True if processing is being done to activate the current domain
-- configuration.
dsProcessing :: Lens' DomainStatus (Maybe Bool)
dsProcessing = lens _dsProcessing (\ s a -> s{_dsProcessing = a});

-- | FIXME: Undocumented member.
dsDomainId :: Lens' DomainStatus Text
dsDomainId = lens _dsDomainId (\ s a -> s{_dsDomainId = a});

-- | FIXME: Undocumented member.
dsDomainName :: Lens' DomainStatus Text
dsDomainName = lens _dsDomainName (\ s a -> s{_dsDomainName = a});

-- | True if IndexDocuments needs to be called to activate the current domain
-- configuration.
dsRequiresIndexDocuments :: Lens' DomainStatus Bool
dsRequiresIndexDocuments = lens _dsRequiresIndexDocuments (\ s a -> s{_dsRequiresIndexDocuments = a});

instance FromXML DomainStatus where
        parseXML x
          = DomainStatus' <$>
              (x .@? "SearchInstanceCount") <*>
                (x .@? "SearchInstanceType")
                <*> (x .@? "ARN")
                <*> (x .@? "DocService")
                <*> (x .@? "Created")
                <*> (x .@? "SearchService")
                <*> (x .@? "Limits")
                <*> (x .@? "SearchPartitionCount")
                <*> (x .@? "Deleted")
                <*> (x .@? "Processing")
                <*> (x .@ "DomainId")
                <*> (x .@ "DomainName")
                <*> (x .@ "RequiresIndexDocuments")

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if @IndexFieldType@ specifies the field
-- is of type @double-array@. All options are enabled by default.
--
-- /See:/ 'doubleArrayOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daoSourceFields'
--
-- * 'daoReturnEnabled'
--
-- * 'daoFacetEnabled'
--
-- * 'daoSearchEnabled'
--
-- * 'daoDefaultValue'
data DoubleArrayOptions = DoubleArrayOptions'{_daoSourceFields :: Maybe Text, _daoReturnEnabled :: Maybe Bool, _daoFacetEnabled :: Maybe Bool, _daoSearchEnabled :: Maybe Bool, _daoDefaultValue :: Maybe Double} deriving (Eq, Read, Show)

-- | 'DoubleArrayOptions' smart constructor.
doubleArrayOptions :: DoubleArrayOptions
doubleArrayOptions = DoubleArrayOptions'{_daoSourceFields = Nothing, _daoReturnEnabled = Nothing, _daoFacetEnabled = Nothing, _daoSearchEnabled = Nothing, _daoDefaultValue = Nothing};

-- | A list of source fields to map to the field.
daoSourceFields :: Lens' DoubleArrayOptions (Maybe Text)
daoSourceFields = lens _daoSourceFields (\ s a -> s{_daoSourceFields = a});

-- | Whether the contents of the field can be returned in the search results.
daoReturnEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoReturnEnabled = lens _daoReturnEnabled (\ s a -> s{_daoReturnEnabled = a});

-- | Whether facet information can be returned for the field.
daoFacetEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoFacetEnabled = lens _daoFacetEnabled (\ s a -> s{_daoFacetEnabled = a});

-- | Whether the contents of the field are searchable.
daoSearchEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoSearchEnabled = lens _daoSearchEnabled (\ s a -> s{_daoSearchEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
daoDefaultValue :: Lens' DoubleArrayOptions (Maybe Double)
daoDefaultValue = lens _daoDefaultValue (\ s a -> s{_daoDefaultValue = a});

instance FromXML DoubleArrayOptions where
        parseXML x
          = DoubleArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery DoubleArrayOptions where
        toQuery DoubleArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _daoSourceFields,
               "ReturnEnabled" =: _daoReturnEnabled,
               "FacetEnabled" =: _daoFacetEnabled,
               "SearchEnabled" =: _daoSearchEnabled,
               "DefaultValue" =: _daoDefaultValue]

-- | Options for a double-precision 64-bit floating point field. Present if
-- @IndexFieldType@ specifies the field is of type @double@. All options
-- are enabled by default.
--
-- /See:/ 'doubleOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'douSourceField'
--
-- * 'douReturnEnabled'
--
-- * 'douFacetEnabled'
--
-- * 'douSearchEnabled'
--
-- * 'douSortEnabled'
--
-- * 'douDefaultValue'
data DoubleOptions = DoubleOptions'{_douSourceField :: Maybe Text, _douReturnEnabled :: Maybe Bool, _douFacetEnabled :: Maybe Bool, _douSearchEnabled :: Maybe Bool, _douSortEnabled :: Maybe Bool, _douDefaultValue :: Maybe Double} deriving (Eq, Read, Show)

-- | 'DoubleOptions' smart constructor.
doubleOptions :: DoubleOptions
doubleOptions = DoubleOptions'{_douSourceField = Nothing, _douReturnEnabled = Nothing, _douFacetEnabled = Nothing, _douSearchEnabled = Nothing, _douSortEnabled = Nothing, _douDefaultValue = Nothing};

-- | The name of the source field to map to the field.
douSourceField :: Lens' DoubleOptions (Maybe Text)
douSourceField = lens _douSourceField (\ s a -> s{_douSourceField = a});

-- | Whether the contents of the field can be returned in the search results.
douReturnEnabled :: Lens' DoubleOptions (Maybe Bool)
douReturnEnabled = lens _douReturnEnabled (\ s a -> s{_douReturnEnabled = a});

-- | Whether facet information can be returned for the field.
douFacetEnabled :: Lens' DoubleOptions (Maybe Bool)
douFacetEnabled = lens _douFacetEnabled (\ s a -> s{_douFacetEnabled = a});

-- | Whether the contents of the field are searchable.
douSearchEnabled :: Lens' DoubleOptions (Maybe Bool)
douSearchEnabled = lens _douSearchEnabled (\ s a -> s{_douSearchEnabled = a});

-- | Whether the field can be used to sort the search results.
douSortEnabled :: Lens' DoubleOptions (Maybe Bool)
douSortEnabled = lens _douSortEnabled (\ s a -> s{_douSortEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
douDefaultValue :: Lens' DoubleOptions (Maybe Double)
douDefaultValue = lens _douDefaultValue (\ s a -> s{_douDefaultValue = a});

instance FromXML DoubleOptions where
        parseXML x
          = DoubleOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery DoubleOptions where
        toQuery DoubleOptions'{..}
          = mconcat
              ["SourceField" =: _douSourceField,
               "ReturnEnabled" =: _douReturnEnabled,
               "FacetEnabled" =: _douFacetEnabled,
               "SearchEnabled" =: _douSearchEnabled,
               "SortEnabled" =: _douSortEnabled,
               "DefaultValue" =: _douDefaultValue]

-- | A named expression that can be evaluated at search time. Can be used to
-- sort the search results, define other expressions, or return computed
-- information in the search results.
--
-- /See:/ 'expression' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'expExpressionName'
--
-- * 'expExpressionValue'
data Expression = Expression'{_expExpressionName :: Text, _expExpressionValue :: Text} deriving (Eq, Read, Show)

-- | 'Expression' smart constructor.
expression :: Text -> Text -> Expression
expression pExpressionName pExpressionValue = Expression'{_expExpressionName = pExpressionName, _expExpressionValue = pExpressionValue};

-- | FIXME: Undocumented member.
expExpressionName :: Lens' Expression Text
expExpressionName = lens _expExpressionName (\ s a -> s{_expExpressionName = a});

-- | FIXME: Undocumented member.
expExpressionValue :: Lens' Expression Text
expExpressionValue = lens _expExpressionValue (\ s a -> s{_expExpressionValue = a});

instance FromXML Expression where
        parseXML x
          = Expression' <$>
              (x .@ "ExpressionName") <*> (x .@ "ExpressionValue")

instance ToQuery Expression where
        toQuery Expression'{..}
          = mconcat
              ["ExpressionName" =: _expExpressionName,
               "ExpressionValue" =: _expExpressionValue]

-- | The value of an @Expression@ and its current status.
--
-- /See:/ 'expressionStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esOptions'
--
-- * 'esStatus'
data ExpressionStatus = ExpressionStatus'{_esOptions :: Expression, _esStatus :: OptionStatus} deriving (Eq, Read, Show)

-- | 'ExpressionStatus' smart constructor.
expressionStatus :: Expression -> OptionStatus -> ExpressionStatus
expressionStatus pOptions pStatus = ExpressionStatus'{_esOptions = pOptions, _esStatus = pStatus};

-- | The expression that is evaluated for sorting while processing a search
-- request.
esOptions :: Lens' ExpressionStatus Expression
esOptions = lens _esOptions (\ s a -> s{_esOptions = a});

-- | FIXME: Undocumented member.
esStatus :: Lens' ExpressionStatus OptionStatus
esStatus = lens _esStatus (\ s a -> s{_esStatus = a});

instance FromXML ExpressionStatus where
        parseXML x
          = ExpressionStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

-- | Configuration information for a field in the index, including its name,
-- type, and options. The supported options depend on the @IndexFieldType@.
--
-- /See:/ 'indexField' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ifDateOptions'
--
-- * 'ifTextArrayOptions'
--
-- * 'ifDoubleArrayOptions'
--
-- * 'ifDoubleOptions'
--
-- * 'ifTextOptions'
--
-- * 'ifLatLonOptions'
--
-- * 'ifIntArrayOptions'
--
-- * 'ifLiteralArrayOptions'
--
-- * 'ifDateArrayOptions'
--
-- * 'ifLiteralOptions'
--
-- * 'ifIntOptions'
--
-- * 'ifIndexFieldName'
--
-- * 'ifIndexFieldType'
data IndexField = IndexField'{_ifDateOptions :: Maybe DateOptions, _ifTextArrayOptions :: Maybe TextArrayOptions, _ifDoubleArrayOptions :: Maybe DoubleArrayOptions, _ifDoubleOptions :: Maybe DoubleOptions, _ifTextOptions :: Maybe TextOptions, _ifLatLonOptions :: Maybe LatLonOptions, _ifIntArrayOptions :: Maybe IntArrayOptions, _ifLiteralArrayOptions :: Maybe LiteralArrayOptions, _ifDateArrayOptions :: Maybe DateArrayOptions, _ifLiteralOptions :: Maybe LiteralOptions, _ifIntOptions :: Maybe IntOptions, _ifIndexFieldName :: Text, _ifIndexFieldType :: IndexFieldType} deriving (Eq, Read, Show)

-- | 'IndexField' smart constructor.
indexField :: Text -> IndexFieldType -> IndexField
indexField pIndexFieldName pIndexFieldType = IndexField'{_ifDateOptions = Nothing, _ifTextArrayOptions = Nothing, _ifDoubleArrayOptions = Nothing, _ifDoubleOptions = Nothing, _ifTextOptions = Nothing, _ifLatLonOptions = Nothing, _ifIntArrayOptions = Nothing, _ifLiteralArrayOptions = Nothing, _ifDateArrayOptions = Nothing, _ifLiteralOptions = Nothing, _ifIntOptions = Nothing, _ifIndexFieldName = pIndexFieldName, _ifIndexFieldType = pIndexFieldType};

-- | FIXME: Undocumented member.
ifDateOptions :: Lens' IndexField (Maybe DateOptions)
ifDateOptions = lens _ifDateOptions (\ s a -> s{_ifDateOptions = a});

-- | FIXME: Undocumented member.
ifTextArrayOptions :: Lens' IndexField (Maybe TextArrayOptions)
ifTextArrayOptions = lens _ifTextArrayOptions (\ s a -> s{_ifTextArrayOptions = a});

-- | FIXME: Undocumented member.
ifDoubleArrayOptions :: Lens' IndexField (Maybe DoubleArrayOptions)
ifDoubleArrayOptions = lens _ifDoubleArrayOptions (\ s a -> s{_ifDoubleArrayOptions = a});

-- | FIXME: Undocumented member.
ifDoubleOptions :: Lens' IndexField (Maybe DoubleOptions)
ifDoubleOptions = lens _ifDoubleOptions (\ s a -> s{_ifDoubleOptions = a});

-- | FIXME: Undocumented member.
ifTextOptions :: Lens' IndexField (Maybe TextOptions)
ifTextOptions = lens _ifTextOptions (\ s a -> s{_ifTextOptions = a});

-- | FIXME: Undocumented member.
ifLatLonOptions :: Lens' IndexField (Maybe LatLonOptions)
ifLatLonOptions = lens _ifLatLonOptions (\ s a -> s{_ifLatLonOptions = a});

-- | FIXME: Undocumented member.
ifIntArrayOptions :: Lens' IndexField (Maybe IntArrayOptions)
ifIntArrayOptions = lens _ifIntArrayOptions (\ s a -> s{_ifIntArrayOptions = a});

-- | FIXME: Undocumented member.
ifLiteralArrayOptions :: Lens' IndexField (Maybe LiteralArrayOptions)
ifLiteralArrayOptions = lens _ifLiteralArrayOptions (\ s a -> s{_ifLiteralArrayOptions = a});

-- | FIXME: Undocumented member.
ifDateArrayOptions :: Lens' IndexField (Maybe DateArrayOptions)
ifDateArrayOptions = lens _ifDateArrayOptions (\ s a -> s{_ifDateArrayOptions = a});

-- | FIXME: Undocumented member.
ifLiteralOptions :: Lens' IndexField (Maybe LiteralOptions)
ifLiteralOptions = lens _ifLiteralOptions (\ s a -> s{_ifLiteralOptions = a});

-- | FIXME: Undocumented member.
ifIntOptions :: Lens' IndexField (Maybe IntOptions)
ifIntOptions = lens _ifIntOptions (\ s a -> s{_ifIntOptions = a});

-- | A string that represents the name of an index field. CloudSearch
-- supports regular index fields as well as dynamic fields. A dynamic
-- field\'s name defines a pattern that begins or ends with a wildcard. Any
-- document fields that don\'t map to a regular index field but do match a
-- dynamic field\'s pattern are configured with the dynamic field\'s
-- indexing options.
--
-- Regular field names begin with a letter and can contain the following
-- characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field
-- names must begin or end with a wildcard (*). The wildcard can also be
-- the only character in a dynamic field name. Multiple wildcards, and
-- wildcards embedded within a string are not supported.
--
-- The name @score@ is reserved and cannot be used as a field name. To
-- reference a document\'s ID, you can use the name @_id@.
ifIndexFieldName :: Lens' IndexField Text
ifIndexFieldName = lens _ifIndexFieldName (\ s a -> s{_ifIndexFieldName = a});

-- | FIXME: Undocumented member.
ifIndexFieldType :: Lens' IndexField IndexFieldType
ifIndexFieldType = lens _ifIndexFieldType (\ s a -> s{_ifIndexFieldType = a});

instance FromXML IndexField where
        parseXML x
          = IndexField' <$>
              (x .@? "DateOptions") <*> (x .@? "TextArrayOptions")
                <*> (x .@? "DoubleArrayOptions")
                <*> (x .@? "DoubleOptions")
                <*> (x .@? "TextOptions")
                <*> (x .@? "LatLonOptions")
                <*> (x .@? "IntArrayOptions")
                <*> (x .@? "LiteralArrayOptions")
                <*> (x .@? "DateArrayOptions")
                <*> (x .@? "LiteralOptions")
                <*> (x .@? "IntOptions")
                <*> (x .@ "IndexFieldName")
                <*> (x .@ "IndexFieldType")

instance ToQuery IndexField where
        toQuery IndexField'{..}
          = mconcat
              ["DateOptions" =: _ifDateOptions,
               "TextArrayOptions" =: _ifTextArrayOptions,
               "DoubleArrayOptions" =: _ifDoubleArrayOptions,
               "DoubleOptions" =: _ifDoubleOptions,
               "TextOptions" =: _ifTextOptions,
               "LatLonOptions" =: _ifLatLonOptions,
               "IntArrayOptions" =: _ifIntArrayOptions,
               "LiteralArrayOptions" =: _ifLiteralArrayOptions,
               "DateArrayOptions" =: _ifDateArrayOptions,
               "LiteralOptions" =: _ifLiteralOptions,
               "IntOptions" =: _ifIntOptions,
               "IndexFieldName" =: _ifIndexFieldName,
               "IndexFieldType" =: _ifIndexFieldType]

-- | The value of an @IndexField@ and its current status.
--
-- /See:/ 'indexFieldStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ifsOptions'
--
-- * 'ifsStatus'
data IndexFieldStatus = IndexFieldStatus'{_ifsOptions :: IndexField, _ifsStatus :: OptionStatus} deriving (Eq, Read, Show)

-- | 'IndexFieldStatus' smart constructor.
indexFieldStatus :: IndexField -> OptionStatus -> IndexFieldStatus
indexFieldStatus pOptions pStatus = IndexFieldStatus'{_ifsOptions = pOptions, _ifsStatus = pStatus};

-- | FIXME: Undocumented member.
ifsOptions :: Lens' IndexFieldStatus IndexField
ifsOptions = lens _ifsOptions (\ s a -> s{_ifsOptions = a});

-- | FIXME: Undocumented member.
ifsStatus :: Lens' IndexFieldStatus OptionStatus
ifsStatus = lens _ifsStatus (\ s a -> s{_ifsStatus = a});

instance FromXML IndexFieldStatus where
        parseXML x
          = IndexFieldStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

-- | Options for a field that contains an array of 64-bit signed integers.
-- Present if @IndexFieldType@ specifies the field is of type @int-array@.
-- All options are enabled by default.
--
-- /See:/ 'intArrayOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iaoSourceFields'
--
-- * 'iaoReturnEnabled'
--
-- * 'iaoFacetEnabled'
--
-- * 'iaoSearchEnabled'
--
-- * 'iaoDefaultValue'
data IntArrayOptions = IntArrayOptions'{_iaoSourceFields :: Maybe Text, _iaoReturnEnabled :: Maybe Bool, _iaoFacetEnabled :: Maybe Bool, _iaoSearchEnabled :: Maybe Bool, _iaoDefaultValue :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'IntArrayOptions' smart constructor.
intArrayOptions :: IntArrayOptions
intArrayOptions = IntArrayOptions'{_iaoSourceFields = Nothing, _iaoReturnEnabled = Nothing, _iaoFacetEnabled = Nothing, _iaoSearchEnabled = Nothing, _iaoDefaultValue = Nothing};

-- | A list of source fields to map to the field.
iaoSourceFields :: Lens' IntArrayOptions (Maybe Text)
iaoSourceFields = lens _iaoSourceFields (\ s a -> s{_iaoSourceFields = a});

-- | Whether the contents of the field can be returned in the search results.
iaoReturnEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoReturnEnabled = lens _iaoReturnEnabled (\ s a -> s{_iaoReturnEnabled = a});

-- | Whether facet information can be returned for the field.
iaoFacetEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoFacetEnabled = lens _iaoFacetEnabled (\ s a -> s{_iaoFacetEnabled = a});

-- | Whether the contents of the field are searchable.
iaoSearchEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoSearchEnabled = lens _iaoSearchEnabled (\ s a -> s{_iaoSearchEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
iaoDefaultValue :: Lens' IntArrayOptions (Maybe Integer)
iaoDefaultValue = lens _iaoDefaultValue (\ s a -> s{_iaoDefaultValue = a});

instance FromXML IntArrayOptions where
        parseXML x
          = IntArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery IntArrayOptions where
        toQuery IntArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _iaoSourceFields,
               "ReturnEnabled" =: _iaoReturnEnabled,
               "FacetEnabled" =: _iaoFacetEnabled,
               "SearchEnabled" =: _iaoSearchEnabled,
               "DefaultValue" =: _iaoDefaultValue]

-- | Options for a 64-bit signed integer field. Present if @IndexFieldType@
-- specifies the field is of type @int@. All options are enabled by
-- default.
--
-- /See:/ 'intOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ioSourceField'
--
-- * 'ioReturnEnabled'
--
-- * 'ioFacetEnabled'
--
-- * 'ioSearchEnabled'
--
-- * 'ioSortEnabled'
--
-- * 'ioDefaultValue'
data IntOptions = IntOptions'{_ioSourceField :: Maybe Text, _ioReturnEnabled :: Maybe Bool, _ioFacetEnabled :: Maybe Bool, _ioSearchEnabled :: Maybe Bool, _ioSortEnabled :: Maybe Bool, _ioDefaultValue :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'IntOptions' smart constructor.
intOptions :: IntOptions
intOptions = IntOptions'{_ioSourceField = Nothing, _ioReturnEnabled = Nothing, _ioFacetEnabled = Nothing, _ioSearchEnabled = Nothing, _ioSortEnabled = Nothing, _ioDefaultValue = Nothing};

-- | The name of the source field to map to the field.
ioSourceField :: Lens' IntOptions (Maybe Text)
ioSourceField = lens _ioSourceField (\ s a -> s{_ioSourceField = a});

-- | Whether the contents of the field can be returned in the search results.
ioReturnEnabled :: Lens' IntOptions (Maybe Bool)
ioReturnEnabled = lens _ioReturnEnabled (\ s a -> s{_ioReturnEnabled = a});

-- | Whether facet information can be returned for the field.
ioFacetEnabled :: Lens' IntOptions (Maybe Bool)
ioFacetEnabled = lens _ioFacetEnabled (\ s a -> s{_ioFacetEnabled = a});

-- | Whether the contents of the field are searchable.
ioSearchEnabled :: Lens' IntOptions (Maybe Bool)
ioSearchEnabled = lens _ioSearchEnabled (\ s a -> s{_ioSearchEnabled = a});

-- | Whether the field can be used to sort the search results.
ioSortEnabled :: Lens' IntOptions (Maybe Bool)
ioSortEnabled = lens _ioSortEnabled (\ s a -> s{_ioSortEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
ioDefaultValue :: Lens' IntOptions (Maybe Integer)
ioDefaultValue = lens _ioDefaultValue (\ s a -> s{_ioDefaultValue = a});

instance FromXML IntOptions where
        parseXML x
          = IntOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery IntOptions where
        toQuery IntOptions'{..}
          = mconcat
              ["SourceField" =: _ioSourceField,
               "ReturnEnabled" =: _ioReturnEnabled,
               "FacetEnabled" =: _ioFacetEnabled,
               "SearchEnabled" =: _ioSearchEnabled,
               "SortEnabled" =: _ioSortEnabled,
               "DefaultValue" =: _ioDefaultValue]

-- | Options for a latlon field. A latlon field contains a location stored as
-- a latitude and longitude value pair. Present if @IndexFieldType@
-- specifies the field is of type @latlon@. All options are enabled by
-- default.
--
-- /See:/ 'latLonOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lloSourceField'
--
-- * 'lloReturnEnabled'
--
-- * 'lloFacetEnabled'
--
-- * 'lloSearchEnabled'
--
-- * 'lloSortEnabled'
--
-- * 'lloDefaultValue'
data LatLonOptions = LatLonOptions'{_lloSourceField :: Maybe Text, _lloReturnEnabled :: Maybe Bool, _lloFacetEnabled :: Maybe Bool, _lloSearchEnabled :: Maybe Bool, _lloSortEnabled :: Maybe Bool, _lloDefaultValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'LatLonOptions' smart constructor.
latLonOptions :: LatLonOptions
latLonOptions = LatLonOptions'{_lloSourceField = Nothing, _lloReturnEnabled = Nothing, _lloFacetEnabled = Nothing, _lloSearchEnabled = Nothing, _lloSortEnabled = Nothing, _lloDefaultValue = Nothing};

-- | FIXME: Undocumented member.
lloSourceField :: Lens' LatLonOptions (Maybe Text)
lloSourceField = lens _lloSourceField (\ s a -> s{_lloSourceField = a});

-- | Whether the contents of the field can be returned in the search results.
lloReturnEnabled :: Lens' LatLonOptions (Maybe Bool)
lloReturnEnabled = lens _lloReturnEnabled (\ s a -> s{_lloReturnEnabled = a});

-- | Whether facet information can be returned for the field.
lloFacetEnabled :: Lens' LatLonOptions (Maybe Bool)
lloFacetEnabled = lens _lloFacetEnabled (\ s a -> s{_lloFacetEnabled = a});

-- | Whether the contents of the field are searchable.
lloSearchEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSearchEnabled = lens _lloSearchEnabled (\ s a -> s{_lloSearchEnabled = a});

-- | Whether the field can be used to sort the search results.
lloSortEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSortEnabled = lens _lloSortEnabled (\ s a -> s{_lloSortEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
lloDefaultValue :: Lens' LatLonOptions (Maybe Text)
lloDefaultValue = lens _lloDefaultValue (\ s a -> s{_lloDefaultValue = a});

instance FromXML LatLonOptions where
        parseXML x
          = LatLonOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery LatLonOptions where
        toQuery LatLonOptions'{..}
          = mconcat
              ["SourceField" =: _lloSourceField,
               "ReturnEnabled" =: _lloReturnEnabled,
               "FacetEnabled" =: _lloFacetEnabled,
               "SearchEnabled" =: _lloSearchEnabled,
               "SortEnabled" =: _lloSortEnabled,
               "DefaultValue" =: _lloDefaultValue]

-- | /See:/ 'limits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'limMaximumReplicationCount'
--
-- * 'limMaximumPartitionCount'
data Limits = Limits'{_limMaximumReplicationCount :: Nat, _limMaximumPartitionCount :: Nat} deriving (Eq, Read, Show)

-- | 'Limits' smart constructor.
limits :: Natural -> Natural -> Limits
limits pMaximumReplicationCount pMaximumPartitionCount = Limits'{_limMaximumReplicationCount = _Nat # pMaximumReplicationCount, _limMaximumPartitionCount = _Nat # pMaximumPartitionCount};

-- | FIXME: Undocumented member.
limMaximumReplicationCount :: Lens' Limits Natural
limMaximumReplicationCount = lens _limMaximumReplicationCount (\ s a -> s{_limMaximumReplicationCount = a}) . _Nat;

-- | FIXME: Undocumented member.
limMaximumPartitionCount :: Lens' Limits Natural
limMaximumPartitionCount = lens _limMaximumPartitionCount (\ s a -> s{_limMaximumPartitionCount = a}) . _Nat;

instance FromXML Limits where
        parseXML x
          = Limits' <$>
              (x .@ "MaximumReplicationCount") <*>
                (x .@ "MaximumPartitionCount")

-- | Options for a field that contains an array of literal strings. Present
-- if @IndexFieldType@ specifies the field is of type @literal-array@. All
-- options are enabled by default.
--
-- /See:/ 'literalArrayOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laoSourceFields'
--
-- * 'laoReturnEnabled'
--
-- * 'laoFacetEnabled'
--
-- * 'laoSearchEnabled'
--
-- * 'laoDefaultValue'
data LiteralArrayOptions = LiteralArrayOptions'{_laoSourceFields :: Maybe Text, _laoReturnEnabled :: Maybe Bool, _laoFacetEnabled :: Maybe Bool, _laoSearchEnabled :: Maybe Bool, _laoDefaultValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'LiteralArrayOptions' smart constructor.
literalArrayOptions :: LiteralArrayOptions
literalArrayOptions = LiteralArrayOptions'{_laoSourceFields = Nothing, _laoReturnEnabled = Nothing, _laoFacetEnabled = Nothing, _laoSearchEnabled = Nothing, _laoDefaultValue = Nothing};

-- | A list of source fields to map to the field.
laoSourceFields :: Lens' LiteralArrayOptions (Maybe Text)
laoSourceFields = lens _laoSourceFields (\ s a -> s{_laoSourceFields = a});

-- | Whether the contents of the field can be returned in the search results.
laoReturnEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoReturnEnabled = lens _laoReturnEnabled (\ s a -> s{_laoReturnEnabled = a});

-- | Whether facet information can be returned for the field.
laoFacetEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoFacetEnabled = lens _laoFacetEnabled (\ s a -> s{_laoFacetEnabled = a});

-- | Whether the contents of the field are searchable.
laoSearchEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoSearchEnabled = lens _laoSearchEnabled (\ s a -> s{_laoSearchEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
laoDefaultValue :: Lens' LiteralArrayOptions (Maybe Text)
laoDefaultValue = lens _laoDefaultValue (\ s a -> s{_laoDefaultValue = a});

instance FromXML LiteralArrayOptions where
        parseXML x
          = LiteralArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery LiteralArrayOptions where
        toQuery LiteralArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _laoSourceFields,
               "ReturnEnabled" =: _laoReturnEnabled,
               "FacetEnabled" =: _laoFacetEnabled,
               "SearchEnabled" =: _laoSearchEnabled,
               "DefaultValue" =: _laoDefaultValue]

-- | Options for literal field. Present if @IndexFieldType@ specifies the
-- field is of type @literal@. All options are enabled by default.
--
-- /See:/ 'literalOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loSourceField'
--
-- * 'loReturnEnabled'
--
-- * 'loFacetEnabled'
--
-- * 'loSearchEnabled'
--
-- * 'loSortEnabled'
--
-- * 'loDefaultValue'
data LiteralOptions = LiteralOptions'{_loSourceField :: Maybe Text, _loReturnEnabled :: Maybe Bool, _loFacetEnabled :: Maybe Bool, _loSearchEnabled :: Maybe Bool, _loSortEnabled :: Maybe Bool, _loDefaultValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'LiteralOptions' smart constructor.
literalOptions :: LiteralOptions
literalOptions = LiteralOptions'{_loSourceField = Nothing, _loReturnEnabled = Nothing, _loFacetEnabled = Nothing, _loSearchEnabled = Nothing, _loSortEnabled = Nothing, _loDefaultValue = Nothing};

-- | FIXME: Undocumented member.
loSourceField :: Lens' LiteralOptions (Maybe Text)
loSourceField = lens _loSourceField (\ s a -> s{_loSourceField = a});

-- | Whether the contents of the field can be returned in the search results.
loReturnEnabled :: Lens' LiteralOptions (Maybe Bool)
loReturnEnabled = lens _loReturnEnabled (\ s a -> s{_loReturnEnabled = a});

-- | Whether facet information can be returned for the field.
loFacetEnabled :: Lens' LiteralOptions (Maybe Bool)
loFacetEnabled = lens _loFacetEnabled (\ s a -> s{_loFacetEnabled = a});

-- | Whether the contents of the field are searchable.
loSearchEnabled :: Lens' LiteralOptions (Maybe Bool)
loSearchEnabled = lens _loSearchEnabled (\ s a -> s{_loSearchEnabled = a});

-- | Whether the field can be used to sort the search results.
loSortEnabled :: Lens' LiteralOptions (Maybe Bool)
loSortEnabled = lens _loSortEnabled (\ s a -> s{_loSortEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
loDefaultValue :: Lens' LiteralOptions (Maybe Text)
loDefaultValue = lens _loDefaultValue (\ s a -> s{_loDefaultValue = a});

instance FromXML LiteralOptions where
        parseXML x
          = LiteralOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery LiteralOptions where
        toQuery LiteralOptions'{..}
          = mconcat
              ["SourceField" =: _loSourceField,
               "ReturnEnabled" =: _loReturnEnabled,
               "FacetEnabled" =: _loFacetEnabled,
               "SearchEnabled" =: _loSearchEnabled,
               "SortEnabled" =: _loSortEnabled,
               "DefaultValue" =: _loDefaultValue]

-- | The status of domain configuration option.
--
-- /See:/ 'optionStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osPendingDeletion'
--
-- * 'osUpdateVersion'
--
-- * 'osCreationDate'
--
-- * 'osUpdateDate'
--
-- * 'osState'
data OptionStatus = OptionStatus'{_osPendingDeletion :: Maybe Bool, _osUpdateVersion :: Maybe Nat, _osCreationDate :: ISO8601, _osUpdateDate :: ISO8601, _osState :: OptionState} deriving (Eq, Read, Show)

-- | 'OptionStatus' smart constructor.
optionStatus :: UTCTime -> UTCTime -> OptionState -> OptionStatus
optionStatus pCreationDate pUpdateDate pState = OptionStatus'{_osPendingDeletion = Nothing, _osUpdateVersion = Nothing, _osCreationDate = _Time # pCreationDate, _osUpdateDate = _Time # pUpdateDate, _osState = pState};

-- | Indicates that the option will be deleted once processing is complete.
osPendingDeletion :: Lens' OptionStatus (Maybe Bool)
osPendingDeletion = lens _osPendingDeletion (\ s a -> s{_osPendingDeletion = a});

-- | A unique integer that indicates when this option was last updated.
osUpdateVersion :: Lens' OptionStatus (Maybe Natural)
osUpdateVersion = lens _osUpdateVersion (\ s a -> s{_osUpdateVersion = a}) . mapping _Nat;

-- | A timestamp for when this option was created.
osCreationDate :: Lens' OptionStatus UTCTime
osCreationDate = lens _osCreationDate (\ s a -> s{_osCreationDate = a}) . _Time;

-- | A timestamp for when this option was last updated.
osUpdateDate :: Lens' OptionStatus UTCTime
osUpdateDate = lens _osUpdateDate (\ s a -> s{_osUpdateDate = a}) . _Time;

-- | The state of processing a change to an option. Possible values:
--
-- -   @RequiresIndexDocuments@: the option\'s latest value will not be
--     deployed until IndexDocuments has been called and indexing is
--     complete.
-- -   @Processing@: the option\'s latest value is in the process of being
--     activated.
-- -   @Active@: the option\'s latest value is completely deployed.
-- -   @FailedToValidate@: the option value is not compatible with the
--     domain\'s data and cannot be used to index the data. You must either
--     modify the option value or update or remove the incompatible
--     documents.
osState :: Lens' OptionStatus OptionState
osState = lens _osState (\ s a -> s{_osState = a});

instance FromXML OptionStatus where
        parseXML x
          = OptionStatus' <$>
              (x .@? "PendingDeletion") <*> (x .@? "UpdateVersion")
                <*> (x .@ "CreationDate")
                <*> (x .@ "UpdateDate")
                <*> (x .@ "State")

-- | The desired instance type and desired number of replicas of each index
-- partition.
--
-- /See:/ 'scalingParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spDesiredInstanceType'
--
-- * 'spDesiredReplicationCount'
--
-- * 'spDesiredPartitionCount'
data ScalingParameters = ScalingParameters'{_spDesiredInstanceType :: Maybe PartitionInstanceType, _spDesiredReplicationCount :: Maybe Nat, _spDesiredPartitionCount :: Maybe Nat} deriving (Eq, Read, Show)

-- | 'ScalingParameters' smart constructor.
scalingParameters :: ScalingParameters
scalingParameters = ScalingParameters'{_spDesiredInstanceType = Nothing, _spDesiredReplicationCount = Nothing, _spDesiredPartitionCount = Nothing};

-- | The instance type that you want to preconfigure for your domain. For
-- example, @search.m1.small@.
spDesiredInstanceType :: Lens' ScalingParameters (Maybe PartitionInstanceType)
spDesiredInstanceType = lens _spDesiredInstanceType (\ s a -> s{_spDesiredInstanceType = a});

-- | The number of replicas you want to preconfigure for each index
-- partition.
spDesiredReplicationCount :: Lens' ScalingParameters (Maybe Natural)
spDesiredReplicationCount = lens _spDesiredReplicationCount (\ s a -> s{_spDesiredReplicationCount = a}) . mapping _Nat;

-- | The number of partitions you want to preconfigure for your domain. Only
-- valid when you select @m2.2xlarge@ as the desired instance type.
spDesiredPartitionCount :: Lens' ScalingParameters (Maybe Natural)
spDesiredPartitionCount = lens _spDesiredPartitionCount (\ s a -> s{_spDesiredPartitionCount = a}) . mapping _Nat;

instance FromXML ScalingParameters where
        parseXML x
          = ScalingParameters' <$>
              (x .@? "DesiredInstanceType") <*>
                (x .@? "DesiredReplicationCount")
                <*> (x .@? "DesiredPartitionCount")

instance ToQuery ScalingParameters where
        toQuery ScalingParameters'{..}
          = mconcat
              ["DesiredInstanceType" =: _spDesiredInstanceType,
               "DesiredReplicationCount" =:
                 _spDesiredReplicationCount,
               "DesiredPartitionCount" =: _spDesiredPartitionCount]

-- | The status and configuration of a search domain\'s scaling parameters.
--
-- /See:/ 'scalingParametersStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spsOptions'
--
-- * 'spsStatus'
data ScalingParametersStatus = ScalingParametersStatus'{_spsOptions :: ScalingParameters, _spsStatus :: OptionStatus} deriving (Eq, Read, Show)

-- | 'ScalingParametersStatus' smart constructor.
scalingParametersStatus :: ScalingParameters -> OptionStatus -> ScalingParametersStatus
scalingParametersStatus pOptions pStatus = ScalingParametersStatus'{_spsOptions = pOptions, _spsStatus = pStatus};

-- | FIXME: Undocumented member.
spsOptions :: Lens' ScalingParametersStatus ScalingParameters
spsOptions = lens _spsOptions (\ s a -> s{_spsOptions = a});

-- | FIXME: Undocumented member.
spsStatus :: Lens' ScalingParametersStatus OptionStatus
spsStatus = lens _spsStatus (\ s a -> s{_spsStatus = a});

instance FromXML ScalingParametersStatus where
        parseXML x
          = ScalingParametersStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

-- | The endpoint to which service requests can be submitted.
--
-- /See:/ 'serviceEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seEndpoint'
newtype ServiceEndpoint = ServiceEndpoint'{_seEndpoint :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ServiceEndpoint' smart constructor.
serviceEndpoint :: ServiceEndpoint
serviceEndpoint = ServiceEndpoint'{_seEndpoint = Nothing};

-- | FIXME: Undocumented member.
seEndpoint :: Lens' ServiceEndpoint (Maybe Text)
seEndpoint = lens _seEndpoint (\ s a -> s{_seEndpoint = a});

instance FromXML ServiceEndpoint where
        parseXML x = ServiceEndpoint' <$> (x .@? "Endpoint")

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for
-- suggestions. The following options can be configured for a suggester:
-- @FuzzyMatching@, @SortExpression@.
--
-- /See:/ 'suggester' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sugSuggesterName'
--
-- * 'sugDocumentSuggesterOptions'
data Suggester = Suggester'{_sugSuggesterName :: Text, _sugDocumentSuggesterOptions :: DocumentSuggesterOptions} deriving (Eq, Read, Show)

-- | 'Suggester' smart constructor.
suggester :: Text -> DocumentSuggesterOptions -> Suggester
suggester pSuggesterName pDocumentSuggesterOptions = Suggester'{_sugSuggesterName = pSuggesterName, _sugDocumentSuggesterOptions = pDocumentSuggesterOptions};

-- | FIXME: Undocumented member.
sugSuggesterName :: Lens' Suggester Text
sugSuggesterName = lens _sugSuggesterName (\ s a -> s{_sugSuggesterName = a});

-- | FIXME: Undocumented member.
sugDocumentSuggesterOptions :: Lens' Suggester DocumentSuggesterOptions
sugDocumentSuggesterOptions = lens _sugDocumentSuggesterOptions (\ s a -> s{_sugDocumentSuggesterOptions = a});

instance FromXML Suggester where
        parseXML x
          = Suggester' <$>
              (x .@ "SuggesterName") <*>
                (x .@ "DocumentSuggesterOptions")

instance ToQuery Suggester where
        toQuery Suggester'{..}
          = mconcat
              ["SuggesterName" =: _sugSuggesterName,
               "DocumentSuggesterOptions" =:
                 _sugDocumentSuggesterOptions]

-- | The value of a @Suggester@ and its current status.
--
-- /See:/ 'suggesterStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssOptions'
--
-- * 'ssStatus'
data SuggesterStatus = SuggesterStatus'{_ssOptions :: Suggester, _ssStatus :: OptionStatus} deriving (Eq, Read, Show)

-- | 'SuggesterStatus' smart constructor.
suggesterStatus :: Suggester -> OptionStatus -> SuggesterStatus
suggesterStatus pOptions pStatus = SuggesterStatus'{_ssOptions = pOptions, _ssStatus = pStatus};

-- | FIXME: Undocumented member.
ssOptions :: Lens' SuggesterStatus Suggester
ssOptions = lens _ssOptions (\ s a -> s{_ssOptions = a});

-- | FIXME: Undocumented member.
ssStatus :: Lens' SuggesterStatus OptionStatus
ssStatus = lens _ssStatus (\ s a -> s{_ssStatus = a});

instance FromXML SuggesterStatus where
        parseXML x
          = SuggesterStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

-- | Options for a field that contains an array of text strings. Present if
-- @IndexFieldType@ specifies the field is of type @text-array@. A
-- @text-array@ field is always searchable. All options are enabled by
-- default.
--
-- /See:/ 'textArrayOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'taoSourceFields'
--
-- * 'taoReturnEnabled'
--
-- * 'taoAnalysisScheme'
--
-- * 'taoHighlightEnabled'
--
-- * 'taoDefaultValue'
data TextArrayOptions = TextArrayOptions'{_taoSourceFields :: Maybe Text, _taoReturnEnabled :: Maybe Bool, _taoAnalysisScheme :: Maybe Text, _taoHighlightEnabled :: Maybe Bool, _taoDefaultValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'TextArrayOptions' smart constructor.
textArrayOptions :: TextArrayOptions
textArrayOptions = TextArrayOptions'{_taoSourceFields = Nothing, _taoReturnEnabled = Nothing, _taoAnalysisScheme = Nothing, _taoHighlightEnabled = Nothing, _taoDefaultValue = Nothing};

-- | A list of source fields to map to the field.
taoSourceFields :: Lens' TextArrayOptions (Maybe Text)
taoSourceFields = lens _taoSourceFields (\ s a -> s{_taoSourceFields = a});

-- | Whether the contents of the field can be returned in the search results.
taoReturnEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoReturnEnabled = lens _taoReturnEnabled (\ s a -> s{_taoReturnEnabled = a});

-- | The name of an analysis scheme for a @text-array@ field.
taoAnalysisScheme :: Lens' TextArrayOptions (Maybe Text)
taoAnalysisScheme = lens _taoAnalysisScheme (\ s a -> s{_taoAnalysisScheme = a});

-- | Whether highlights can be returned for the field.
taoHighlightEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoHighlightEnabled = lens _taoHighlightEnabled (\ s a -> s{_taoHighlightEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
taoDefaultValue :: Lens' TextArrayOptions (Maybe Text)
taoDefaultValue = lens _taoDefaultValue (\ s a -> s{_taoDefaultValue = a});

instance FromXML TextArrayOptions where
        parseXML x
          = TextArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "AnalysisScheme")
                <*> (x .@? "HighlightEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery TextArrayOptions where
        toQuery TextArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _taoSourceFields,
               "ReturnEnabled" =: _taoReturnEnabled,
               "AnalysisScheme" =: _taoAnalysisScheme,
               "HighlightEnabled" =: _taoHighlightEnabled,
               "DefaultValue" =: _taoDefaultValue]

-- | Options for text field. Present if @IndexFieldType@ specifies the field
-- is of type @text@. A @text@ field is always searchable. All options are
-- enabled by default.
--
-- /See:/ 'textOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'toSourceField'
--
-- * 'toReturnEnabled'
--
-- * 'toAnalysisScheme'
--
-- * 'toHighlightEnabled'
--
-- * 'toSortEnabled'
--
-- * 'toDefaultValue'
data TextOptions = TextOptions'{_toSourceField :: Maybe Text, _toReturnEnabled :: Maybe Bool, _toAnalysisScheme :: Maybe Text, _toHighlightEnabled :: Maybe Bool, _toSortEnabled :: Maybe Bool, _toDefaultValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'TextOptions' smart constructor.
textOptions :: TextOptions
textOptions = TextOptions'{_toSourceField = Nothing, _toReturnEnabled = Nothing, _toAnalysisScheme = Nothing, _toHighlightEnabled = Nothing, _toSortEnabled = Nothing, _toDefaultValue = Nothing};

-- | FIXME: Undocumented member.
toSourceField :: Lens' TextOptions (Maybe Text)
toSourceField = lens _toSourceField (\ s a -> s{_toSourceField = a});

-- | Whether the contents of the field can be returned in the search results.
toReturnEnabled :: Lens' TextOptions (Maybe Bool)
toReturnEnabled = lens _toReturnEnabled (\ s a -> s{_toReturnEnabled = a});

-- | The name of an analysis scheme for a @text@ field.
toAnalysisScheme :: Lens' TextOptions (Maybe Text)
toAnalysisScheme = lens _toAnalysisScheme (\ s a -> s{_toAnalysisScheme = a});

-- | Whether highlights can be returned for the field.
toHighlightEnabled :: Lens' TextOptions (Maybe Bool)
toHighlightEnabled = lens _toHighlightEnabled (\ s a -> s{_toHighlightEnabled = a});

-- | Whether the field can be used to sort the search results.
toSortEnabled :: Lens' TextOptions (Maybe Bool)
toSortEnabled = lens _toSortEnabled (\ s a -> s{_toSortEnabled = a});

-- | A value to use for the field if the field isn\'t specified for a
-- document.
toDefaultValue :: Lens' TextOptions (Maybe Text)
toDefaultValue = lens _toDefaultValue (\ s a -> s{_toDefaultValue = a});

instance FromXML TextOptions where
        parseXML x
          = TextOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "AnalysisScheme")
                <*> (x .@? "HighlightEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance ToQuery TextOptions where
        toQuery TextOptions'{..}
          = mconcat
              ["SourceField" =: _toSourceField,
               "ReturnEnabled" =: _toReturnEnabled,
               "AnalysisScheme" =: _toAnalysisScheme,
               "HighlightEnabled" =: _toHighlightEnabled,
               "SortEnabled" =: _toSortEnabled,
               "DefaultValue" =: _toDefaultValue]
