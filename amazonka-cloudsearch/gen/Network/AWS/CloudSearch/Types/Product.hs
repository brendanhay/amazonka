{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.Product where

import Network.AWS.CloudSearch.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configured access rules for the domain's document and search endpoints, and the current status of those rules.
--
--
--
-- /See:/ 'accessPoliciesStatus' smart constructor.
data AccessPoliciesStatus = AccessPoliciesStatus'
  { _apsOptions :: !Text
  , _apsStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessPoliciesStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsOptions' - Undocumented member.
--
-- * 'apsStatus' - Undocumented member.
accessPoliciesStatus
    :: Text -- ^ 'apsOptions'
    -> OptionStatus -- ^ 'apsStatus'
    -> AccessPoliciesStatus
accessPoliciesStatus pOptions_ pStatus_ =
  AccessPoliciesStatus' {_apsOptions = pOptions_, _apsStatus = pStatus_}


-- | Undocumented member.
apsOptions :: Lens' AccessPoliciesStatus Text
apsOptions = lens _apsOptions (\ s a -> s{_apsOptions = a})

-- | Undocumented member.
apsStatus :: Lens' AccessPoliciesStatus OptionStatus
apsStatus = lens _apsStatus (\ s a -> s{_apsStatus = a})

instance FromXML AccessPoliciesStatus where
        parseXML x
          = AccessPoliciesStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

instance Hashable AccessPoliciesStatus where

instance NFData AccessPoliciesStatus where

-- | Synonyms, stopwords, and stemming options for an analysis scheme. Includes tokenization dictionary for Japanese.
--
--
--
-- /See:/ 'analysisOptions' smart constructor.
data AnalysisOptions = AnalysisOptions'
  { _aoAlgorithmicStemming            :: !(Maybe AlgorithmicStemming)
  , _aoStopwords                      :: !(Maybe Text)
  , _aoJapaneseTokenizationDictionary :: !(Maybe Text)
  , _aoSynonyms                       :: !(Maybe Text)
  , _aoStemmingDictionary             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalysisOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aoAlgorithmicStemming' - The level of algorithmic stemming to perform: @none@ , @minimal@ , @light@ , or @full@ . The available levels vary depending on the language. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings> in the /Amazon CloudSearch Developer Guide/
--
-- * 'aoStopwords' - A JSON array of terms to ignore during indexing and searching. For example, @["a", "an", "the", "of"]@ . The stopwords dictionary must explicitly list each word you want to ignore. Wildcards and regular expressions are not supported.
--
-- * 'aoJapaneseTokenizationDictionary' - A JSON array that contains a collection of terms, tokens, readings and part of speech for Japanese Tokenizaiton. The Japanese tokenization dictionary enables you to override the default tokenization for selected terms. This is only valid for Japanese language fields.
--
-- * 'aoSynonyms' - A JSON object that defines synonym groups and aliases. A synonym group is an array of arrays, where each sub-array is a group of terms where each term in the group is considered a synonym of every other term in the group. The aliases value is an object that contains a collection of string:value pairs where the string specifies a term and the array of values specifies each of the aliases for that term. An alias is considered a synonym of the specified term, but the term is not considered a synonym of the alias. For more information about specifying synonyms, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms> in the /Amazon CloudSearch Developer Guide/ .
--
-- * 'aoStemmingDictionary' - A JSON object that contains a collection of string:value pairs that each map a term to its stem. For example, @{"term1": "stem1", "term2": "stem2", "term3": "stem3"}@ . The stemming dictionary is applied in addition to any algorithmic stemming. This enables you to override the results of the algorithmic stemming to correct specific cases of overstemming or understemming. The maximum size of a stemming dictionary is 500 KB.
analysisOptions
    :: AnalysisOptions
analysisOptions =
  AnalysisOptions'
    { _aoAlgorithmicStemming = Nothing
    , _aoStopwords = Nothing
    , _aoJapaneseTokenizationDictionary = Nothing
    , _aoSynonyms = Nothing
    , _aoStemmingDictionary = Nothing
    }


-- | The level of algorithmic stemming to perform: @none@ , @minimal@ , @light@ , or @full@ . The available levels vary depending on the language. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings> in the /Amazon CloudSearch Developer Guide/
aoAlgorithmicStemming :: Lens' AnalysisOptions (Maybe AlgorithmicStemming)
aoAlgorithmicStemming = lens _aoAlgorithmicStemming (\ s a -> s{_aoAlgorithmicStemming = a})

-- | A JSON array of terms to ignore during indexing and searching. For example, @["a", "an", "the", "of"]@ . The stopwords dictionary must explicitly list each word you want to ignore. Wildcards and regular expressions are not supported.
aoStopwords :: Lens' AnalysisOptions (Maybe Text)
aoStopwords = lens _aoStopwords (\ s a -> s{_aoStopwords = a})

-- | A JSON array that contains a collection of terms, tokens, readings and part of speech for Japanese Tokenizaiton. The Japanese tokenization dictionary enables you to override the default tokenization for selected terms. This is only valid for Japanese language fields.
aoJapaneseTokenizationDictionary :: Lens' AnalysisOptions (Maybe Text)
aoJapaneseTokenizationDictionary = lens _aoJapaneseTokenizationDictionary (\ s a -> s{_aoJapaneseTokenizationDictionary = a})

-- | A JSON object that defines synonym groups and aliases. A synonym group is an array of arrays, where each sub-array is a group of terms where each term in the group is considered a synonym of every other term in the group. The aliases value is an object that contains a collection of string:value pairs where the string specifies a term and the array of values specifies each of the aliases for that term. An alias is considered a synonym of the specified term, but the term is not considered a synonym of the alias. For more information about specifying synonyms, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms> in the /Amazon CloudSearch Developer Guide/ .
aoSynonyms :: Lens' AnalysisOptions (Maybe Text)
aoSynonyms = lens _aoSynonyms (\ s a -> s{_aoSynonyms = a})

-- | A JSON object that contains a collection of string:value pairs that each map a term to its stem. For example, @{"term1": "stem1", "term2": "stem2", "term3": "stem3"}@ . The stemming dictionary is applied in addition to any algorithmic stemming. This enables you to override the results of the algorithmic stemming to correct specific cases of overstemming or understemming. The maximum size of a stemming dictionary is 500 KB.
aoStemmingDictionary :: Lens' AnalysisOptions (Maybe Text)
aoStemmingDictionary = lens _aoStemmingDictionary (\ s a -> s{_aoStemmingDictionary = a})

instance FromXML AnalysisOptions where
        parseXML x
          = AnalysisOptions' <$>
              (x .@? "AlgorithmicStemming") <*> (x .@? "Stopwords")
                <*> (x .@? "JapaneseTokenizationDictionary")
                <*> (x .@? "Synonyms")
                <*> (x .@? "StemmingDictionary")

instance Hashable AnalysisOptions where

instance NFData AnalysisOptions where

instance ToQuery AnalysisOptions where
        toQuery AnalysisOptions'{..}
          = mconcat
              ["AlgorithmicStemming" =: _aoAlgorithmicStemming,
               "Stopwords" =: _aoStopwords,
               "JapaneseTokenizationDictionary" =:
                 _aoJapaneseTokenizationDictionary,
               "Synonyms" =: _aoSynonyms,
               "StemmingDictionary" =: _aoStemmingDictionary]

-- | Configuration information for an analysis scheme. Each analysis scheme has a unique name and specifies the language of the text to be processed. The following options can be configured for an analysis scheme: @Synonyms@ , @Stopwords@ , @StemmingDictionary@ , @JapaneseTokenizationDictionary@ and @AlgorithmicStemming@ .
--
--
--
-- /See:/ 'analysisScheme' smart constructor.
data AnalysisScheme = AnalysisScheme'
  { _asAnalysisOptions        :: !(Maybe AnalysisOptions)
  , _asAnalysisSchemeName     :: !Text
  , _asAnalysisSchemeLanguage :: !AnalysisSchemeLanguage
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalysisScheme' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAnalysisOptions' - Undocumented member.
--
-- * 'asAnalysisSchemeName' - Undocumented member.
--
-- * 'asAnalysisSchemeLanguage' - Undocumented member.
analysisScheme
    :: Text -- ^ 'asAnalysisSchemeName'
    -> AnalysisSchemeLanguage -- ^ 'asAnalysisSchemeLanguage'
    -> AnalysisScheme
analysisScheme pAnalysisSchemeName_ pAnalysisSchemeLanguage_ =
  AnalysisScheme'
    { _asAnalysisOptions = Nothing
    , _asAnalysisSchemeName = pAnalysisSchemeName_
    , _asAnalysisSchemeLanguage = pAnalysisSchemeLanguage_
    }


-- | Undocumented member.
asAnalysisOptions :: Lens' AnalysisScheme (Maybe AnalysisOptions)
asAnalysisOptions = lens _asAnalysisOptions (\ s a -> s{_asAnalysisOptions = a})

-- | Undocumented member.
asAnalysisSchemeName :: Lens' AnalysisScheme Text
asAnalysisSchemeName = lens _asAnalysisSchemeName (\ s a -> s{_asAnalysisSchemeName = a})

-- | Undocumented member.
asAnalysisSchemeLanguage :: Lens' AnalysisScheme AnalysisSchemeLanguage
asAnalysisSchemeLanguage = lens _asAnalysisSchemeLanguage (\ s a -> s{_asAnalysisSchemeLanguage = a})

instance FromXML AnalysisScheme where
        parseXML x
          = AnalysisScheme' <$>
              (x .@? "AnalysisOptions") <*>
                (x .@ "AnalysisSchemeName")
                <*> (x .@ "AnalysisSchemeLanguage")

instance Hashable AnalysisScheme where

instance NFData AnalysisScheme where

instance ToQuery AnalysisScheme where
        toQuery AnalysisScheme'{..}
          = mconcat
              ["AnalysisOptions" =: _asAnalysisOptions,
               "AnalysisSchemeName" =: _asAnalysisSchemeName,
               "AnalysisSchemeLanguage" =:
                 _asAnalysisSchemeLanguage]

-- | The status and configuration of an @AnalysisScheme@ .
--
--
--
-- /See:/ 'analysisSchemeStatus' smart constructor.
data AnalysisSchemeStatus = AnalysisSchemeStatus'
  { _assOptions :: !AnalysisScheme
  , _assStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalysisSchemeStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assOptions' - Undocumented member.
--
-- * 'assStatus' - Undocumented member.
analysisSchemeStatus
    :: AnalysisScheme -- ^ 'assOptions'
    -> OptionStatus -- ^ 'assStatus'
    -> AnalysisSchemeStatus
analysisSchemeStatus pOptions_ pStatus_ =
  AnalysisSchemeStatus' {_assOptions = pOptions_, _assStatus = pStatus_}


-- | Undocumented member.
assOptions :: Lens' AnalysisSchemeStatus AnalysisScheme
assOptions = lens _assOptions (\ s a -> s{_assOptions = a})

-- | Undocumented member.
assStatus :: Lens' AnalysisSchemeStatus OptionStatus
assStatus = lens _assStatus (\ s a -> s{_assStatus = a})

instance FromXML AnalysisSchemeStatus where
        parseXML x
          = AnalysisSchemeStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

instance Hashable AnalysisSchemeStatus where

instance NFData AnalysisSchemeStatus where

-- | The status and configuration of the domain's availability options.
--
--
--
-- /See:/ 'availabilityOptionsStatus' smart constructor.
data AvailabilityOptionsStatus = AvailabilityOptionsStatus'
  { _aosOptions :: !Bool
  , _aosStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aosOptions' - The availability options configured for the domain.
--
-- * 'aosStatus' - Undocumented member.
availabilityOptionsStatus
    :: Bool -- ^ 'aosOptions'
    -> OptionStatus -- ^ 'aosStatus'
    -> AvailabilityOptionsStatus
availabilityOptionsStatus pOptions_ pStatus_ =
  AvailabilityOptionsStatus' {_aosOptions = pOptions_, _aosStatus = pStatus_}


-- | The availability options configured for the domain.
aosOptions :: Lens' AvailabilityOptionsStatus Bool
aosOptions = lens _aosOptions (\ s a -> s{_aosOptions = a})

-- | Undocumented member.
aosStatus :: Lens' AvailabilityOptionsStatus OptionStatus
aosStatus = lens _aosStatus (\ s a -> s{_aosStatus = a})

instance FromXML AvailabilityOptionsStatus where
        parseXML x
          = AvailabilityOptionsStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

instance Hashable AvailabilityOptionsStatus where

instance NFData AvailabilityOptionsStatus where

-- | Options for a field that contains an array of dates. Present if @IndexFieldType@ specifies the field is of type @date-array@ . All options are enabled by default.
--
--
--
-- /See:/ 'dateArrayOptions' smart constructor.
data DateArrayOptions = DateArrayOptions'
  { _daosSourceFields  :: !(Maybe Text)
  , _daosReturnEnabled :: !(Maybe Bool)
  , _daosFacetEnabled  :: !(Maybe Bool)
  , _daosSearchEnabled :: !(Maybe Bool)
  , _daosDefaultValue  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DateArrayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daosSourceFields' - A list of source fields to map to the field.
--
-- * 'daosReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'daosFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'daosSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'daosDefaultValue' - A value to use for the field if the field isn't specified for a document.
dateArrayOptions
    :: DateArrayOptions
dateArrayOptions =
  DateArrayOptions'
    { _daosSourceFields = Nothing
    , _daosReturnEnabled = Nothing
    , _daosFacetEnabled = Nothing
    , _daosSearchEnabled = Nothing
    , _daosDefaultValue = Nothing
    }


-- | A list of source fields to map to the field.
daosSourceFields :: Lens' DateArrayOptions (Maybe Text)
daosSourceFields = lens _daosSourceFields (\ s a -> s{_daosSourceFields = a})

-- | Whether the contents of the field can be returned in the search results.
daosReturnEnabled :: Lens' DateArrayOptions (Maybe Bool)
daosReturnEnabled = lens _daosReturnEnabled (\ s a -> s{_daosReturnEnabled = a})

-- | Whether facet information can be returned for the field.
daosFacetEnabled :: Lens' DateArrayOptions (Maybe Bool)
daosFacetEnabled = lens _daosFacetEnabled (\ s a -> s{_daosFacetEnabled = a})

-- | Whether the contents of the field are searchable.
daosSearchEnabled :: Lens' DateArrayOptions (Maybe Bool)
daosSearchEnabled = lens _daosSearchEnabled (\ s a -> s{_daosSearchEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
daosDefaultValue :: Lens' DateArrayOptions (Maybe Text)
daosDefaultValue = lens _daosDefaultValue (\ s a -> s{_daosDefaultValue = a})

instance FromXML DateArrayOptions where
        parseXML x
          = DateArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable DateArrayOptions where

instance NFData DateArrayOptions where

instance ToQuery DateArrayOptions where
        toQuery DateArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _daosSourceFields,
               "ReturnEnabled" =: _daosReturnEnabled,
               "FacetEnabled" =: _daosFacetEnabled,
               "SearchEnabled" =: _daosSearchEnabled,
               "DefaultValue" =: _daosDefaultValue]

-- | Options for a date field. Dates and times are specified in UTC (Coordinated Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if @IndexFieldType@ specifies the field is of type @date@ . All options are enabled by default.
--
--
--
-- /See:/ 'dateOptions' smart constructor.
data DateOptions = DateOptions'
  { _doSourceField   :: !(Maybe Text)
  , _doReturnEnabled :: !(Maybe Bool)
  , _doFacetEnabled  :: !(Maybe Bool)
  , _doSearchEnabled :: !(Maybe Bool)
  , _doSortEnabled   :: !(Maybe Bool)
  , _doDefaultValue  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DateOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doSourceField' - Undocumented member.
--
-- * 'doReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'doFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'doSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'doSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'doDefaultValue' - A value to use for the field if the field isn't specified for a document.
dateOptions
    :: DateOptions
dateOptions =
  DateOptions'
    { _doSourceField = Nothing
    , _doReturnEnabled = Nothing
    , _doFacetEnabled = Nothing
    , _doSearchEnabled = Nothing
    , _doSortEnabled = Nothing
    , _doDefaultValue = Nothing
    }


-- | Undocumented member.
doSourceField :: Lens' DateOptions (Maybe Text)
doSourceField = lens _doSourceField (\ s a -> s{_doSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
doReturnEnabled :: Lens' DateOptions (Maybe Bool)
doReturnEnabled = lens _doReturnEnabled (\ s a -> s{_doReturnEnabled = a})

-- | Whether facet information can be returned for the field.
doFacetEnabled :: Lens' DateOptions (Maybe Bool)
doFacetEnabled = lens _doFacetEnabled (\ s a -> s{_doFacetEnabled = a})

-- | Whether the contents of the field are searchable.
doSearchEnabled :: Lens' DateOptions (Maybe Bool)
doSearchEnabled = lens _doSearchEnabled (\ s a -> s{_doSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
doSortEnabled :: Lens' DateOptions (Maybe Bool)
doSortEnabled = lens _doSortEnabled (\ s a -> s{_doSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
doDefaultValue :: Lens' DateOptions (Maybe Text)
doDefaultValue = lens _doDefaultValue (\ s a -> s{_doDefaultValue = a})

instance FromXML DateOptions where
        parseXML x
          = DateOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable DateOptions where

instance NFData DateOptions where

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
--
--
-- /See:/ 'documentSuggesterOptions' smart constructor.
data DocumentSuggesterOptions = DocumentSuggesterOptions'
  { _dsoSortExpression :: !(Maybe Text)
  , _dsoFuzzyMatching  :: !(Maybe SuggesterFuzzyMatching)
  , _dsoSourceField    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentSuggesterOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsoSortExpression' - An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
--
-- * 'dsoFuzzyMatching' - The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
--
-- * 'dsoSourceField' - The name of the index field you want to use for suggestions.
documentSuggesterOptions
    :: Text -- ^ 'dsoSourceField'
    -> DocumentSuggesterOptions
documentSuggesterOptions pSourceField_ =
  DocumentSuggesterOptions'
    { _dsoSortExpression = Nothing
    , _dsoFuzzyMatching = Nothing
    , _dsoSourceField = pSourceField_
    }


-- | An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
dsoSortExpression :: Lens' DocumentSuggesterOptions (Maybe Text)
dsoSortExpression = lens _dsoSortExpression (\ s a -> s{_dsoSortExpression = a})

-- | The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
dsoFuzzyMatching :: Lens' DocumentSuggesterOptions (Maybe SuggesterFuzzyMatching)
dsoFuzzyMatching = lens _dsoFuzzyMatching (\ s a -> s{_dsoFuzzyMatching = a})

-- | The name of the index field you want to use for suggestions.
dsoSourceField :: Lens' DocumentSuggesterOptions Text
dsoSourceField = lens _dsoSourceField (\ s a -> s{_dsoSourceField = a})

instance FromXML DocumentSuggesterOptions where
        parseXML x
          = DocumentSuggesterOptions' <$>
              (x .@? "SortExpression") <*> (x .@? "FuzzyMatching")
                <*> (x .@ "SourceField")

instance Hashable DocumentSuggesterOptions where

instance NFData DocumentSuggesterOptions where

instance ToQuery DocumentSuggesterOptions where
        toQuery DocumentSuggesterOptions'{..}
          = mconcat
              ["SortExpression" =: _dsoSortExpression,
               "FuzzyMatching" =: _dsoFuzzyMatching,
               "SourceField" =: _dsoSourceField]

-- | The current status of the search domain.
--
--
--
-- /See:/ 'domainStatus' smart constructor.
data DomainStatus = DomainStatus'
  { _dsSearchInstanceCount    :: !(Maybe Nat)
  , _dsSearchInstanceType     :: !(Maybe Text)
  , _dsDocService             :: !(Maybe ServiceEndpoint)
  , _dsARN                    :: !(Maybe Text)
  , _dsCreated                :: !(Maybe Bool)
  , _dsSearchService          :: !(Maybe ServiceEndpoint)
  , _dsLimits                 :: !(Maybe Limits)
  , _dsSearchPartitionCount   :: !(Maybe Nat)
  , _dsDeleted                :: !(Maybe Bool)
  , _dsProcessing             :: !(Maybe Bool)
  , _dsDomainId               :: !Text
  , _dsDomainName             :: !Text
  , _dsRequiresIndexDocuments :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSearchInstanceCount' - The number of search instances that are available to process search requests.
--
-- * 'dsSearchInstanceType' - The instance type that is being used to process search requests.
--
-- * 'dsDocService' - The service endpoint for updating documents in a search domain.
--
-- * 'dsARN' - Undocumented member.
--
-- * 'dsCreated' - True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
--
-- * 'dsSearchService' - The service endpoint for requesting search results from a search domain.
--
-- * 'dsLimits' - Undocumented member.
--
-- * 'dsSearchPartitionCount' - The number of partitions across which the search index is spread.
--
-- * 'dsDeleted' - True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
--
-- * 'dsProcessing' - True if processing is being done to activate the current domain configuration.
--
-- * 'dsDomainId' - Undocumented member.
--
-- * 'dsDomainName' - Undocumented member.
--
-- * 'dsRequiresIndexDocuments' - True if 'IndexDocuments' needs to be called to activate the current domain configuration.
domainStatus
    :: Text -- ^ 'dsDomainId'
    -> Text -- ^ 'dsDomainName'
    -> Bool -- ^ 'dsRequiresIndexDocuments'
    -> DomainStatus
domainStatus pDomainId_ pDomainName_ pRequiresIndexDocuments_ =
  DomainStatus'
    { _dsSearchInstanceCount = Nothing
    , _dsSearchInstanceType = Nothing
    , _dsDocService = Nothing
    , _dsARN = Nothing
    , _dsCreated = Nothing
    , _dsSearchService = Nothing
    , _dsLimits = Nothing
    , _dsSearchPartitionCount = Nothing
    , _dsDeleted = Nothing
    , _dsProcessing = Nothing
    , _dsDomainId = pDomainId_
    , _dsDomainName = pDomainName_
    , _dsRequiresIndexDocuments = pRequiresIndexDocuments_
    }


-- | The number of search instances that are available to process search requests.
dsSearchInstanceCount :: Lens' DomainStatus (Maybe Natural)
dsSearchInstanceCount = lens _dsSearchInstanceCount (\ s a -> s{_dsSearchInstanceCount = a}) . mapping _Nat

-- | The instance type that is being used to process search requests.
dsSearchInstanceType :: Lens' DomainStatus (Maybe Text)
dsSearchInstanceType = lens _dsSearchInstanceType (\ s a -> s{_dsSearchInstanceType = a})

-- | The service endpoint for updating documents in a search domain.
dsDocService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsDocService = lens _dsDocService (\ s a -> s{_dsDocService = a})

-- | Undocumented member.
dsARN :: Lens' DomainStatus (Maybe Text)
dsARN = lens _dsARN (\ s a -> s{_dsARN = a})

-- | True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
dsCreated :: Lens' DomainStatus (Maybe Bool)
dsCreated = lens _dsCreated (\ s a -> s{_dsCreated = a})

-- | The service endpoint for requesting search results from a search domain.
dsSearchService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsSearchService = lens _dsSearchService (\ s a -> s{_dsSearchService = a})

-- | Undocumented member.
dsLimits :: Lens' DomainStatus (Maybe Limits)
dsLimits = lens _dsLimits (\ s a -> s{_dsLimits = a})

-- | The number of partitions across which the search index is spread.
dsSearchPartitionCount :: Lens' DomainStatus (Maybe Natural)
dsSearchPartitionCount = lens _dsSearchPartitionCount (\ s a -> s{_dsSearchPartitionCount = a}) . mapping _Nat

-- | True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
dsDeleted :: Lens' DomainStatus (Maybe Bool)
dsDeleted = lens _dsDeleted (\ s a -> s{_dsDeleted = a})

-- | True if processing is being done to activate the current domain configuration.
dsProcessing :: Lens' DomainStatus (Maybe Bool)
dsProcessing = lens _dsProcessing (\ s a -> s{_dsProcessing = a})

-- | Undocumented member.
dsDomainId :: Lens' DomainStatus Text
dsDomainId = lens _dsDomainId (\ s a -> s{_dsDomainId = a})

-- | Undocumented member.
dsDomainName :: Lens' DomainStatus Text
dsDomainName = lens _dsDomainName (\ s a -> s{_dsDomainName = a})

-- | True if 'IndexDocuments' needs to be called to activate the current domain configuration.
dsRequiresIndexDocuments :: Lens' DomainStatus Bool
dsRequiresIndexDocuments = lens _dsRequiresIndexDocuments (\ s a -> s{_dsRequiresIndexDocuments = a})

instance FromXML DomainStatus where
        parseXML x
          = DomainStatus' <$>
              (x .@? "SearchInstanceCount") <*>
                (x .@? "SearchInstanceType")
                <*> (x .@? "DocService")
                <*> (x .@? "ARN")
                <*> (x .@? "Created")
                <*> (x .@? "SearchService")
                <*> (x .@? "Limits")
                <*> (x .@? "SearchPartitionCount")
                <*> (x .@? "Deleted")
                <*> (x .@? "Processing")
                <*> (x .@ "DomainId")
                <*> (x .@ "DomainName")
                <*> (x .@ "RequiresIndexDocuments")

instance Hashable DomainStatus where

instance NFData DomainStatus where

-- | Options for a field that contains an array of double-precision 64-bit floating point values. Present if @IndexFieldType@ specifies the field is of type @double-array@ . All options are enabled by default.
--
--
--
-- /See:/ 'doubleArrayOptions' smart constructor.
data DoubleArrayOptions = DoubleArrayOptions'
  { _daoSourceFields  :: !(Maybe Text)
  , _daoReturnEnabled :: !(Maybe Bool)
  , _daoFacetEnabled  :: !(Maybe Bool)
  , _daoSearchEnabled :: !(Maybe Bool)
  , _daoDefaultValue  :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DoubleArrayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daoSourceFields' - A list of source fields to map to the field.
--
-- * 'daoReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'daoFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'daoSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'daoDefaultValue' - A value to use for the field if the field isn't specified for a document.
doubleArrayOptions
    :: DoubleArrayOptions
doubleArrayOptions =
  DoubleArrayOptions'
    { _daoSourceFields = Nothing
    , _daoReturnEnabled = Nothing
    , _daoFacetEnabled = Nothing
    , _daoSearchEnabled = Nothing
    , _daoDefaultValue = Nothing
    }


-- | A list of source fields to map to the field.
daoSourceFields :: Lens' DoubleArrayOptions (Maybe Text)
daoSourceFields = lens _daoSourceFields (\ s a -> s{_daoSourceFields = a})

-- | Whether the contents of the field can be returned in the search results.
daoReturnEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoReturnEnabled = lens _daoReturnEnabled (\ s a -> s{_daoReturnEnabled = a})

-- | Whether facet information can be returned for the field.
daoFacetEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoFacetEnabled = lens _daoFacetEnabled (\ s a -> s{_daoFacetEnabled = a})

-- | Whether the contents of the field are searchable.
daoSearchEnabled :: Lens' DoubleArrayOptions (Maybe Bool)
daoSearchEnabled = lens _daoSearchEnabled (\ s a -> s{_daoSearchEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
daoDefaultValue :: Lens' DoubleArrayOptions (Maybe Double)
daoDefaultValue = lens _daoDefaultValue (\ s a -> s{_daoDefaultValue = a})

instance FromXML DoubleArrayOptions where
        parseXML x
          = DoubleArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable DoubleArrayOptions where

instance NFData DoubleArrayOptions where

instance ToQuery DoubleArrayOptions where
        toQuery DoubleArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _daoSourceFields,
               "ReturnEnabled" =: _daoReturnEnabled,
               "FacetEnabled" =: _daoFacetEnabled,
               "SearchEnabled" =: _daoSearchEnabled,
               "DefaultValue" =: _daoDefaultValue]

-- | Options for a double-precision 64-bit floating point field. Present if @IndexFieldType@ specifies the field is of type @double@ . All options are enabled by default.
--
--
--
-- /See:/ 'doubleOptions' smart constructor.
data DoubleOptions = DoubleOptions'
  { _dSourceField   :: !(Maybe Text)
  , _dReturnEnabled :: !(Maybe Bool)
  , _dFacetEnabled  :: !(Maybe Bool)
  , _dSearchEnabled :: !(Maybe Bool)
  , _dSortEnabled   :: !(Maybe Bool)
  , _dDefaultValue  :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DoubleOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSourceField' - The name of the source field to map to the field.
--
-- * 'dReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'dFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'dSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'dSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'dDefaultValue' - A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
doubleOptions
    :: DoubleOptions
doubleOptions =
  DoubleOptions'
    { _dSourceField = Nothing
    , _dReturnEnabled = Nothing
    , _dFacetEnabled = Nothing
    , _dSearchEnabled = Nothing
    , _dSortEnabled = Nothing
    , _dDefaultValue = Nothing
    }


-- | The name of the source field to map to the field.
dSourceField :: Lens' DoubleOptions (Maybe Text)
dSourceField = lens _dSourceField (\ s a -> s{_dSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
dReturnEnabled :: Lens' DoubleOptions (Maybe Bool)
dReturnEnabled = lens _dReturnEnabled (\ s a -> s{_dReturnEnabled = a})

-- | Whether facet information can be returned for the field.
dFacetEnabled :: Lens' DoubleOptions (Maybe Bool)
dFacetEnabled = lens _dFacetEnabled (\ s a -> s{_dFacetEnabled = a})

-- | Whether the contents of the field are searchable.
dSearchEnabled :: Lens' DoubleOptions (Maybe Bool)
dSearchEnabled = lens _dSearchEnabled (\ s a -> s{_dSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
dSortEnabled :: Lens' DoubleOptions (Maybe Bool)
dSortEnabled = lens _dSortEnabled (\ s a -> s{_dSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
dDefaultValue :: Lens' DoubleOptions (Maybe Double)
dDefaultValue = lens _dDefaultValue (\ s a -> s{_dDefaultValue = a})

instance FromXML DoubleOptions where
        parseXML x
          = DoubleOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable DoubleOptions where

instance NFData DoubleOptions where

instance ToQuery DoubleOptions where
        toQuery DoubleOptions'{..}
          = mconcat
              ["SourceField" =: _dSourceField,
               "ReturnEnabled" =: _dReturnEnabled,
               "FacetEnabled" =: _dFacetEnabled,
               "SearchEnabled" =: _dSearchEnabled,
               "SortEnabled" =: _dSortEnabled,
               "DefaultValue" =: _dDefaultValue]

-- | A named expression that can be evaluated at search time. Can be used to sort the search results, define other expressions, or return computed information in the search results.
--
--
--
-- /See:/ 'expression' smart constructor.
data Expression = Expression'
  { _eExpressionName  :: !Text
  , _eExpressionValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Expression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eExpressionName' - Undocumented member.
--
-- * 'eExpressionValue' - Undocumented member.
expression
    :: Text -- ^ 'eExpressionName'
    -> Text -- ^ 'eExpressionValue'
    -> Expression
expression pExpressionName_ pExpressionValue_ =
  Expression'
    {_eExpressionName = pExpressionName_, _eExpressionValue = pExpressionValue_}


-- | Undocumented member.
eExpressionName :: Lens' Expression Text
eExpressionName = lens _eExpressionName (\ s a -> s{_eExpressionName = a})

-- | Undocumented member.
eExpressionValue :: Lens' Expression Text
eExpressionValue = lens _eExpressionValue (\ s a -> s{_eExpressionValue = a})

instance FromXML Expression where
        parseXML x
          = Expression' <$>
              (x .@ "ExpressionName") <*> (x .@ "ExpressionValue")

instance Hashable Expression where

instance NFData Expression where

instance ToQuery Expression where
        toQuery Expression'{..}
          = mconcat
              ["ExpressionName" =: _eExpressionName,
               "ExpressionValue" =: _eExpressionValue]

-- | The value of an @Expression@ and its current status.
--
--
--
-- /See:/ 'expressionStatus' smart constructor.
data ExpressionStatus = ExpressionStatus'
  { _esOptions :: !Expression
  , _esStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExpressionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esOptions' - The expression that is evaluated for sorting while processing a search request.
--
-- * 'esStatus' - Undocumented member.
expressionStatus
    :: Expression -- ^ 'esOptions'
    -> OptionStatus -- ^ 'esStatus'
    -> ExpressionStatus
expressionStatus pOptions_ pStatus_ =
  ExpressionStatus' {_esOptions = pOptions_, _esStatus = pStatus_}


-- | The expression that is evaluated for sorting while processing a search request.
esOptions :: Lens' ExpressionStatus Expression
esOptions = lens _esOptions (\ s a -> s{_esOptions = a})

-- | Undocumented member.
esStatus :: Lens' ExpressionStatus OptionStatus
esStatus = lens _esStatus (\ s a -> s{_esStatus = a})

instance FromXML ExpressionStatus where
        parseXML x
          = ExpressionStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

instance Hashable ExpressionStatus where

instance NFData ExpressionStatus where

-- | Configuration information for a field in the index, including its name, type, and options. The supported options depend on the @'IndexFieldType' @ .
--
--
--
-- /See:/ 'indexField' smart constructor.
data IndexField = IndexField'
  { _ifDoubleArrayOptions  :: !(Maybe DoubleArrayOptions)
  , _ifDateOptions         :: !(Maybe DateOptions)
  , _ifTextArrayOptions    :: !(Maybe TextArrayOptions)
  , _ifDoubleOptions       :: !(Maybe DoubleOptions)
  , _ifTextOptions         :: !(Maybe TextOptions)
  , _ifLatLonOptions       :: !(Maybe LatLonOptions)
  , _ifLiteralArrayOptions :: !(Maybe LiteralArrayOptions)
  , _ifIntArrayOptions     :: !(Maybe IntArrayOptions)
  , _ifDateArrayOptions    :: !(Maybe DateArrayOptions)
  , _ifIntOptions          :: !(Maybe IntOptions)
  , _ifLiteralOptions      :: !(Maybe LiteralOptions)
  , _ifIndexFieldName      :: !Text
  , _ifIndexFieldType      :: !IndexFieldType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IndexField' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifDoubleArrayOptions' - Undocumented member.
--
-- * 'ifDateOptions' - Undocumented member.
--
-- * 'ifTextArrayOptions' - Undocumented member.
--
-- * 'ifDoubleOptions' - Undocumented member.
--
-- * 'ifTextOptions' - Undocumented member.
--
-- * 'ifLatLonOptions' - Undocumented member.
--
-- * 'ifLiteralArrayOptions' - Undocumented member.
--
-- * 'ifIntArrayOptions' - Undocumented member.
--
-- * 'ifDateArrayOptions' - Undocumented member.
--
-- * 'ifIntOptions' - Undocumented member.
--
-- * 'ifLiteralOptions' - Undocumented member.
--
-- * 'ifIndexFieldName' - A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options.  Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported.  The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ .
--
-- * 'ifIndexFieldType' - Undocumented member.
indexField
    :: Text -- ^ 'ifIndexFieldName'
    -> IndexFieldType -- ^ 'ifIndexFieldType'
    -> IndexField
indexField pIndexFieldName_ pIndexFieldType_ =
  IndexField'
    { _ifDoubleArrayOptions = Nothing
    , _ifDateOptions = Nothing
    , _ifTextArrayOptions = Nothing
    , _ifDoubleOptions = Nothing
    , _ifTextOptions = Nothing
    , _ifLatLonOptions = Nothing
    , _ifLiteralArrayOptions = Nothing
    , _ifIntArrayOptions = Nothing
    , _ifDateArrayOptions = Nothing
    , _ifIntOptions = Nothing
    , _ifLiteralOptions = Nothing
    , _ifIndexFieldName = pIndexFieldName_
    , _ifIndexFieldType = pIndexFieldType_
    }


-- | Undocumented member.
ifDoubleArrayOptions :: Lens' IndexField (Maybe DoubleArrayOptions)
ifDoubleArrayOptions = lens _ifDoubleArrayOptions (\ s a -> s{_ifDoubleArrayOptions = a})

-- | Undocumented member.
ifDateOptions :: Lens' IndexField (Maybe DateOptions)
ifDateOptions = lens _ifDateOptions (\ s a -> s{_ifDateOptions = a})

-- | Undocumented member.
ifTextArrayOptions :: Lens' IndexField (Maybe TextArrayOptions)
ifTextArrayOptions = lens _ifTextArrayOptions (\ s a -> s{_ifTextArrayOptions = a})

-- | Undocumented member.
ifDoubleOptions :: Lens' IndexField (Maybe DoubleOptions)
ifDoubleOptions = lens _ifDoubleOptions (\ s a -> s{_ifDoubleOptions = a})

-- | Undocumented member.
ifTextOptions :: Lens' IndexField (Maybe TextOptions)
ifTextOptions = lens _ifTextOptions (\ s a -> s{_ifTextOptions = a})

-- | Undocumented member.
ifLatLonOptions :: Lens' IndexField (Maybe LatLonOptions)
ifLatLonOptions = lens _ifLatLonOptions (\ s a -> s{_ifLatLonOptions = a})

-- | Undocumented member.
ifLiteralArrayOptions :: Lens' IndexField (Maybe LiteralArrayOptions)
ifLiteralArrayOptions = lens _ifLiteralArrayOptions (\ s a -> s{_ifLiteralArrayOptions = a})

-- | Undocumented member.
ifIntArrayOptions :: Lens' IndexField (Maybe IntArrayOptions)
ifIntArrayOptions = lens _ifIntArrayOptions (\ s a -> s{_ifIntArrayOptions = a})

-- | Undocumented member.
ifDateArrayOptions :: Lens' IndexField (Maybe DateArrayOptions)
ifDateArrayOptions = lens _ifDateArrayOptions (\ s a -> s{_ifDateArrayOptions = a})

-- | Undocumented member.
ifIntOptions :: Lens' IndexField (Maybe IntOptions)
ifIntOptions = lens _ifIntOptions (\ s a -> s{_ifIntOptions = a})

-- | Undocumented member.
ifLiteralOptions :: Lens' IndexField (Maybe LiteralOptions)
ifLiteralOptions = lens _ifLiteralOptions (\ s a -> s{_ifLiteralOptions = a})

-- | A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options.  Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported.  The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ .
ifIndexFieldName :: Lens' IndexField Text
ifIndexFieldName = lens _ifIndexFieldName (\ s a -> s{_ifIndexFieldName = a})

-- | Undocumented member.
ifIndexFieldType :: Lens' IndexField IndexFieldType
ifIndexFieldType = lens _ifIndexFieldType (\ s a -> s{_ifIndexFieldType = a})

instance FromXML IndexField where
        parseXML x
          = IndexField' <$>
              (x .@? "DoubleArrayOptions") <*>
                (x .@? "DateOptions")
                <*> (x .@? "TextArrayOptions")
                <*> (x .@? "DoubleOptions")
                <*> (x .@? "TextOptions")
                <*> (x .@? "LatLonOptions")
                <*> (x .@? "LiteralArrayOptions")
                <*> (x .@? "IntArrayOptions")
                <*> (x .@? "DateArrayOptions")
                <*> (x .@? "IntOptions")
                <*> (x .@? "LiteralOptions")
                <*> (x .@ "IndexFieldName")
                <*> (x .@ "IndexFieldType")

instance Hashable IndexField where

instance NFData IndexField where

instance ToQuery IndexField where
        toQuery IndexField'{..}
          = mconcat
              ["DoubleArrayOptions" =: _ifDoubleArrayOptions,
               "DateOptions" =: _ifDateOptions,
               "TextArrayOptions" =: _ifTextArrayOptions,
               "DoubleOptions" =: _ifDoubleOptions,
               "TextOptions" =: _ifTextOptions,
               "LatLonOptions" =: _ifLatLonOptions,
               "LiteralArrayOptions" =: _ifLiteralArrayOptions,
               "IntArrayOptions" =: _ifIntArrayOptions,
               "DateArrayOptions" =: _ifDateArrayOptions,
               "IntOptions" =: _ifIntOptions,
               "LiteralOptions" =: _ifLiteralOptions,
               "IndexFieldName" =: _ifIndexFieldName,
               "IndexFieldType" =: _ifIndexFieldType]

-- | The value of an @IndexField@ and its current status.
--
--
--
-- /See:/ 'indexFieldStatus' smart constructor.
data IndexFieldStatus = IndexFieldStatus'
  { _ifsOptions :: !IndexField
  , _ifsStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IndexFieldStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifsOptions' - Undocumented member.
--
-- * 'ifsStatus' - Undocumented member.
indexFieldStatus
    :: IndexField -- ^ 'ifsOptions'
    -> OptionStatus -- ^ 'ifsStatus'
    -> IndexFieldStatus
indexFieldStatus pOptions_ pStatus_ =
  IndexFieldStatus' {_ifsOptions = pOptions_, _ifsStatus = pStatus_}


-- | Undocumented member.
ifsOptions :: Lens' IndexFieldStatus IndexField
ifsOptions = lens _ifsOptions (\ s a -> s{_ifsOptions = a})

-- | Undocumented member.
ifsStatus :: Lens' IndexFieldStatus OptionStatus
ifsStatus = lens _ifsStatus (\ s a -> s{_ifsStatus = a})

instance FromXML IndexFieldStatus where
        parseXML x
          = IndexFieldStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

instance Hashable IndexFieldStatus where

instance NFData IndexFieldStatus where

-- | Options for a field that contains an array of 64-bit signed integers. Present if @IndexFieldType@ specifies the field is of type @int-array@ . All options are enabled by default.
--
--
--
-- /See:/ 'intArrayOptions' smart constructor.
data IntArrayOptions = IntArrayOptions'
  { _iaoSourceFields  :: !(Maybe Text)
  , _iaoReturnEnabled :: !(Maybe Bool)
  , _iaoFacetEnabled  :: !(Maybe Bool)
  , _iaoSearchEnabled :: !(Maybe Bool)
  , _iaoDefaultValue  :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntArrayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaoSourceFields' - A list of source fields to map to the field.
--
-- * 'iaoReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'iaoFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'iaoSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'iaoDefaultValue' - A value to use for the field if the field isn't specified for a document.
intArrayOptions
    :: IntArrayOptions
intArrayOptions =
  IntArrayOptions'
    { _iaoSourceFields = Nothing
    , _iaoReturnEnabled = Nothing
    , _iaoFacetEnabled = Nothing
    , _iaoSearchEnabled = Nothing
    , _iaoDefaultValue = Nothing
    }


-- | A list of source fields to map to the field.
iaoSourceFields :: Lens' IntArrayOptions (Maybe Text)
iaoSourceFields = lens _iaoSourceFields (\ s a -> s{_iaoSourceFields = a})

-- | Whether the contents of the field can be returned in the search results.
iaoReturnEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoReturnEnabled = lens _iaoReturnEnabled (\ s a -> s{_iaoReturnEnabled = a})

-- | Whether facet information can be returned for the field.
iaoFacetEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoFacetEnabled = lens _iaoFacetEnabled (\ s a -> s{_iaoFacetEnabled = a})

-- | Whether the contents of the field are searchable.
iaoSearchEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoSearchEnabled = lens _iaoSearchEnabled (\ s a -> s{_iaoSearchEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
iaoDefaultValue :: Lens' IntArrayOptions (Maybe Integer)
iaoDefaultValue = lens _iaoDefaultValue (\ s a -> s{_iaoDefaultValue = a})

instance FromXML IntArrayOptions where
        parseXML x
          = IntArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable IntArrayOptions where

instance NFData IntArrayOptions where

instance ToQuery IntArrayOptions where
        toQuery IntArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _iaoSourceFields,
               "ReturnEnabled" =: _iaoReturnEnabled,
               "FacetEnabled" =: _iaoFacetEnabled,
               "SearchEnabled" =: _iaoSearchEnabled,
               "DefaultValue" =: _iaoDefaultValue]

-- | Options for a 64-bit signed integer field. Present if @IndexFieldType@ specifies the field is of type @int@ . All options are enabled by default.
--
--
--
-- /See:/ 'intOptions' smart constructor.
data IntOptions = IntOptions'
  { _ioSourceField   :: !(Maybe Text)
  , _ioReturnEnabled :: !(Maybe Bool)
  , _ioFacetEnabled  :: !(Maybe Bool)
  , _ioSearchEnabled :: !(Maybe Bool)
  , _ioSortEnabled   :: !(Maybe Bool)
  , _ioDefaultValue  :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ioSourceField' - The name of the source field to map to the field.
--
-- * 'ioReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'ioFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'ioSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'ioSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'ioDefaultValue' - A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
intOptions
    :: IntOptions
intOptions =
  IntOptions'
    { _ioSourceField = Nothing
    , _ioReturnEnabled = Nothing
    , _ioFacetEnabled = Nothing
    , _ioSearchEnabled = Nothing
    , _ioSortEnabled = Nothing
    , _ioDefaultValue = Nothing
    }


-- | The name of the source field to map to the field.
ioSourceField :: Lens' IntOptions (Maybe Text)
ioSourceField = lens _ioSourceField (\ s a -> s{_ioSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
ioReturnEnabled :: Lens' IntOptions (Maybe Bool)
ioReturnEnabled = lens _ioReturnEnabled (\ s a -> s{_ioReturnEnabled = a})

-- | Whether facet information can be returned for the field.
ioFacetEnabled :: Lens' IntOptions (Maybe Bool)
ioFacetEnabled = lens _ioFacetEnabled (\ s a -> s{_ioFacetEnabled = a})

-- | Whether the contents of the field are searchable.
ioSearchEnabled :: Lens' IntOptions (Maybe Bool)
ioSearchEnabled = lens _ioSearchEnabled (\ s a -> s{_ioSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
ioSortEnabled :: Lens' IntOptions (Maybe Bool)
ioSortEnabled = lens _ioSortEnabled (\ s a -> s{_ioSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
ioDefaultValue :: Lens' IntOptions (Maybe Integer)
ioDefaultValue = lens _ioDefaultValue (\ s a -> s{_ioDefaultValue = a})

instance FromXML IntOptions where
        parseXML x
          = IntOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable IntOptions where

instance NFData IntOptions where

instance ToQuery IntOptions where
        toQuery IntOptions'{..}
          = mconcat
              ["SourceField" =: _ioSourceField,
               "ReturnEnabled" =: _ioReturnEnabled,
               "FacetEnabled" =: _ioFacetEnabled,
               "SearchEnabled" =: _ioSearchEnabled,
               "SortEnabled" =: _ioSortEnabled,
               "DefaultValue" =: _ioDefaultValue]

-- | Options for a latlon field. A latlon field contains a location stored as a latitude and longitude value pair. Present if @IndexFieldType@ specifies the field is of type @latlon@ . All options are enabled by default.
--
--
--
-- /See:/ 'latLonOptions' smart constructor.
data LatLonOptions = LatLonOptions'
  { _lloSourceField   :: !(Maybe Text)
  , _lloReturnEnabled :: !(Maybe Bool)
  , _lloFacetEnabled  :: !(Maybe Bool)
  , _lloSearchEnabled :: !(Maybe Bool)
  , _lloSortEnabled   :: !(Maybe Bool)
  , _lloDefaultValue  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LatLonOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lloSourceField' - Undocumented member.
--
-- * 'lloReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'lloFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'lloSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'lloSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'lloDefaultValue' - A value to use for the field if the field isn't specified for a document.
latLonOptions
    :: LatLonOptions
latLonOptions =
  LatLonOptions'
    { _lloSourceField = Nothing
    , _lloReturnEnabled = Nothing
    , _lloFacetEnabled = Nothing
    , _lloSearchEnabled = Nothing
    , _lloSortEnabled = Nothing
    , _lloDefaultValue = Nothing
    }


-- | Undocumented member.
lloSourceField :: Lens' LatLonOptions (Maybe Text)
lloSourceField = lens _lloSourceField (\ s a -> s{_lloSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
lloReturnEnabled :: Lens' LatLonOptions (Maybe Bool)
lloReturnEnabled = lens _lloReturnEnabled (\ s a -> s{_lloReturnEnabled = a})

-- | Whether facet information can be returned for the field.
lloFacetEnabled :: Lens' LatLonOptions (Maybe Bool)
lloFacetEnabled = lens _lloFacetEnabled (\ s a -> s{_lloFacetEnabled = a})

-- | Whether the contents of the field are searchable.
lloSearchEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSearchEnabled = lens _lloSearchEnabled (\ s a -> s{_lloSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
lloSortEnabled :: Lens' LatLonOptions (Maybe Bool)
lloSortEnabled = lens _lloSortEnabled (\ s a -> s{_lloSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
lloDefaultValue :: Lens' LatLonOptions (Maybe Text)
lloDefaultValue = lens _lloDefaultValue (\ s a -> s{_lloDefaultValue = a})

instance FromXML LatLonOptions where
        parseXML x
          = LatLonOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable LatLonOptions where

instance NFData LatLonOptions where

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
data Limits = Limits'
  { _lMaximumReplicationCount :: !Nat
  , _lMaximumPartitionCount   :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Limits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMaximumReplicationCount' - Undocumented member.
--
-- * 'lMaximumPartitionCount' - Undocumented member.
limits
    :: Natural -- ^ 'lMaximumReplicationCount'
    -> Natural -- ^ 'lMaximumPartitionCount'
    -> Limits
limits pMaximumReplicationCount_ pMaximumPartitionCount_ =
  Limits'
    { _lMaximumReplicationCount = _Nat # pMaximumReplicationCount_
    , _lMaximumPartitionCount = _Nat # pMaximumPartitionCount_
    }


-- | Undocumented member.
lMaximumReplicationCount :: Lens' Limits Natural
lMaximumReplicationCount = lens _lMaximumReplicationCount (\ s a -> s{_lMaximumReplicationCount = a}) . _Nat

-- | Undocumented member.
lMaximumPartitionCount :: Lens' Limits Natural
lMaximumPartitionCount = lens _lMaximumPartitionCount (\ s a -> s{_lMaximumPartitionCount = a}) . _Nat

instance FromXML Limits where
        parseXML x
          = Limits' <$>
              (x .@ "MaximumReplicationCount") <*>
                (x .@ "MaximumPartitionCount")

instance Hashable Limits where

instance NFData Limits where

-- | Options for a field that contains an array of literal strings. Present if @IndexFieldType@ specifies the field is of type @literal-array@ . All options are enabled by default.
--
--
--
-- /See:/ 'literalArrayOptions' smart constructor.
data LiteralArrayOptions = LiteralArrayOptions'
  { _laoSourceFields  :: !(Maybe Text)
  , _laoReturnEnabled :: !(Maybe Bool)
  , _laoFacetEnabled  :: !(Maybe Bool)
  , _laoSearchEnabled :: !(Maybe Bool)
  , _laoDefaultValue  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LiteralArrayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laoSourceFields' - A list of source fields to map to the field.
--
-- * 'laoReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'laoFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'laoSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'laoDefaultValue' - A value to use for the field if the field isn't specified for a document.
literalArrayOptions
    :: LiteralArrayOptions
literalArrayOptions =
  LiteralArrayOptions'
    { _laoSourceFields = Nothing
    , _laoReturnEnabled = Nothing
    , _laoFacetEnabled = Nothing
    , _laoSearchEnabled = Nothing
    , _laoDefaultValue = Nothing
    }


-- | A list of source fields to map to the field.
laoSourceFields :: Lens' LiteralArrayOptions (Maybe Text)
laoSourceFields = lens _laoSourceFields (\ s a -> s{_laoSourceFields = a})

-- | Whether the contents of the field can be returned in the search results.
laoReturnEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoReturnEnabled = lens _laoReturnEnabled (\ s a -> s{_laoReturnEnabled = a})

-- | Whether facet information can be returned for the field.
laoFacetEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoFacetEnabled = lens _laoFacetEnabled (\ s a -> s{_laoFacetEnabled = a})

-- | Whether the contents of the field are searchable.
laoSearchEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoSearchEnabled = lens _laoSearchEnabled (\ s a -> s{_laoSearchEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
laoDefaultValue :: Lens' LiteralArrayOptions (Maybe Text)
laoDefaultValue = lens _laoDefaultValue (\ s a -> s{_laoDefaultValue = a})

instance FromXML LiteralArrayOptions where
        parseXML x
          = LiteralArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable LiteralArrayOptions where

instance NFData LiteralArrayOptions where

instance ToQuery LiteralArrayOptions where
        toQuery LiteralArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _laoSourceFields,
               "ReturnEnabled" =: _laoReturnEnabled,
               "FacetEnabled" =: _laoFacetEnabled,
               "SearchEnabled" =: _laoSearchEnabled,
               "DefaultValue" =: _laoDefaultValue]

-- | Options for literal field. Present if @IndexFieldType@ specifies the field is of type @literal@ . All options are enabled by default.
--
--
--
-- /See:/ 'literalOptions' smart constructor.
data LiteralOptions = LiteralOptions'
  { _loSourceField   :: !(Maybe Text)
  , _loReturnEnabled :: !(Maybe Bool)
  , _loFacetEnabled  :: !(Maybe Bool)
  , _loSearchEnabled :: !(Maybe Bool)
  , _loSortEnabled   :: !(Maybe Bool)
  , _loDefaultValue  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LiteralOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loSourceField' - Undocumented member.
--
-- * 'loReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'loFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'loSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'loSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'loDefaultValue' - A value to use for the field if the field isn't specified for a document.
literalOptions
    :: LiteralOptions
literalOptions =
  LiteralOptions'
    { _loSourceField = Nothing
    , _loReturnEnabled = Nothing
    , _loFacetEnabled = Nothing
    , _loSearchEnabled = Nothing
    , _loSortEnabled = Nothing
    , _loDefaultValue = Nothing
    }


-- | Undocumented member.
loSourceField :: Lens' LiteralOptions (Maybe Text)
loSourceField = lens _loSourceField (\ s a -> s{_loSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
loReturnEnabled :: Lens' LiteralOptions (Maybe Bool)
loReturnEnabled = lens _loReturnEnabled (\ s a -> s{_loReturnEnabled = a})

-- | Whether facet information can be returned for the field.
loFacetEnabled :: Lens' LiteralOptions (Maybe Bool)
loFacetEnabled = lens _loFacetEnabled (\ s a -> s{_loFacetEnabled = a})

-- | Whether the contents of the field are searchable.
loSearchEnabled :: Lens' LiteralOptions (Maybe Bool)
loSearchEnabled = lens _loSearchEnabled (\ s a -> s{_loSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
loSortEnabled :: Lens' LiteralOptions (Maybe Bool)
loSortEnabled = lens _loSortEnabled (\ s a -> s{_loSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
loDefaultValue :: Lens' LiteralOptions (Maybe Text)
loDefaultValue = lens _loDefaultValue (\ s a -> s{_loDefaultValue = a})

instance FromXML LiteralOptions where
        parseXML x
          = LiteralOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "FacetEnabled")
                <*> (x .@? "SearchEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable LiteralOptions where

instance NFData LiteralOptions where

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
--
--
-- /See:/ 'optionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { _osPendingDeletion :: !(Maybe Bool)
  , _osUpdateVersion   :: !(Maybe Nat)
  , _osCreationDate    :: !ISO8601
  , _osUpdateDate      :: !ISO8601
  , _osState           :: !OptionState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osPendingDeletion' - Indicates that the option will be deleted once processing is complete.
--
-- * 'osUpdateVersion' - A unique integer that indicates when this option was last updated.
--
-- * 'osCreationDate' - A timestamp for when this option was created.
--
-- * 'osUpdateDate' - A timestamp for when this option was last updated.
--
-- * 'osState' - The state of processing a change to an option. Possible values:     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.    * @Processing@ : the option's latest value is in the process of being activated.     * @Active@ : the option's latest value is completely deployed.    * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
optionStatus
    :: UTCTime -- ^ 'osCreationDate'
    -> UTCTime -- ^ 'osUpdateDate'
    -> OptionState -- ^ 'osState'
    -> OptionStatus
optionStatus pCreationDate_ pUpdateDate_ pState_ =
  OptionStatus'
    { _osPendingDeletion = Nothing
    , _osUpdateVersion = Nothing
    , _osCreationDate = _Time # pCreationDate_
    , _osUpdateDate = _Time # pUpdateDate_
    , _osState = pState_
    }


-- | Indicates that the option will be deleted once processing is complete.
osPendingDeletion :: Lens' OptionStatus (Maybe Bool)
osPendingDeletion = lens _osPendingDeletion (\ s a -> s{_osPendingDeletion = a})

-- | A unique integer that indicates when this option was last updated.
osUpdateVersion :: Lens' OptionStatus (Maybe Natural)
osUpdateVersion = lens _osUpdateVersion (\ s a -> s{_osUpdateVersion = a}) . mapping _Nat

-- | A timestamp for when this option was created.
osCreationDate :: Lens' OptionStatus UTCTime
osCreationDate = lens _osCreationDate (\ s a -> s{_osCreationDate = a}) . _Time

-- | A timestamp for when this option was last updated.
osUpdateDate :: Lens' OptionStatus UTCTime
osUpdateDate = lens _osUpdateDate (\ s a -> s{_osUpdateDate = a}) . _Time

-- | The state of processing a change to an option. Possible values:     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.    * @Processing@ : the option's latest value is in the process of being activated.     * @Active@ : the option's latest value is completely deployed.    * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
osState :: Lens' OptionStatus OptionState
osState = lens _osState (\ s a -> s{_osState = a})

instance FromXML OptionStatus where
        parseXML x
          = OptionStatus' <$>
              (x .@? "PendingDeletion") <*> (x .@? "UpdateVersion")
                <*> (x .@ "CreationDate")
                <*> (x .@ "UpdateDate")
                <*> (x .@ "State")

instance Hashable OptionStatus where

instance NFData OptionStatus where

-- | The desired instance type and desired number of replicas of each index partition.
--
--
--
-- /See:/ 'scalingParameters' smart constructor.
data ScalingParameters = ScalingParameters'
  { _spDesiredInstanceType     :: !(Maybe PartitionInstanceType)
  , _spDesiredReplicationCount :: !(Maybe Nat)
  , _spDesiredPartitionCount   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spDesiredInstanceType' - The instance type that you want to preconfigure for your domain. For example, @search.m1.small@ .
--
-- * 'spDesiredReplicationCount' - The number of replicas you want to preconfigure for each index partition.
--
-- * 'spDesiredPartitionCount' - The number of partitions you want to preconfigure for your domain. Only valid when you select @m2.2xlarge@ as the desired instance type.
scalingParameters
    :: ScalingParameters
scalingParameters =
  ScalingParameters'
    { _spDesiredInstanceType = Nothing
    , _spDesiredReplicationCount = Nothing
    , _spDesiredPartitionCount = Nothing
    }


-- | The instance type that you want to preconfigure for your domain. For example, @search.m1.small@ .
spDesiredInstanceType :: Lens' ScalingParameters (Maybe PartitionInstanceType)
spDesiredInstanceType = lens _spDesiredInstanceType (\ s a -> s{_spDesiredInstanceType = a})

-- | The number of replicas you want to preconfigure for each index partition.
spDesiredReplicationCount :: Lens' ScalingParameters (Maybe Natural)
spDesiredReplicationCount = lens _spDesiredReplicationCount (\ s a -> s{_spDesiredReplicationCount = a}) . mapping _Nat

-- | The number of partitions you want to preconfigure for your domain. Only valid when you select @m2.2xlarge@ as the desired instance type.
spDesiredPartitionCount :: Lens' ScalingParameters (Maybe Natural)
spDesiredPartitionCount = lens _spDesiredPartitionCount (\ s a -> s{_spDesiredPartitionCount = a}) . mapping _Nat

instance FromXML ScalingParameters where
        parseXML x
          = ScalingParameters' <$>
              (x .@? "DesiredInstanceType") <*>
                (x .@? "DesiredReplicationCount")
                <*> (x .@? "DesiredPartitionCount")

instance Hashable ScalingParameters where

instance NFData ScalingParameters where

instance ToQuery ScalingParameters where
        toQuery ScalingParameters'{..}
          = mconcat
              ["DesiredInstanceType" =: _spDesiredInstanceType,
               "DesiredReplicationCount" =:
                 _spDesiredReplicationCount,
               "DesiredPartitionCount" =: _spDesiredPartitionCount]

-- | The status and configuration of a search domain's scaling parameters.
--
--
--
-- /See:/ 'scalingParametersStatus' smart constructor.
data ScalingParametersStatus = ScalingParametersStatus'
  { _spsOptions :: !ScalingParameters
  , _spsStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingParametersStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spsOptions' - Undocumented member.
--
-- * 'spsStatus' - Undocumented member.
scalingParametersStatus
    :: ScalingParameters -- ^ 'spsOptions'
    -> OptionStatus -- ^ 'spsStatus'
    -> ScalingParametersStatus
scalingParametersStatus pOptions_ pStatus_ =
  ScalingParametersStatus' {_spsOptions = pOptions_, _spsStatus = pStatus_}


-- | Undocumented member.
spsOptions :: Lens' ScalingParametersStatus ScalingParameters
spsOptions = lens _spsOptions (\ s a -> s{_spsOptions = a})

-- | Undocumented member.
spsStatus :: Lens' ScalingParametersStatus OptionStatus
spsStatus = lens _spsStatus (\ s a -> s{_spsStatus = a})

instance FromXML ScalingParametersStatus where
        parseXML x
          = ScalingParametersStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

instance Hashable ScalingParametersStatus where

instance NFData ScalingParametersStatus where

-- | The endpoint to which service requests can be submitted.
--
--
--
-- /See:/ 'serviceEndpoint' smart constructor.
newtype ServiceEndpoint = ServiceEndpoint'
  { _seEndpoint :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seEndpoint' - Undocumented member.
serviceEndpoint
    :: ServiceEndpoint
serviceEndpoint = ServiceEndpoint' {_seEndpoint = Nothing}


-- | Undocumented member.
seEndpoint :: Lens' ServiceEndpoint (Maybe Text)
seEndpoint = lens _seEndpoint (\ s a -> s{_seEndpoint = a})

instance FromXML ServiceEndpoint where
        parseXML x = ServiceEndpoint' <$> (x .@? "Endpoint")

instance Hashable ServiceEndpoint where

instance NFData ServiceEndpoint where

-- | Configuration information for a search suggester. Each suggester has a unique name and specifies the text field you want to use for suggestions. The following options can be configured for a suggester: @FuzzyMatching@ , @SortExpression@ .
--
--
--
-- /See:/ 'suggester' smart constructor.
data Suggester = Suggester'
  { _sSuggesterName            :: !Text
  , _sDocumentSuggesterOptions :: !DocumentSuggesterOptions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Suggester' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSuggesterName' - Undocumented member.
--
-- * 'sDocumentSuggesterOptions' - Undocumented member.
suggester
    :: Text -- ^ 'sSuggesterName'
    -> DocumentSuggesterOptions -- ^ 'sDocumentSuggesterOptions'
    -> Suggester
suggester pSuggesterName_ pDocumentSuggesterOptions_ =
  Suggester'
    { _sSuggesterName = pSuggesterName_
    , _sDocumentSuggesterOptions = pDocumentSuggesterOptions_
    }


-- | Undocumented member.
sSuggesterName :: Lens' Suggester Text
sSuggesterName = lens _sSuggesterName (\ s a -> s{_sSuggesterName = a})

-- | Undocumented member.
sDocumentSuggesterOptions :: Lens' Suggester DocumentSuggesterOptions
sDocumentSuggesterOptions = lens _sDocumentSuggesterOptions (\ s a -> s{_sDocumentSuggesterOptions = a})

instance FromXML Suggester where
        parseXML x
          = Suggester' <$>
              (x .@ "SuggesterName") <*>
                (x .@ "DocumentSuggesterOptions")

instance Hashable Suggester where

instance NFData Suggester where

instance ToQuery Suggester where
        toQuery Suggester'{..}
          = mconcat
              ["SuggesterName" =: _sSuggesterName,
               "DocumentSuggesterOptions" =:
                 _sDocumentSuggesterOptions]

-- | The value of a @Suggester@ and its current status.
--
--
--
-- /See:/ 'suggesterStatus' smart constructor.
data SuggesterStatus = SuggesterStatus'
  { _ssOptions :: !Suggester
  , _ssStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuggesterStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssOptions' - Undocumented member.
--
-- * 'ssStatus' - Undocumented member.
suggesterStatus
    :: Suggester -- ^ 'ssOptions'
    -> OptionStatus -- ^ 'ssStatus'
    -> SuggesterStatus
suggesterStatus pOptions_ pStatus_ =
  SuggesterStatus' {_ssOptions = pOptions_, _ssStatus = pStatus_}


-- | Undocumented member.
ssOptions :: Lens' SuggesterStatus Suggester
ssOptions = lens _ssOptions (\ s a -> s{_ssOptions = a})

-- | Undocumented member.
ssStatus :: Lens' SuggesterStatus OptionStatus
ssStatus = lens _ssStatus (\ s a -> s{_ssStatus = a})

instance FromXML SuggesterStatus where
        parseXML x
          = SuggesterStatus' <$>
              (x .@ "Options") <*> (x .@ "Status")

instance Hashable SuggesterStatus where

instance NFData SuggesterStatus where

-- | Options for a field that contains an array of text strings. Present if @IndexFieldType@ specifies the field is of type @text-array@ . A @text-array@ field is always searchable. All options are enabled by default.
--
--
--
-- /See:/ 'textArrayOptions' smart constructor.
data TextArrayOptions = TextArrayOptions'
  { _taoSourceFields     :: !(Maybe Text)
  , _taoReturnEnabled    :: !(Maybe Bool)
  , _taoAnalysisScheme   :: !(Maybe Text)
  , _taoHighlightEnabled :: !(Maybe Bool)
  , _taoDefaultValue     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TextArrayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taoSourceFields' - A list of source fields to map to the field.
--
-- * 'taoReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'taoAnalysisScheme' - The name of an analysis scheme for a @text-array@ field.
--
-- * 'taoHighlightEnabled' - Whether highlights can be returned for the field.
--
-- * 'taoDefaultValue' - A value to use for the field if the field isn't specified for a document.
textArrayOptions
    :: TextArrayOptions
textArrayOptions =
  TextArrayOptions'
    { _taoSourceFields = Nothing
    , _taoReturnEnabled = Nothing
    , _taoAnalysisScheme = Nothing
    , _taoHighlightEnabled = Nothing
    , _taoDefaultValue = Nothing
    }


-- | A list of source fields to map to the field.
taoSourceFields :: Lens' TextArrayOptions (Maybe Text)
taoSourceFields = lens _taoSourceFields (\ s a -> s{_taoSourceFields = a})

-- | Whether the contents of the field can be returned in the search results.
taoReturnEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoReturnEnabled = lens _taoReturnEnabled (\ s a -> s{_taoReturnEnabled = a})

-- | The name of an analysis scheme for a @text-array@ field.
taoAnalysisScheme :: Lens' TextArrayOptions (Maybe Text)
taoAnalysisScheme = lens _taoAnalysisScheme (\ s a -> s{_taoAnalysisScheme = a})

-- | Whether highlights can be returned for the field.
taoHighlightEnabled :: Lens' TextArrayOptions (Maybe Bool)
taoHighlightEnabled = lens _taoHighlightEnabled (\ s a -> s{_taoHighlightEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
taoDefaultValue :: Lens' TextArrayOptions (Maybe Text)
taoDefaultValue = lens _taoDefaultValue (\ s a -> s{_taoDefaultValue = a})

instance FromXML TextArrayOptions where
        parseXML x
          = TextArrayOptions' <$>
              (x .@? "SourceFields") <*> (x .@? "ReturnEnabled")
                <*> (x .@? "AnalysisScheme")
                <*> (x .@? "HighlightEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable TextArrayOptions where

instance NFData TextArrayOptions where

instance ToQuery TextArrayOptions where
        toQuery TextArrayOptions'{..}
          = mconcat
              ["SourceFields" =: _taoSourceFields,
               "ReturnEnabled" =: _taoReturnEnabled,
               "AnalysisScheme" =: _taoAnalysisScheme,
               "HighlightEnabled" =: _taoHighlightEnabled,
               "DefaultValue" =: _taoDefaultValue]

-- | Options for text field. Present if @IndexFieldType@ specifies the field is of type @text@ . A @text@ field is always searchable. All options are enabled by default.
--
--
--
-- /See:/ 'textOptions' smart constructor.
data TextOptions = TextOptions'
  { _toSourceField      :: !(Maybe Text)
  , _toReturnEnabled    :: !(Maybe Bool)
  , _toAnalysisScheme   :: !(Maybe Text)
  , _toHighlightEnabled :: !(Maybe Bool)
  , _toSortEnabled      :: !(Maybe Bool)
  , _toDefaultValue     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TextOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toSourceField' - Undocumented member.
--
-- * 'toReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'toAnalysisScheme' - The name of an analysis scheme for a @text@ field.
--
-- * 'toHighlightEnabled' - Whether highlights can be returned for the field.
--
-- * 'toSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'toDefaultValue' - A value to use for the field if the field isn't specified for a document.
textOptions
    :: TextOptions
textOptions =
  TextOptions'
    { _toSourceField = Nothing
    , _toReturnEnabled = Nothing
    , _toAnalysisScheme = Nothing
    , _toHighlightEnabled = Nothing
    , _toSortEnabled = Nothing
    , _toDefaultValue = Nothing
    }


-- | Undocumented member.
toSourceField :: Lens' TextOptions (Maybe Text)
toSourceField = lens _toSourceField (\ s a -> s{_toSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
toReturnEnabled :: Lens' TextOptions (Maybe Bool)
toReturnEnabled = lens _toReturnEnabled (\ s a -> s{_toReturnEnabled = a})

-- | The name of an analysis scheme for a @text@ field.
toAnalysisScheme :: Lens' TextOptions (Maybe Text)
toAnalysisScheme = lens _toAnalysisScheme (\ s a -> s{_toAnalysisScheme = a})

-- | Whether highlights can be returned for the field.
toHighlightEnabled :: Lens' TextOptions (Maybe Bool)
toHighlightEnabled = lens _toHighlightEnabled (\ s a -> s{_toHighlightEnabled = a})

-- | Whether the field can be used to sort the search results.
toSortEnabled :: Lens' TextOptions (Maybe Bool)
toSortEnabled = lens _toSortEnabled (\ s a -> s{_toSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
toDefaultValue :: Lens' TextOptions (Maybe Text)
toDefaultValue = lens _toDefaultValue (\ s a -> s{_toDefaultValue = a})

instance FromXML TextOptions where
        parseXML x
          = TextOptions' <$>
              (x .@? "SourceField") <*> (x .@? "ReturnEnabled") <*>
                (x .@? "AnalysisScheme")
                <*> (x .@? "HighlightEnabled")
                <*> (x .@? "SortEnabled")
                <*> (x .@? "DefaultValue")

instance Hashable TextOptions where

instance NFData TextOptions where

instance ToQuery TextOptions where
        toQuery TextOptions'{..}
          = mconcat
              ["SourceField" =: _toSourceField,
               "ReturnEnabled" =: _toReturnEnabled,
               "AnalysisScheme" =: _toAnalysisScheme,
               "HighlightEnabled" =: _toHighlightEnabled,
               "SortEnabled" =: _toSortEnabled,
               "DefaultValue" =: _toDefaultValue]
