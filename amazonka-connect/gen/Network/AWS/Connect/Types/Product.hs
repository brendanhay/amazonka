{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Product where

import Network.AWS.Connect.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The credentials to use for federation.
--
--
--
-- /See:/ 'credentials' smart constructor.
data Credentials = Credentials'
  { _cAccessTokenExpiration  :: !(Maybe POSIX)
  , _cAccessToken            :: !(Maybe (Sensitive Text))
  , _cRefreshToken           :: !(Maybe (Sensitive Text))
  , _cRefreshTokenExpiration :: !(Maybe POSIX)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Credentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAccessTokenExpiration' - A token generated with an expiration time for the session a user is logged in to Amazon Connect
--
-- * 'cAccessToken' - An access token generated for a federated user to access Amazon Connect
--
-- * 'cRefreshToken' - Renews a token generated for a user to access the Amazon Connect instance.
--
-- * 'cRefreshTokenExpiration' - Renews the expiration timer for a generated token.
credentials
    :: Credentials
credentials =
  Credentials'
    { _cAccessTokenExpiration = Nothing
    , _cAccessToken = Nothing
    , _cRefreshToken = Nothing
    , _cRefreshTokenExpiration = Nothing
    }


-- | A token generated with an expiration time for the session a user is logged in to Amazon Connect
cAccessTokenExpiration :: Lens' Credentials (Maybe UTCTime)
cAccessTokenExpiration = lens _cAccessTokenExpiration (\ s a -> s{_cAccessTokenExpiration = a}) . mapping _Time

-- | An access token generated for a federated user to access Amazon Connect
cAccessToken :: Lens' Credentials (Maybe Text)
cAccessToken = lens _cAccessToken (\ s a -> s{_cAccessToken = a}) . mapping _Sensitive

-- | Renews a token generated for a user to access the Amazon Connect instance.
cRefreshToken :: Lens' Credentials (Maybe Text)
cRefreshToken = lens _cRefreshToken (\ s a -> s{_cRefreshToken = a}) . mapping _Sensitive

-- | Renews the expiration timer for a generated token.
cRefreshTokenExpiration :: Lens' Credentials (Maybe UTCTime)
cRefreshTokenExpiration = lens _cRefreshTokenExpiration (\ s a -> s{_cRefreshTokenExpiration = a}) . mapping _Time

instance FromJSON Credentials where
        parseJSON
          = withObject "Credentials"
              (\ x ->
                 Credentials' <$>
                   (x .:? "AccessTokenExpiration") <*>
                     (x .:? "AccessToken")
                     <*> (x .:? "RefreshToken")
                     <*> (x .:? "RefreshTokenExpiration"))

instance Hashable Credentials where

instance NFData Credentials where

-- | A @CurrentMetric@ object that contains the Name and Unit for the metric.
--
--
--
-- /See:/ 'currentMetric' smart constructor.
data CurrentMetric = CurrentMetric'
  { _cmName :: !(Maybe CurrentMetricName)
  , _cmUnit :: !(Maybe Unit)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CurrentMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmName' - The name of the metric.
--
-- * 'cmUnit' - The unit for the metric.
currentMetric
    :: CurrentMetric
currentMetric = CurrentMetric' {_cmName = Nothing, _cmUnit = Nothing}


-- | The name of the metric.
cmName :: Lens' CurrentMetric (Maybe CurrentMetricName)
cmName = lens _cmName (\ s a -> s{_cmName = a})

-- | The unit for the metric.
cmUnit :: Lens' CurrentMetric (Maybe Unit)
cmUnit = lens _cmUnit (\ s a -> s{_cmUnit = a})

instance FromJSON CurrentMetric where
        parseJSON
          = withObject "CurrentMetric"
              (\ x ->
                 CurrentMetric' <$> (x .:? "Name") <*> (x .:? "Unit"))

instance Hashable CurrentMetric where

instance NFData CurrentMetric where

instance ToJSON CurrentMetric where
        toJSON CurrentMetric'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _cmName, ("Unit" .=) <$> _cmUnit])

-- | A @CurrentMetricData@ object.
--
--
--
-- /See:/ 'currentMetricData' smart constructor.
data CurrentMetricData = CurrentMetricData'
  { _cmdValue  :: !(Maybe Double)
  , _cmdMetric :: !(Maybe CurrentMetric)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CurrentMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmdValue' - The value of the metric in the CurrentMetricData object.
--
-- * 'cmdMetric' - The metric in a @CurrentMetricData@ object.
currentMetricData
    :: CurrentMetricData
currentMetricData =
  CurrentMetricData' {_cmdValue = Nothing, _cmdMetric = Nothing}


-- | The value of the metric in the CurrentMetricData object.
cmdValue :: Lens' CurrentMetricData (Maybe Double)
cmdValue = lens _cmdValue (\ s a -> s{_cmdValue = a})

-- | The metric in a @CurrentMetricData@ object.
cmdMetric :: Lens' CurrentMetricData (Maybe CurrentMetric)
cmdMetric = lens _cmdMetric (\ s a -> s{_cmdMetric = a})

instance FromJSON CurrentMetricData where
        parseJSON
          = withObject "CurrentMetricData"
              (\ x ->
                 CurrentMetricData' <$>
                   (x .:? "Value") <*> (x .:? "Metric"))

instance Hashable CurrentMetricData where

instance NFData CurrentMetricData where

-- | A @CurrentMetricResult@ object.
--
--
--
-- /See:/ 'currentMetricResult' smart constructor.
data CurrentMetricResult = CurrentMetricResult'
  { _cmrCollections :: !(Maybe [CurrentMetricData])
  , _cmrDimensions  :: !(Maybe Dimensions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CurrentMetricResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrCollections' - The @Collections@ for the @CurrentMetricResult@ object.
--
-- * 'cmrDimensions' - The @Dimensions@ for the @CurrentMetricResult@ object.
currentMetricResult
    :: CurrentMetricResult
currentMetricResult =
  CurrentMetricResult' {_cmrCollections = Nothing, _cmrDimensions = Nothing}


-- | The @Collections@ for the @CurrentMetricResult@ object.
cmrCollections :: Lens' CurrentMetricResult [CurrentMetricData]
cmrCollections = lens _cmrCollections (\ s a -> s{_cmrCollections = a}) . _Default . _Coerce

-- | The @Dimensions@ for the @CurrentMetricResult@ object.
cmrDimensions :: Lens' CurrentMetricResult (Maybe Dimensions)
cmrDimensions = lens _cmrDimensions (\ s a -> s{_cmrDimensions = a})

instance FromJSON CurrentMetricResult where
        parseJSON
          = withObject "CurrentMetricResult"
              (\ x ->
                 CurrentMetricResult' <$>
                   (x .:? "Collections" .!= mempty) <*>
                     (x .:? "Dimensions"))

instance Hashable CurrentMetricResult where

instance NFData CurrentMetricResult where

-- | A @Dimensions@ object that includes the Channel and Queue for the metric.
--
--
--
-- /See:/ 'dimensions' smart constructor.
data Dimensions = Dimensions'
  { _dChannel :: !(Maybe Channel)
  , _dQueue   :: !(Maybe QueueReference)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Dimensions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dChannel' - The channel used for grouping and filters. Only VOICE is supported.
--
-- * 'dQueue' - A @QueueReference@ object used as one part of dimension for the metrics results.
dimensions
    :: Dimensions
dimensions = Dimensions' {_dChannel = Nothing, _dQueue = Nothing}


-- | The channel used for grouping and filters. Only VOICE is supported.
dChannel :: Lens' Dimensions (Maybe Channel)
dChannel = lens _dChannel (\ s a -> s{_dChannel = a})

-- | A @QueueReference@ object used as one part of dimension for the metrics results.
dQueue :: Lens' Dimensions (Maybe QueueReference)
dQueue = lens _dQueue (\ s a -> s{_dQueue = a})

instance FromJSON Dimensions where
        parseJSON
          = withObject "Dimensions"
              (\ x ->
                 Dimensions' <$>
                   (x .:? "Channel") <*> (x .:? "Queue"))

instance Hashable Dimensions where

instance NFData Dimensions where

-- | The filter, either channel or queues, to apply to the metric results retrieved.
--
--
--
-- /See:/ 'filters' smart constructor.
data Filters = Filters'
  { _fQueues   :: !(Maybe (List1 Text))
  , _fChannels :: !(Maybe [Channel])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fQueues' - A list of up to 100 queue IDs or queue ARNs to use to filter the metrics retrieved. You can include both IDs and ARNs in a request.
--
-- * 'fChannels' - The Channel to use as a filter for the metrics returned. Only VOICE is supported.
filters
    :: Filters
filters = Filters' {_fQueues = Nothing, _fChannels = Nothing}


-- | A list of up to 100 queue IDs or queue ARNs to use to filter the metrics retrieved. You can include both IDs and ARNs in a request.
fQueues :: Lens' Filters (Maybe (NonEmpty Text))
fQueues = lens _fQueues (\ s a -> s{_fQueues = a}) . mapping _List1

-- | The Channel to use as a filter for the metrics returned. Only VOICE is supported.
fChannels :: Lens' Filters [Channel]
fChannels = lens _fChannels (\ s a -> s{_fChannels = a}) . _Default . _Coerce

instance Hashable Filters where

instance NFData Filters where

instance ToJSON Filters where
        toJSON Filters'{..}
          = object
              (catMaybes
                 [("Queues" .=) <$> _fQueues,
                  ("Channels" .=) <$> _fChannels])

-- | A @HierarchyGroup@ object that contains information about a hierarchy group in your Amazon Connect instance.
--
--
--
-- /See:/ 'hierarchyGroup' smart constructor.
data HierarchyGroup = HierarchyGroup'
  { _hgARN           :: !(Maybe Text)
  , _hgName          :: !(Maybe Text)
  , _hgHierarchyPath :: !(Maybe HierarchyPath)
  , _hgId            :: !(Maybe Text)
  , _hgLevelId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HierarchyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hgARN' - The Amazon Resource Name (ARN) for the hierarchy group.
--
-- * 'hgName' - The name of the hierarchy group in your instance.
--
-- * 'hgHierarchyPath' - A @HierarchyPath@ object that contains information about the levels in the hierarchy group.
--
-- * 'hgId' - The identifier for the hierarchy group.
--
-- * 'hgLevelId' - The identifier for the level in the hierarchy group.
hierarchyGroup
    :: HierarchyGroup
hierarchyGroup =
  HierarchyGroup'
    { _hgARN = Nothing
    , _hgName = Nothing
    , _hgHierarchyPath = Nothing
    , _hgId = Nothing
    , _hgLevelId = Nothing
    }


-- | The Amazon Resource Name (ARN) for the hierarchy group.
hgARN :: Lens' HierarchyGroup (Maybe Text)
hgARN = lens _hgARN (\ s a -> s{_hgARN = a})

-- | The name of the hierarchy group in your instance.
hgName :: Lens' HierarchyGroup (Maybe Text)
hgName = lens _hgName (\ s a -> s{_hgName = a})

-- | A @HierarchyPath@ object that contains information about the levels in the hierarchy group.
hgHierarchyPath :: Lens' HierarchyGroup (Maybe HierarchyPath)
hgHierarchyPath = lens _hgHierarchyPath (\ s a -> s{_hgHierarchyPath = a})

-- | The identifier for the hierarchy group.
hgId :: Lens' HierarchyGroup (Maybe Text)
hgId = lens _hgId (\ s a -> s{_hgId = a})

-- | The identifier for the level in the hierarchy group.
hgLevelId :: Lens' HierarchyGroup (Maybe Text)
hgLevelId = lens _hgLevelId (\ s a -> s{_hgLevelId = a})

instance FromJSON HierarchyGroup where
        parseJSON
          = withObject "HierarchyGroup"
              (\ x ->
                 HierarchyGroup' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*>
                     (x .:? "HierarchyPath")
                     <*> (x .:? "Id")
                     <*> (x .:? "LevelId"))

instance Hashable HierarchyGroup where

instance NFData HierarchyGroup where

-- | A @HierarchyGroupSummary@ object that contains information about the hierarchy group, including ARN, Id, and Name.
--
--
--
-- /See:/ 'hierarchyGroupSummary' smart constructor.
data HierarchyGroupSummary = HierarchyGroupSummary'
  { _hgsARN  :: !(Maybe Text)
  , _hgsName :: !(Maybe Text)
  , _hgsId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HierarchyGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hgsARN' - The ARN for the hierarchy group.
--
-- * 'hgsName' - The name of the hierarchy group.
--
-- * 'hgsId' - The identifier of the hierarchy group.
hierarchyGroupSummary
    :: HierarchyGroupSummary
hierarchyGroupSummary =
  HierarchyGroupSummary'
    {_hgsARN = Nothing, _hgsName = Nothing, _hgsId = Nothing}


-- | The ARN for the hierarchy group.
hgsARN :: Lens' HierarchyGroupSummary (Maybe Text)
hgsARN = lens _hgsARN (\ s a -> s{_hgsARN = a})

-- | The name of the hierarchy group.
hgsName :: Lens' HierarchyGroupSummary (Maybe Text)
hgsName = lens _hgsName (\ s a -> s{_hgsName = a})

-- | The identifier of the hierarchy group.
hgsId :: Lens' HierarchyGroupSummary (Maybe Text)
hgsId = lens _hgsId (\ s a -> s{_hgsId = a})

instance FromJSON HierarchyGroupSummary where
        parseJSON
          = withObject "HierarchyGroupSummary"
              (\ x ->
                 HierarchyGroupSummary' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id"))

instance Hashable HierarchyGroupSummary where

instance NFData HierarchyGroupSummary where

-- | A @HierarchyLevel@ object that contains information about the levels in a hierarchy group, including ARN, Id, and Name.
--
--
--
-- /See:/ 'hierarchyLevel' smart constructor.
data HierarchyLevel = HierarchyLevel'
  { _hlARN  :: !(Maybe Text)
  , _hlName :: !(Maybe Text)
  , _hlId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HierarchyLevel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlARN' - The ARN for the hierarchy group level.
--
-- * 'hlName' - The name of the hierarchy group level.
--
-- * 'hlId' - The identifier for the hierarchy group level.
hierarchyLevel
    :: HierarchyLevel
hierarchyLevel =
  HierarchyLevel' {_hlARN = Nothing, _hlName = Nothing, _hlId = Nothing}


-- | The ARN for the hierarchy group level.
hlARN :: Lens' HierarchyLevel (Maybe Text)
hlARN = lens _hlARN (\ s a -> s{_hlARN = a})

-- | The name of the hierarchy group level.
hlName :: Lens' HierarchyLevel (Maybe Text)
hlName = lens _hlName (\ s a -> s{_hlName = a})

-- | The identifier for the hierarchy group level.
hlId :: Lens' HierarchyLevel (Maybe Text)
hlId = lens _hlId (\ s a -> s{_hlId = a})

instance FromJSON HierarchyLevel where
        parseJSON
          = withObject "HierarchyLevel"
              (\ x ->
                 HierarchyLevel' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id"))

instance Hashable HierarchyLevel where

instance NFData HierarchyLevel where

-- | A @HierarchyPath@ object that contains information about the levels of the hierarchy group.
--
--
--
-- /See:/ 'hierarchyPath' smart constructor.
data HierarchyPath = HierarchyPath'
  { _hpLevelFive  :: !(Maybe HierarchyGroupSummary)
  , _hpLevelThree :: !(Maybe HierarchyGroupSummary)
  , _hpLevelFour  :: !(Maybe HierarchyGroupSummary)
  , _hpLevelTwo   :: !(Maybe HierarchyGroupSummary)
  , _hpLevelOne   :: !(Maybe HierarchyGroupSummary)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HierarchyPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpLevelFive' - A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
--
-- * 'hpLevelThree' - A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
--
-- * 'hpLevelFour' - A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
--
-- * 'hpLevelTwo' - A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
--
-- * 'hpLevelOne' - A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
hierarchyPath
    :: HierarchyPath
hierarchyPath =
  HierarchyPath'
    { _hpLevelFive = Nothing
    , _hpLevelThree = Nothing
    , _hpLevelFour = Nothing
    , _hpLevelTwo = Nothing
    , _hpLevelOne = Nothing
    }


-- | A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
hpLevelFive :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelFive = lens _hpLevelFive (\ s a -> s{_hpLevelFive = a})

-- | A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
hpLevelThree :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelThree = lens _hpLevelThree (\ s a -> s{_hpLevelThree = a})

-- | A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
hpLevelFour :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelFour = lens _hpLevelFour (\ s a -> s{_hpLevelFour = a})

-- | A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
hpLevelTwo :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelTwo = lens _hpLevelTwo (\ s a -> s{_hpLevelTwo = a})

-- | A @HierarchyGroupSummary@ object that contains information about the level of the hierarchy group, including ARN, Id, and Name.
hpLevelOne :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelOne = lens _hpLevelOne (\ s a -> s{_hpLevelOne = a})

instance FromJSON HierarchyPath where
        parseJSON
          = withObject "HierarchyPath"
              (\ x ->
                 HierarchyPath' <$>
                   (x .:? "LevelFive") <*> (x .:? "LevelThree") <*>
                     (x .:? "LevelFour")
                     <*> (x .:? "LevelTwo")
                     <*> (x .:? "LevelOne"))

instance Hashable HierarchyPath where

instance NFData HierarchyPath where

-- | A @HierarchyStructure@ object that contains information about the hierarchy group structure.
--
--
--
-- /See:/ 'hierarchyStructure' smart constructor.
data HierarchyStructure = HierarchyStructure'
  { _hsLevelFive  :: !(Maybe HierarchyLevel)
  , _hsLevelThree :: !(Maybe HierarchyLevel)
  , _hsLevelFour  :: !(Maybe HierarchyLevel)
  , _hsLevelTwo   :: !(Maybe HierarchyLevel)
  , _hsLevelOne   :: !(Maybe HierarchyLevel)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HierarchyStructure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsLevelFive' - A @HierarchyLevel@ object that contains information about the hierarchy group level.
--
-- * 'hsLevelThree' - A @HierarchyLevel@ object that contains information about the hierarchy group level.
--
-- * 'hsLevelFour' - A @HierarchyLevel@ object that contains information about the hierarchy group level.
--
-- * 'hsLevelTwo' - A @HierarchyLevel@ object that contains information about the hierarchy group level.
--
-- * 'hsLevelOne' - A @HierarchyLevel@ object that contains information about the hierarchy group level.
hierarchyStructure
    :: HierarchyStructure
hierarchyStructure =
  HierarchyStructure'
    { _hsLevelFive = Nothing
    , _hsLevelThree = Nothing
    , _hsLevelFour = Nothing
    , _hsLevelTwo = Nothing
    , _hsLevelOne = Nothing
    }


-- | A @HierarchyLevel@ object that contains information about the hierarchy group level.
hsLevelFive :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelFive = lens _hsLevelFive (\ s a -> s{_hsLevelFive = a})

-- | A @HierarchyLevel@ object that contains information about the hierarchy group level.
hsLevelThree :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelThree = lens _hsLevelThree (\ s a -> s{_hsLevelThree = a})

-- | A @HierarchyLevel@ object that contains information about the hierarchy group level.
hsLevelFour :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelFour = lens _hsLevelFour (\ s a -> s{_hsLevelFour = a})

-- | A @HierarchyLevel@ object that contains information about the hierarchy group level.
hsLevelTwo :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelTwo = lens _hsLevelTwo (\ s a -> s{_hsLevelTwo = a})

-- | A @HierarchyLevel@ object that contains information about the hierarchy group level.
hsLevelOne :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelOne = lens _hsLevelOne (\ s a -> s{_hsLevelOne = a})

instance FromJSON HierarchyStructure where
        parseJSON
          = withObject "HierarchyStructure"
              (\ x ->
                 HierarchyStructure' <$>
                   (x .:? "LevelFive") <*> (x .:? "LevelThree") <*>
                     (x .:? "LevelFour")
                     <*> (x .:? "LevelTwo")
                     <*> (x .:? "LevelOne"))

instance Hashable HierarchyStructure where

instance NFData HierarchyStructure where

-- | A @HistoricalMetric@ object that contains the Name, Unit, Statistic, and Threshold for the metric.
--
--
--
-- /See:/ 'historicalMetric' smart constructor.
data HistoricalMetric = HistoricalMetric'
  { _hmName      :: !(Maybe HistoricalMetricName)
  , _hmThreshold :: !(Maybe Threshold)
  , _hmUnit      :: !(Maybe Unit)
  , _hmStatistic :: !(Maybe Statistic)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HistoricalMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmName' - The name of the historical metric.
--
-- * 'hmThreshold' - The threshold for the metric, used with service level metrics.
--
-- * 'hmUnit' - The unit for the metric.
--
-- * 'hmStatistic' - The statistic for the metric.
historicalMetric
    :: HistoricalMetric
historicalMetric =
  HistoricalMetric'
    { _hmName = Nothing
    , _hmThreshold = Nothing
    , _hmUnit = Nothing
    , _hmStatistic = Nothing
    }


-- | The name of the historical metric.
hmName :: Lens' HistoricalMetric (Maybe HistoricalMetricName)
hmName = lens _hmName (\ s a -> s{_hmName = a})

-- | The threshold for the metric, used with service level metrics.
hmThreshold :: Lens' HistoricalMetric (Maybe Threshold)
hmThreshold = lens _hmThreshold (\ s a -> s{_hmThreshold = a})

-- | The unit for the metric.
hmUnit :: Lens' HistoricalMetric (Maybe Unit)
hmUnit = lens _hmUnit (\ s a -> s{_hmUnit = a})

-- | The statistic for the metric.
hmStatistic :: Lens' HistoricalMetric (Maybe Statistic)
hmStatistic = lens _hmStatistic (\ s a -> s{_hmStatistic = a})

instance FromJSON HistoricalMetric where
        parseJSON
          = withObject "HistoricalMetric"
              (\ x ->
                 HistoricalMetric' <$>
                   (x .:? "Name") <*> (x .:? "Threshold") <*>
                     (x .:? "Unit")
                     <*> (x .:? "Statistic"))

instance Hashable HistoricalMetric where

instance NFData HistoricalMetric where

instance ToJSON HistoricalMetric where
        toJSON HistoricalMetric'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _hmName,
                  ("Threshold" .=) <$> _hmThreshold,
                  ("Unit" .=) <$> _hmUnit,
                  ("Statistic" .=) <$> _hmStatistic])

-- | A @HistoricalMetricData@ object than contains a @Metric@ and a @Value@ .
--
--
--
-- /See:/ 'historicalMetricData' smart constructor.
data HistoricalMetricData = HistoricalMetricData'
  { _hmdValue  :: !(Maybe Double)
  , _hmdMetric :: !(Maybe HistoricalMetric)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HistoricalMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmdValue' - The @Value@ of the metric.
--
-- * 'hmdMetric' - A @HistoricalMetric@ object.
historicalMetricData
    :: HistoricalMetricData
historicalMetricData =
  HistoricalMetricData' {_hmdValue = Nothing, _hmdMetric = Nothing}


-- | The @Value@ of the metric.
hmdValue :: Lens' HistoricalMetricData (Maybe Double)
hmdValue = lens _hmdValue (\ s a -> s{_hmdValue = a})

-- | A @HistoricalMetric@ object.
hmdMetric :: Lens' HistoricalMetricData (Maybe HistoricalMetric)
hmdMetric = lens _hmdMetric (\ s a -> s{_hmdMetric = a})

instance FromJSON HistoricalMetricData where
        parseJSON
          = withObject "HistoricalMetricData"
              (\ x ->
                 HistoricalMetricData' <$>
                   (x .:? "Value") <*> (x .:? "Metric"))

instance Hashable HistoricalMetricData where

instance NFData HistoricalMetricData where

-- | The metrics data returned from a @GetMetricData@ operation.
--
--
--
-- /See:/ 'historicalMetricResult' smart constructor.
data HistoricalMetricResult = HistoricalMetricResult'
  { _hmrCollections :: !(Maybe [HistoricalMetricData])
  , _hmrDimensions  :: !(Maybe Dimensions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HistoricalMetricResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmrCollections' - A list of @HistoricalMetricData@ objects.
--
-- * 'hmrDimensions' - The @Dimensions@ for the metrics.
historicalMetricResult
    :: HistoricalMetricResult
historicalMetricResult =
  HistoricalMetricResult' {_hmrCollections = Nothing, _hmrDimensions = Nothing}


-- | A list of @HistoricalMetricData@ objects.
hmrCollections :: Lens' HistoricalMetricResult [HistoricalMetricData]
hmrCollections = lens _hmrCollections (\ s a -> s{_hmrCollections = a}) . _Default . _Coerce

-- | The @Dimensions@ for the metrics.
hmrDimensions :: Lens' HistoricalMetricResult (Maybe Dimensions)
hmrDimensions = lens _hmrDimensions (\ s a -> s{_hmrDimensions = a})

instance FromJSON HistoricalMetricResult where
        parseJSON
          = withObject "HistoricalMetricResult"
              (\ x ->
                 HistoricalMetricResult' <$>
                   (x .:? "Collections" .!= mempty) <*>
                     (x .:? "Dimensions"))

instance Hashable HistoricalMetricResult where

instance NFData HistoricalMetricResult where

-- | A QueueReference object that contains the the QueueId and ARN for the queue resource for which metrics are returned.
--
--
--
-- /See:/ 'queueReference' smart constructor.
data QueueReference = QueueReference'
  { _qrARN :: !(Maybe Text)
  , _qrId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueueReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qrARN' - The Amazon Resource Name (ARN) of queue.
--
-- * 'qrId' - The ID of the queue associated with the metrics returned.
queueReference
    :: QueueReference
queueReference = QueueReference' {_qrARN = Nothing, _qrId = Nothing}


-- | The Amazon Resource Name (ARN) of queue.
qrARN :: Lens' QueueReference (Maybe Text)
qrARN = lens _qrARN (\ s a -> s{_qrARN = a})

-- | The ID of the queue associated with the metrics returned.
qrId :: Lens' QueueReference (Maybe Text)
qrId = lens _qrId (\ s a -> s{_qrId = a})

instance FromJSON QueueReference where
        parseJSON
          = withObject "QueueReference"
              (\ x ->
                 QueueReference' <$> (x .:? "Arn") <*> (x .:? "Id"))

instance Hashable QueueReference where

instance NFData QueueReference where

-- | A @RoutingProfileSummary@ object that contains information about a routing profile, including ARN, Id, and Name.
--
--
--
-- /See:/ 'routingProfileSummary' smart constructor.
data RoutingProfileSummary = RoutingProfileSummary'
  { _rpsARN  :: !(Maybe Text)
  , _rpsName :: !(Maybe Text)
  , _rpsId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoutingProfileSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsARN' - The ARN of the routing profile.
--
-- * 'rpsName' - The name of the routing profile.
--
-- * 'rpsId' - The identifier of the routing profile.
routingProfileSummary
    :: RoutingProfileSummary
routingProfileSummary =
  RoutingProfileSummary'
    {_rpsARN = Nothing, _rpsName = Nothing, _rpsId = Nothing}


-- | The ARN of the routing profile.
rpsARN :: Lens' RoutingProfileSummary (Maybe Text)
rpsARN = lens _rpsARN (\ s a -> s{_rpsARN = a})

-- | The name of the routing profile.
rpsName :: Lens' RoutingProfileSummary (Maybe Text)
rpsName = lens _rpsName (\ s a -> s{_rpsName = a})

-- | The identifier of the routing profile.
rpsId :: Lens' RoutingProfileSummary (Maybe Text)
rpsId = lens _rpsId (\ s a -> s{_rpsId = a})

instance FromJSON RoutingProfileSummary where
        parseJSON
          = withObject "RoutingProfileSummary"
              (\ x ->
                 RoutingProfileSummary' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id"))

instance Hashable RoutingProfileSummary where

instance NFData RoutingProfileSummary where

-- | A @SecurityProfileSummary@ object that contains information about a security profile, including ARN, Id, Name.
--
--
--
-- /See:/ 'securityProfileSummary' smart constructor.
data SecurityProfileSummary = SecurityProfileSummary'
  { _spsARN  :: !(Maybe Text)
  , _spsName :: !(Maybe Text)
  , _spsId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecurityProfileSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spsARN' - The ARN of the security profile.
--
-- * 'spsName' - The name of the security profile.
--
-- * 'spsId' - The identifier of the security profile.
securityProfileSummary
    :: SecurityProfileSummary
securityProfileSummary =
  SecurityProfileSummary'
    {_spsARN = Nothing, _spsName = Nothing, _spsId = Nothing}


-- | The ARN of the security profile.
spsARN :: Lens' SecurityProfileSummary (Maybe Text)
spsARN = lens _spsARN (\ s a -> s{_spsARN = a})

-- | The name of the security profile.
spsName :: Lens' SecurityProfileSummary (Maybe Text)
spsName = lens _spsName (\ s a -> s{_spsName = a})

-- | The identifier of the security profile.
spsId :: Lens' SecurityProfileSummary (Maybe Text)
spsId = lens _spsId (\ s a -> s{_spsId = a})

instance FromJSON SecurityProfileSummary where
        parseJSON
          = withObject "SecurityProfileSummary"
              (\ x ->
                 SecurityProfileSummary' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id"))

instance Hashable SecurityProfileSummary where

instance NFData SecurityProfileSummary where

-- | A @Threshold@ object that includes a comparison and @ThresholdValue@ to compare to. Used with service level metrics.
--
--
--
-- /See:/ 'threshold' smart constructor.
data Threshold = Threshold'
  { _tThresholdValue :: !(Maybe Double)
  , _tComparison     :: !(Maybe Comparison)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Threshold' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tThresholdValue' - The value of the threshold to compare the metric to. Only "Less than" (LT) comparisons are supported.
--
-- * 'tComparison' - The Threshold to use to compare service level metrics to. Only "Less than" (LT) comparisons are supported.
threshold
    :: Threshold
threshold = Threshold' {_tThresholdValue = Nothing, _tComparison = Nothing}


-- | The value of the threshold to compare the metric to. Only "Less than" (LT) comparisons are supported.
tThresholdValue :: Lens' Threshold (Maybe Double)
tThresholdValue = lens _tThresholdValue (\ s a -> s{_tThresholdValue = a})

-- | The Threshold to use to compare service level metrics to. Only "Less than" (LT) comparisons are supported.
tComparison :: Lens' Threshold (Maybe Comparison)
tComparison = lens _tComparison (\ s a -> s{_tComparison = a})

instance FromJSON Threshold where
        parseJSON
          = withObject "Threshold"
              (\ x ->
                 Threshold' <$>
                   (x .:? "ThresholdValue") <*> (x .:? "Comparison"))

instance Hashable Threshold where

instance NFData Threshold where

instance ToJSON Threshold where
        toJSON Threshold'{..}
          = object
              (catMaybes
                 [("ThresholdValue" .=) <$> _tThresholdValue,
                  ("Comparison" .=) <$> _tComparison])

-- | A @User@ object that contains information about a user account in your Amazon Connect instance, including configuration settings.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uRoutingProfileId   :: !(Maybe Text)
  , _uDirectoryUserId    :: !(Maybe Text)
  , _uARN                :: !(Maybe Text)
  , _uIdentityInfo       :: !(Maybe UserIdentityInfo)
  , _uSecurityProfileIds :: !(Maybe (List1 Text))
  , _uUsername           :: !(Maybe Text)
  , _uId                 :: !(Maybe Text)
  , _uHierarchyGroupId   :: !(Maybe Text)
  , _uPhoneConfig        :: !(Maybe UserPhoneConfig)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uRoutingProfileId' - The identifier of the routing profile assigned to the user.
--
-- * 'uDirectoryUserId' - The directory Id for the user account in the existing directory used for identity management.
--
-- * 'uARN' - The ARN of the user account.
--
-- * 'uIdentityInfo' - A @UserIdentityInfo@ object.
--
-- * 'uSecurityProfileIds' - The identifier(s) for the security profile assigned to the user.
--
-- * 'uUsername' - The user name assigned to the user account.
--
-- * 'uId' - The identifier of the user account.
--
-- * 'uHierarchyGroupId' - The identifier for the hierarchy group assigned to the user.
--
-- * 'uPhoneConfig' - A @UserPhoneConfig@ object.
user
    :: User
user =
  User'
    { _uRoutingProfileId = Nothing
    , _uDirectoryUserId = Nothing
    , _uARN = Nothing
    , _uIdentityInfo = Nothing
    , _uSecurityProfileIds = Nothing
    , _uUsername = Nothing
    , _uId = Nothing
    , _uHierarchyGroupId = Nothing
    , _uPhoneConfig = Nothing
    }


-- | The identifier of the routing profile assigned to the user.
uRoutingProfileId :: Lens' User (Maybe Text)
uRoutingProfileId = lens _uRoutingProfileId (\ s a -> s{_uRoutingProfileId = a})

-- | The directory Id for the user account in the existing directory used for identity management.
uDirectoryUserId :: Lens' User (Maybe Text)
uDirectoryUserId = lens _uDirectoryUserId (\ s a -> s{_uDirectoryUserId = a})

-- | The ARN of the user account.
uARN :: Lens' User (Maybe Text)
uARN = lens _uARN (\ s a -> s{_uARN = a})

-- | A @UserIdentityInfo@ object.
uIdentityInfo :: Lens' User (Maybe UserIdentityInfo)
uIdentityInfo = lens _uIdentityInfo (\ s a -> s{_uIdentityInfo = a})

-- | The identifier(s) for the security profile assigned to the user.
uSecurityProfileIds :: Lens' User (Maybe (NonEmpty Text))
uSecurityProfileIds = lens _uSecurityProfileIds (\ s a -> s{_uSecurityProfileIds = a}) . mapping _List1

-- | The user name assigned to the user account.
uUsername :: Lens' User (Maybe Text)
uUsername = lens _uUsername (\ s a -> s{_uUsername = a})

-- | The identifier of the user account.
uId :: Lens' User (Maybe Text)
uId = lens _uId (\ s a -> s{_uId = a})

-- | The identifier for the hierarchy group assigned to the user.
uHierarchyGroupId :: Lens' User (Maybe Text)
uHierarchyGroupId = lens _uHierarchyGroupId (\ s a -> s{_uHierarchyGroupId = a})

-- | A @UserPhoneConfig@ object.
uPhoneConfig :: Lens' User (Maybe UserPhoneConfig)
uPhoneConfig = lens _uPhoneConfig (\ s a -> s{_uPhoneConfig = a})

instance FromJSON User where
        parseJSON
          = withObject "User"
              (\ x ->
                 User' <$>
                   (x .:? "RoutingProfileId") <*>
                     (x .:? "DirectoryUserId")
                     <*> (x .:? "Arn")
                     <*> (x .:? "IdentityInfo")
                     <*> (x .:? "SecurityProfileIds")
                     <*> (x .:? "Username")
                     <*> (x .:? "Id")
                     <*> (x .:? "HierarchyGroupId")
                     <*> (x .:? "PhoneConfig"))

instance Hashable User where

instance NFData User where

-- | A @UserIdentityInfo@ object that contains information about the user's identity, including email address, first name, and last name.
--
--
--
-- /See:/ 'userIdentityInfo' smart constructor.
data UserIdentityInfo = UserIdentityInfo'
  { _uiiEmail     :: !(Maybe Text)
  , _uiiLastName  :: !(Maybe Text)
  , _uiiFirstName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserIdentityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiiEmail' - The email address added to the user account. If you are using SAML for identity management and include this parameter, an @InvalidRequestException@ is returned.
--
-- * 'uiiLastName' - The last name used in the user account. This is required if you are using Amazon Connect or SAML for identity management.
--
-- * 'uiiFirstName' - The first name used in the user account. This is required if you are using Amazon Connect or SAML for identity management.
userIdentityInfo
    :: UserIdentityInfo
userIdentityInfo =
  UserIdentityInfo'
    {_uiiEmail = Nothing, _uiiLastName = Nothing, _uiiFirstName = Nothing}


-- | The email address added to the user account. If you are using SAML for identity management and include this parameter, an @InvalidRequestException@ is returned.
uiiEmail :: Lens' UserIdentityInfo (Maybe Text)
uiiEmail = lens _uiiEmail (\ s a -> s{_uiiEmail = a})

-- | The last name used in the user account. This is required if you are using Amazon Connect or SAML for identity management.
uiiLastName :: Lens' UserIdentityInfo (Maybe Text)
uiiLastName = lens _uiiLastName (\ s a -> s{_uiiLastName = a})

-- | The first name used in the user account. This is required if you are using Amazon Connect or SAML for identity management.
uiiFirstName :: Lens' UserIdentityInfo (Maybe Text)
uiiFirstName = lens _uiiFirstName (\ s a -> s{_uiiFirstName = a})

instance FromJSON UserIdentityInfo where
        parseJSON
          = withObject "UserIdentityInfo"
              (\ x ->
                 UserIdentityInfo' <$>
                   (x .:? "Email") <*> (x .:? "LastName") <*>
                     (x .:? "FirstName"))

instance Hashable UserIdentityInfo where

instance NFData UserIdentityInfo where

instance ToJSON UserIdentityInfo where
        toJSON UserIdentityInfo'{..}
          = object
              (catMaybes
                 [("Email" .=) <$> _uiiEmail,
                  ("LastName" .=) <$> _uiiLastName,
                  ("FirstName" .=) <$> _uiiFirstName])

-- | A @UserPhoneConfig@ object that contains information about the user phone configuration settings.
--
--
--
-- /See:/ 'userPhoneConfig' smart constructor.
data UserPhoneConfig = UserPhoneConfig'
  { _upcAutoAccept                :: !(Maybe Bool)
  , _upcAfterContactWorkTimeLimit :: !(Maybe Nat)
  , _upcDeskPhoneNumber           :: !(Maybe Text)
  , _upcPhoneType                 :: !PhoneType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserPhoneConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcAutoAccept' - The Auto accept setting for the user, Yes or No.
--
-- * 'upcAfterContactWorkTimeLimit' - The After Call Work (ACW) timeout setting, in seconds, for the user.
--
-- * 'upcDeskPhoneNumber' - The phone number for the user's desk phone.
--
-- * 'upcPhoneType' - The phone type selected for the user, either Soft phone or Desk phone.
userPhoneConfig
    :: PhoneType -- ^ 'upcPhoneType'
    -> UserPhoneConfig
userPhoneConfig pPhoneType_ =
  UserPhoneConfig'
    { _upcAutoAccept = Nothing
    , _upcAfterContactWorkTimeLimit = Nothing
    , _upcDeskPhoneNumber = Nothing
    , _upcPhoneType = pPhoneType_
    }


-- | The Auto accept setting for the user, Yes or No.
upcAutoAccept :: Lens' UserPhoneConfig (Maybe Bool)
upcAutoAccept = lens _upcAutoAccept (\ s a -> s{_upcAutoAccept = a})

-- | The After Call Work (ACW) timeout setting, in seconds, for the user.
upcAfterContactWorkTimeLimit :: Lens' UserPhoneConfig (Maybe Natural)
upcAfterContactWorkTimeLimit = lens _upcAfterContactWorkTimeLimit (\ s a -> s{_upcAfterContactWorkTimeLimit = a}) . mapping _Nat

-- | The phone number for the user's desk phone.
upcDeskPhoneNumber :: Lens' UserPhoneConfig (Maybe Text)
upcDeskPhoneNumber = lens _upcDeskPhoneNumber (\ s a -> s{_upcDeskPhoneNumber = a})

-- | The phone type selected for the user, either Soft phone or Desk phone.
upcPhoneType :: Lens' UserPhoneConfig PhoneType
upcPhoneType = lens _upcPhoneType (\ s a -> s{_upcPhoneType = a})

instance FromJSON UserPhoneConfig where
        parseJSON
          = withObject "UserPhoneConfig"
              (\ x ->
                 UserPhoneConfig' <$>
                   (x .:? "AutoAccept") <*>
                     (x .:? "AfterContactWorkTimeLimit")
                     <*> (x .:? "DeskPhoneNumber")
                     <*> (x .: "PhoneType"))

instance Hashable UserPhoneConfig where

instance NFData UserPhoneConfig where

instance ToJSON UserPhoneConfig where
        toJSON UserPhoneConfig'{..}
          = object
              (catMaybes
                 [("AutoAccept" .=) <$> _upcAutoAccept,
                  ("AfterContactWorkTimeLimit" .=) <$>
                    _upcAfterContactWorkTimeLimit,
                  ("DeskPhoneNumber" .=) <$> _upcDeskPhoneNumber,
                  Just ("PhoneType" .= _upcPhoneType)])

-- | A @UserSummary@ object that contains Information about a user, including ARN, Id, and user name.
--
--
--
-- /See:/ 'userSummary' smart constructor.
data UserSummary = UserSummary'
  { _usARN      :: !(Maybe Text)
  , _usUsername :: !(Maybe Text)
  , _usId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usARN' - The ARN for the user account.
--
-- * 'usUsername' - The Amazon Connect user name for the user account.
--
-- * 'usId' - The identifier for the user account.
userSummary
    :: UserSummary
userSummary =
  UserSummary' {_usARN = Nothing, _usUsername = Nothing, _usId = Nothing}


-- | The ARN for the user account.
usARN :: Lens' UserSummary (Maybe Text)
usARN = lens _usARN (\ s a -> s{_usARN = a})

-- | The Amazon Connect user name for the user account.
usUsername :: Lens' UserSummary (Maybe Text)
usUsername = lens _usUsername (\ s a -> s{_usUsername = a})

-- | The identifier for the user account.
usId :: Lens' UserSummary (Maybe Text)
usId = lens _usId (\ s a -> s{_usId = a})

instance FromJSON UserSummary where
        parseJSON
          = withObject "UserSummary"
              (\ x ->
                 UserSummary' <$>
                   (x .:? "Arn") <*> (x .:? "Username") <*>
                     (x .:? "Id"))

instance Hashable UserSummary where

instance NFData UserSummary where
