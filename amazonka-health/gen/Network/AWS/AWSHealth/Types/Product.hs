{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.Product where

import Network.AWS.AWSHealth.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an entity that is affected by a Health event.
--
--
--
-- /See:/ 'affectedEntity' smart constructor.
data AffectedEntity = AffectedEntity'
  { _aeLastUpdatedTime :: !(Maybe POSIX)
  , _aeEntityValue     :: !(Maybe Text)
  , _aeAwsAccountId    :: !(Maybe Text)
  , _aeEventARN        :: !(Maybe Text)
  , _aeEntityARN       :: !(Maybe Text)
  , _aeTags            :: !(Maybe (Map Text Text))
  , _aeStatusCode      :: !(Maybe EntityStatusCode)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AffectedEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeLastUpdatedTime' - The most recent time that the entity was updated.
--
-- * 'aeEntityValue' - The ID of the affected entity.
--
-- * 'aeAwsAccountId' - The 12-digit AWS account number that contains the affected entity.
--
-- * 'aeEventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//EVENT_TYPE_PLUS_ID/ @ . Example: @arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331@
--
-- * 'aeEntityARN' - The unique identifier for the entity. Format: @arn:aws:health:/entity-region/ :/aws-account/ :entity//entity-id/ @ . Example: @arn:aws:health:us-east-1:111222333444:entity/AVh5GGT7ul1arKr1sE1K@
--
-- * 'aeTags' - A map of entity tags attached to the affected entity.
--
-- * 'aeStatusCode' - The most recent status of the entity affected by the event. The possible values are @IMPAIRED@ , @UNIMPAIRED@ , and @UNKNOWN@ .
affectedEntity
    :: AffectedEntity
affectedEntity =
  AffectedEntity'
    { _aeLastUpdatedTime = Nothing
    , _aeEntityValue = Nothing
    , _aeAwsAccountId = Nothing
    , _aeEventARN = Nothing
    , _aeEntityARN = Nothing
    , _aeTags = Nothing
    , _aeStatusCode = Nothing
    }


-- | The most recent time that the entity was updated.
aeLastUpdatedTime :: Lens' AffectedEntity (Maybe UTCTime)
aeLastUpdatedTime = lens _aeLastUpdatedTime (\ s a -> s{_aeLastUpdatedTime = a}) . mapping _Time

-- | The ID of the affected entity.
aeEntityValue :: Lens' AffectedEntity (Maybe Text)
aeEntityValue = lens _aeEntityValue (\ s a -> s{_aeEntityValue = a})

-- | The 12-digit AWS account number that contains the affected entity.
aeAwsAccountId :: Lens' AffectedEntity (Maybe Text)
aeAwsAccountId = lens _aeAwsAccountId (\ s a -> s{_aeAwsAccountId = a})

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//EVENT_TYPE_PLUS_ID/ @ . Example: @arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331@
aeEventARN :: Lens' AffectedEntity (Maybe Text)
aeEventARN = lens _aeEventARN (\ s a -> s{_aeEventARN = a})

-- | The unique identifier for the entity. Format: @arn:aws:health:/entity-region/ :/aws-account/ :entity//entity-id/ @ . Example: @arn:aws:health:us-east-1:111222333444:entity/AVh5GGT7ul1arKr1sE1K@
aeEntityARN :: Lens' AffectedEntity (Maybe Text)
aeEntityARN = lens _aeEntityARN (\ s a -> s{_aeEntityARN = a})

-- | A map of entity tags attached to the affected entity.
aeTags :: Lens' AffectedEntity (HashMap Text Text)
aeTags = lens _aeTags (\ s a -> s{_aeTags = a}) . _Default . _Map

-- | The most recent status of the entity affected by the event. The possible values are @IMPAIRED@ , @UNIMPAIRED@ , and @UNKNOWN@ .
aeStatusCode :: Lens' AffectedEntity (Maybe EntityStatusCode)
aeStatusCode = lens _aeStatusCode (\ s a -> s{_aeStatusCode = a})

instance FromJSON AffectedEntity where
        parseJSON
          = withObject "AffectedEntity"
              (\ x ->
                 AffectedEntity' <$>
                   (x .:? "lastUpdatedTime") <*> (x .:? "entityValue")
                     <*> (x .:? "awsAccountId")
                     <*> (x .:? "eventArn")
                     <*> (x .:? "entityArn")
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "statusCode"))

instance Hashable AffectedEntity where

instance NFData AffectedEntity where

-- | A range of dates and times that is used by the 'EventFilter' and 'EntityFilter' objects. If @from@ is set and @to@ is set: match items where the timestamp (@startTime@ , @endTime@ , or @lastUpdatedTime@ ) is between @from@ and @to@ inclusive. If @from@ is set and @to@ is not set: match items where the timestamp value is equal to or after @from@ . If @from@ is not set and @to@ is set: match items where the timestamp value is equal to or before @to@ .
--
--
--
-- /See:/ 'dateTimeRange' smart constructor.
data DateTimeRange = DateTimeRange'
  { _dtrTo   :: !(Maybe POSIX)
  , _dtrFrom :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DateTimeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrTo' - The ending date and time of a time range.
--
-- * 'dtrFrom' - The starting date and time of a time range.
dateTimeRange
    :: DateTimeRange
dateTimeRange = DateTimeRange' {_dtrTo = Nothing, _dtrFrom = Nothing}


-- | The ending date and time of a time range.
dtrTo :: Lens' DateTimeRange (Maybe UTCTime)
dtrTo = lens _dtrTo (\ s a -> s{_dtrTo = a}) . mapping _Time

-- | The starting date and time of a time range.
dtrFrom :: Lens' DateTimeRange (Maybe UTCTime)
dtrFrom = lens _dtrFrom (\ s a -> s{_dtrFrom = a}) . mapping _Time

instance Hashable DateTimeRange where

instance NFData DateTimeRange where

instance ToJSON DateTimeRange where
        toJSON DateTimeRange'{..}
          = object
              (catMaybes
                 [("to" .=) <$> _dtrTo, ("from" .=) <$> _dtrFrom])

-- | The number of entities that are affected by one or more events. Returned by the 'DescribeEntityAggregates' operation.
--
--
--
-- /See:/ 'entityAggregate' smart constructor.
data EntityAggregate = EntityAggregate'
  { _eCount    :: !(Maybe Int)
  , _eEventARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EntityAggregate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eCount' - The number entities that match the criteria for the specified events.
--
-- * 'eEventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//EVENT_TYPE_PLUS_ID/ @ . Example: @arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331@
entityAggregate
    :: EntityAggregate
entityAggregate = EntityAggregate' {_eCount = Nothing, _eEventARN = Nothing}


-- | The number entities that match the criteria for the specified events.
eCount :: Lens' EntityAggregate (Maybe Int)
eCount = lens _eCount (\ s a -> s{_eCount = a})

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//EVENT_TYPE_PLUS_ID/ @ . Example: @arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331@
eEventARN :: Lens' EntityAggregate (Maybe Text)
eEventARN = lens _eEventARN (\ s a -> s{_eEventARN = a})

instance FromJSON EntityAggregate where
        parseJSON
          = withObject "EntityAggregate"
              (\ x ->
                 EntityAggregate' <$>
                   (x .:? "count") <*> (x .:? "eventArn"))

instance Hashable EntityAggregate where

instance NFData EntityAggregate where

-- | The values to use to filter results from the 'DescribeAffectedEntities' operation.
--
--
--
-- /See:/ 'entityFilter' smart constructor.
data EntityFilter = EntityFilter'
  { _eStatusCodes      :: !(Maybe (List1 EntityStatusCode))
  , _eEntityARNs       :: !(Maybe (List1 Text))
  , _eEntityValues     :: !(Maybe (List1 Text))
  , _eTags             :: !(Maybe [Map Text Text])
  , _eLastUpdatedTimes :: !(Maybe (List1 DateTimeRange))
  , _eEventARNs        :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EntityFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStatusCodes' - A list of entity status codes (@IMPAIRED@ , @UNIMPAIRED@ , or @UNKNOWN@ ).
--
-- * 'eEntityARNs' - A list of entity ARNs (unique identifiers).
--
-- * 'eEntityValues' - A list of IDs for affected entities.
--
-- * 'eTags' - A map of entity tags attached to the affected entity.
--
-- * 'eLastUpdatedTimes' - A list of the most recent dates and times that the entity was updated.
--
-- * 'eEventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331", "arn:aws:health:us-west-1::event/AWS_EBS_LOST_VOLUME_xyz"@
entityFilter
    :: NonEmpty Text -- ^ 'eEventARNs'
    -> EntityFilter
entityFilter pEventARNs_ =
  EntityFilter'
    { _eStatusCodes = Nothing
    , _eEntityARNs = Nothing
    , _eEntityValues = Nothing
    , _eTags = Nothing
    , _eLastUpdatedTimes = Nothing
    , _eEventARNs = _List1 # pEventARNs_
    }


-- | A list of entity status codes (@IMPAIRED@ , @UNIMPAIRED@ , or @UNKNOWN@ ).
eStatusCodes :: Lens' EntityFilter (Maybe (NonEmpty EntityStatusCode))
eStatusCodes = lens _eStatusCodes (\ s a -> s{_eStatusCodes = a}) . mapping _List1

-- | A list of entity ARNs (unique identifiers).
eEntityARNs :: Lens' EntityFilter (Maybe (NonEmpty Text))
eEntityARNs = lens _eEntityARNs (\ s a -> s{_eEntityARNs = a}) . mapping _List1

-- | A list of IDs for affected entities.
eEntityValues :: Lens' EntityFilter (Maybe (NonEmpty Text))
eEntityValues = lens _eEntityValues (\ s a -> s{_eEntityValues = a}) . mapping _List1

-- | A map of entity tags attached to the affected entity.
eTags :: Lens' EntityFilter [HashMap Text Text]
eTags = lens _eTags (\ s a -> s{_eTags = a}) . _Default . _Coerce

-- | A list of the most recent dates and times that the entity was updated.
eLastUpdatedTimes :: Lens' EntityFilter (Maybe (NonEmpty DateTimeRange))
eLastUpdatedTimes = lens _eLastUpdatedTimes (\ s a -> s{_eLastUpdatedTimes = a}) . mapping _List1

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331", "arn:aws:health:us-west-1::event/AWS_EBS_LOST_VOLUME_xyz"@
eEventARNs :: Lens' EntityFilter (NonEmpty Text)
eEventARNs = lens _eEventARNs (\ s a -> s{_eEventARNs = a}) . _List1

instance Hashable EntityFilter where

instance NFData EntityFilter where

instance ToJSON EntityFilter where
        toJSON EntityFilter'{..}
          = object
              (catMaybes
                 [("statusCodes" .=) <$> _eStatusCodes,
                  ("entityArns" .=) <$> _eEntityARNs,
                  ("entityValues" .=) <$> _eEntityValues,
                  ("tags" .=) <$> _eTags,
                  ("lastUpdatedTimes" .=) <$> _eLastUpdatedTimes,
                  Just ("eventArns" .= _eEventARNs)])

-- | Summary information about an event, returned by the 'DescribeEvents' operation. The 'DescribeEventDetails' operation also returns this information, as well as the 'EventDescription' and additional event metadata.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eLastUpdatedTime   :: !(Maybe POSIX)
  , _eArn               :: !(Maybe Text)
  , _eService           :: !(Maybe Text)
  , _eStartTime         :: !(Maybe POSIX)
  , _eEventTypeCode     :: !(Maybe Text)
  , _eEventTypeCategory :: !(Maybe EventTypeCategory)
  , _eAvailabilityZone  :: !(Maybe Text)
  , _eEndTime           :: !(Maybe POSIX)
  , _eRegion            :: !(Maybe Text)
  , _eStatusCode        :: !(Maybe EventStatusCode)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eLastUpdatedTime' - The most recent date and time that the event was updated.
--
-- * 'eArn' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//EVENT_TYPE_PLUS_ID/ @ . Example: @arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331@
--
-- * 'eService' - The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
--
-- * 'eStartTime' - The date and time that the event began.
--
-- * 'eEventTypeCode' - The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- * 'eEventTypeCategory' - The
--
-- * 'eAvailabilityZone' - The AWS Availability Zone of the event. For example, us-east-1a.
--
-- * 'eEndTime' - The date and time that the event ended.
--
-- * 'eRegion' - The AWS region name of the event.
--
-- * 'eStatusCode' - The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
event
    :: Event
event =
  Event'
    { _eLastUpdatedTime = Nothing
    , _eArn = Nothing
    , _eService = Nothing
    , _eStartTime = Nothing
    , _eEventTypeCode = Nothing
    , _eEventTypeCategory = Nothing
    , _eAvailabilityZone = Nothing
    , _eEndTime = Nothing
    , _eRegion = Nothing
    , _eStatusCode = Nothing
    }


-- | The most recent date and time that the event was updated.
eLastUpdatedTime :: Lens' Event (Maybe UTCTime)
eLastUpdatedTime = lens _eLastUpdatedTime (\ s a -> s{_eLastUpdatedTime = a}) . mapping _Time

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//EVENT_TYPE_PLUS_ID/ @ . Example: @arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331@
eArn :: Lens' Event (Maybe Text)
eArn = lens _eArn (\ s a -> s{_eArn = a})

-- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
eService :: Lens' Event (Maybe Text)
eService = lens _eService (\ s a -> s{_eService = a})

-- | The date and time that the event began.
eStartTime :: Lens' Event (Maybe UTCTime)
eStartTime = lens _eStartTime (\ s a -> s{_eStartTime = a}) . mapping _Time

-- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
eEventTypeCode :: Lens' Event (Maybe Text)
eEventTypeCode = lens _eEventTypeCode (\ s a -> s{_eEventTypeCode = a})

-- | The
eEventTypeCategory :: Lens' Event (Maybe EventTypeCategory)
eEventTypeCategory = lens _eEventTypeCategory (\ s a -> s{_eEventTypeCategory = a})

-- | The AWS Availability Zone of the event. For example, us-east-1a.
eAvailabilityZone :: Lens' Event (Maybe Text)
eAvailabilityZone = lens _eAvailabilityZone (\ s a -> s{_eAvailabilityZone = a})

-- | The date and time that the event ended.
eEndTime :: Lens' Event (Maybe UTCTime)
eEndTime = lens _eEndTime (\ s a -> s{_eEndTime = a}) . mapping _Time

-- | The AWS region name of the event.
eRegion :: Lens' Event (Maybe Text)
eRegion = lens _eRegion (\ s a -> s{_eRegion = a})

-- | The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
eStatusCode :: Lens' Event (Maybe EventStatusCode)
eStatusCode = lens _eStatusCode (\ s a -> s{_eStatusCode = a})

instance FromJSON Event where
        parseJSON
          = withObject "Event"
              (\ x ->
                 Event' <$>
                   (x .:? "lastUpdatedTime") <*> (x .:? "arn") <*>
                     (x .:? "service")
                     <*> (x .:? "startTime")
                     <*> (x .:? "eventTypeCode")
                     <*> (x .:? "eventTypeCategory")
                     <*> (x .:? "availabilityZone")
                     <*> (x .:? "endTime")
                     <*> (x .:? "region")
                     <*> (x .:? "statusCode"))

instance Hashable Event where

instance NFData Event where

-- | The number of events of each issue type. Returned by the 'DescribeEventAggregates' operation.
--
--
--
-- /See:/ 'eventAggregate' smart constructor.
data EventAggregate = EventAggregate'
  { _eaCount          :: !(Maybe Int)
  , _eaAggregateValue :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventAggregate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaCount' - The number of events of the associated issue type.
--
-- * 'eaAggregateValue' - The issue type for the associated count.
eventAggregate
    :: EventAggregate
eventAggregate =
  EventAggregate' {_eaCount = Nothing, _eaAggregateValue = Nothing}


-- | The number of events of the associated issue type.
eaCount :: Lens' EventAggregate (Maybe Int)
eaCount = lens _eaCount (\ s a -> s{_eaCount = a})

-- | The issue type for the associated count.
eaAggregateValue :: Lens' EventAggregate (Maybe Text)
eaAggregateValue = lens _eaAggregateValue (\ s a -> s{_eaAggregateValue = a})

instance FromJSON EventAggregate where
        parseJSON
          = withObject "EventAggregate"
              (\ x ->
                 EventAggregate' <$>
                   (x .:? "count") <*> (x .:? "aggregateValue"))

instance Hashable EventAggregate where

instance NFData EventAggregate where

-- | The detailed description of the event. Included in the information returned by the 'DescribeEventDetails' operation.
--
--
--
-- /See:/ 'eventDescription' smart constructor.
newtype EventDescription = EventDescription'
  { _edLatestDescription :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edLatestDescription' - The most recent description of the event.
eventDescription
    :: EventDescription
eventDescription = EventDescription' {_edLatestDescription = Nothing}


-- | The most recent description of the event.
edLatestDescription :: Lens' EventDescription (Maybe Text)
edLatestDescription = lens _edLatestDescription (\ s a -> s{_edLatestDescription = a})

instance FromJSON EventDescription where
        parseJSON
          = withObject "EventDescription"
              (\ x ->
                 EventDescription' <$> (x .:? "latestDescription"))

instance Hashable EventDescription where

instance NFData EventDescription where

-- | Detailed information about an event. A combination of an 'Event' object, an 'EventDescription' object, and additional metadata about the event. Returned by the 'DescribeEventDetails' operation.
--
--
--
-- /See:/ 'eventDetails' smart constructor.
data EventDetails = EventDetails'
  { _edEvent            :: !(Maybe Event)
  , _edEventDescription :: !(Maybe EventDescription)
  , _edEventMetadata    :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edEvent' - Summary information about the event.
--
-- * 'edEventDescription' - The most recent description of the event.
--
-- * 'edEventMetadata' - Additional metadata about the event.
eventDetails
    :: EventDetails
eventDetails =
  EventDetails'
    { _edEvent = Nothing
    , _edEventDescription = Nothing
    , _edEventMetadata = Nothing
    }


-- | Summary information about the event.
edEvent :: Lens' EventDetails (Maybe Event)
edEvent = lens _edEvent (\ s a -> s{_edEvent = a})

-- | The most recent description of the event.
edEventDescription :: Lens' EventDetails (Maybe EventDescription)
edEventDescription = lens _edEventDescription (\ s a -> s{_edEventDescription = a})

-- | Additional metadata about the event.
edEventMetadata :: Lens' EventDetails (HashMap Text Text)
edEventMetadata = lens _edEventMetadata (\ s a -> s{_edEventMetadata = a}) . _Default . _Map

instance FromJSON EventDetails where
        parseJSON
          = withObject "EventDetails"
              (\ x ->
                 EventDetails' <$>
                   (x .:? "event") <*> (x .:? "eventDescription") <*>
                     (x .:? "eventMetadata" .!= mempty))

instance Hashable EventDetails where

instance NFData EventDetails where

-- | Error information returned when a 'DescribeEventDetails' operation cannot find a specified event.
--
--
--
-- /See:/ 'eventDetailsErrorItem' smart constructor.
data EventDetailsErrorItem = EventDetailsErrorItem'
  { _edeiEventARN     :: !(Maybe Text)
  , _edeiErrorName    :: !(Maybe Text)
  , _edeiErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventDetailsErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edeiEventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//EVENT_TYPE_PLUS_ID/ @ . Example: @arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331@
--
-- * 'edeiErrorName' - The name of the error.
--
-- * 'edeiErrorMessage' - A message that describes the error.
eventDetailsErrorItem
    :: EventDetailsErrorItem
eventDetailsErrorItem =
  EventDetailsErrorItem'
    { _edeiEventARN = Nothing
    , _edeiErrorName = Nothing
    , _edeiErrorMessage = Nothing
    }


-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//EVENT_TYPE_PLUS_ID/ @ . Example: @arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331@
edeiEventARN :: Lens' EventDetailsErrorItem (Maybe Text)
edeiEventARN = lens _edeiEventARN (\ s a -> s{_edeiEventARN = a})

-- | The name of the error.
edeiErrorName :: Lens' EventDetailsErrorItem (Maybe Text)
edeiErrorName = lens _edeiErrorName (\ s a -> s{_edeiErrorName = a})

-- | A message that describes the error.
edeiErrorMessage :: Lens' EventDetailsErrorItem (Maybe Text)
edeiErrorMessage = lens _edeiErrorMessage (\ s a -> s{_edeiErrorMessage = a})

instance FromJSON EventDetailsErrorItem where
        parseJSON
          = withObject "EventDetailsErrorItem"
              (\ x ->
                 EventDetailsErrorItem' <$>
                   (x .:? "eventArn") <*> (x .:? "errorName") <*>
                     (x .:? "errorMessage"))

instance Hashable EventDetailsErrorItem where

instance NFData EventDetailsErrorItem where

-- | The values to use to filter results from the 'DescribeEvents' and 'DescribeEventAggregates' operations.
--
--
--
-- /See:/ 'eventFilter' smart constructor.
data EventFilter = EventFilter'
  { _efEventARNs           :: !(Maybe (List1 Text))
  , _efEventTypeCategories :: !(Maybe (List1 EventTypeCategory))
  , _efEventTypeCodes      :: !(Maybe (List1 Text))
  , _efRegions             :: !(Maybe (List1 Text))
  , _efEventStatusCodes    :: !(Maybe (List1 EventStatusCode))
  , _efEndTimes            :: !(Maybe (List1 DateTimeRange))
  , _efAvailabilityZones   :: !(Maybe [Text])
  , _efEntityARNs          :: !(Maybe (List1 Text))
  , _efEntityValues        :: !(Maybe (List1 Text))
  , _efStartTimes          :: !(Maybe (List1 DateTimeRange))
  , _efServices            :: !(Maybe (List1 Text))
  , _efTags                :: !(Maybe [Map Text Text])
  , _efLastUpdatedTimes    :: !(Maybe (List1 DateTimeRange))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efEventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331", "arn:aws:health:us-west-1::event/AWS_EBS_LOST_VOLUME_xyz"@
--
-- * 'efEventTypeCategories' - A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- * 'efEventTypeCodes' - A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED"@
--
-- * 'efRegions' - A list of AWS regions.
--
-- * 'efEventStatusCodes' - A list of event status codes.
--
-- * 'efEndTimes' - A list of dates and times that the event ended.
--
-- * 'efAvailabilityZones' - A list of AWS availability zones.
--
-- * 'efEntityARNs' - A list of entity ARNs (unique identifiers).
--
-- * 'efEntityValues' - A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@ ) or EBS volumes (@vol-426ab23e@ ).
--
-- * 'efStartTimes' - A list of dates and times that the event began.
--
-- * 'efServices' - The AWS services associated with the event. For example, @EC2@ , @RDS@ .
--
-- * 'efTags' - A map of entity tags attached to the affected entity.
--
-- * 'efLastUpdatedTimes' - A list of dates and times that the event was last updated.
eventFilter
    :: EventFilter
eventFilter =
  EventFilter'
    { _efEventARNs = Nothing
    , _efEventTypeCategories = Nothing
    , _efEventTypeCodes = Nothing
    , _efRegions = Nothing
    , _efEventStatusCodes = Nothing
    , _efEndTimes = Nothing
    , _efAvailabilityZones = Nothing
    , _efEntityARNs = Nothing
    , _efEntityValues = Nothing
    , _efStartTimes = Nothing
    , _efServices = Nothing
    , _efTags = Nothing
    , _efLastUpdatedTimes = Nothing
    }


-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331", "arn:aws:health:us-west-1::event/AWS_EBS_LOST_VOLUME_xyz"@
efEventARNs :: Lens' EventFilter (Maybe (NonEmpty Text))
efEventARNs = lens _efEventARNs (\ s a -> s{_efEventARNs = a}) . mapping _List1

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
efEventTypeCategories :: Lens' EventFilter (Maybe (NonEmpty EventTypeCategory))
efEventTypeCategories = lens _efEventTypeCategories (\ s a -> s{_efEventTypeCategories = a}) . mapping _List1

-- | A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED"@
efEventTypeCodes :: Lens' EventFilter (Maybe (NonEmpty Text))
efEventTypeCodes = lens _efEventTypeCodes (\ s a -> s{_efEventTypeCodes = a}) . mapping _List1

-- | A list of AWS regions.
efRegions :: Lens' EventFilter (Maybe (NonEmpty Text))
efRegions = lens _efRegions (\ s a -> s{_efRegions = a}) . mapping _List1

-- | A list of event status codes.
efEventStatusCodes :: Lens' EventFilter (Maybe (NonEmpty EventStatusCode))
efEventStatusCodes = lens _efEventStatusCodes (\ s a -> s{_efEventStatusCodes = a}) . mapping _List1

-- | A list of dates and times that the event ended.
efEndTimes :: Lens' EventFilter (Maybe (NonEmpty DateTimeRange))
efEndTimes = lens _efEndTimes (\ s a -> s{_efEndTimes = a}) . mapping _List1

-- | A list of AWS availability zones.
efAvailabilityZones :: Lens' EventFilter [Text]
efAvailabilityZones = lens _efAvailabilityZones (\ s a -> s{_efAvailabilityZones = a}) . _Default . _Coerce

-- | A list of entity ARNs (unique identifiers).
efEntityARNs :: Lens' EventFilter (Maybe (NonEmpty Text))
efEntityARNs = lens _efEntityARNs (\ s a -> s{_efEntityARNs = a}) . mapping _List1

-- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@ ) or EBS volumes (@vol-426ab23e@ ).
efEntityValues :: Lens' EventFilter (Maybe (NonEmpty Text))
efEntityValues = lens _efEntityValues (\ s a -> s{_efEntityValues = a}) . mapping _List1

-- | A list of dates and times that the event began.
efStartTimes :: Lens' EventFilter (Maybe (NonEmpty DateTimeRange))
efStartTimes = lens _efStartTimes (\ s a -> s{_efStartTimes = a}) . mapping _List1

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
efServices :: Lens' EventFilter (Maybe (NonEmpty Text))
efServices = lens _efServices (\ s a -> s{_efServices = a}) . mapping _List1

-- | A map of entity tags attached to the affected entity.
efTags :: Lens' EventFilter [HashMap Text Text]
efTags = lens _efTags (\ s a -> s{_efTags = a}) . _Default . _Coerce

-- | A list of dates and times that the event was last updated.
efLastUpdatedTimes :: Lens' EventFilter (Maybe (NonEmpty DateTimeRange))
efLastUpdatedTimes = lens _efLastUpdatedTimes (\ s a -> s{_efLastUpdatedTimes = a}) . mapping _List1

instance Hashable EventFilter where

instance NFData EventFilter where

instance ToJSON EventFilter where
        toJSON EventFilter'{..}
          = object
              (catMaybes
                 [("eventArns" .=) <$> _efEventARNs,
                  ("eventTypeCategories" .=) <$>
                    _efEventTypeCategories,
                  ("eventTypeCodes" .=) <$> _efEventTypeCodes,
                  ("regions" .=) <$> _efRegions,
                  ("eventStatusCodes" .=) <$> _efEventStatusCodes,
                  ("endTimes" .=) <$> _efEndTimes,
                  ("availabilityZones" .=) <$> _efAvailabilityZones,
                  ("entityArns" .=) <$> _efEntityARNs,
                  ("entityValues" .=) <$> _efEntityValues,
                  ("startTimes" .=) <$> _efStartTimes,
                  ("services" .=) <$> _efServices,
                  ("tags" .=) <$> _efTags,
                  ("lastUpdatedTimes" .=) <$> _efLastUpdatedTimes])

-- | Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, @issue@ ), the service (for example, @EC2@ ), and the event type code (for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
--
--
--
-- /See:/ 'eventType' smart constructor.
data EventType = EventType'
  { _etService  :: !(Maybe Text)
  , _etCategory :: !(Maybe EventTypeCategory)
  , _etCode     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etService' - The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
--
-- * 'etCategory' - A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- * 'etCode' - The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
eventType
    :: EventType
eventType =
  EventType' {_etService = Nothing, _etCategory = Nothing, _etCode = Nothing}


-- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
etService :: Lens' EventType (Maybe Text)
etService = lens _etService (\ s a -> s{_etService = a})

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
etCategory :: Lens' EventType (Maybe EventTypeCategory)
etCategory = lens _etCategory (\ s a -> s{_etCategory = a})

-- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
etCode :: Lens' EventType (Maybe Text)
etCode = lens _etCode (\ s a -> s{_etCode = a})

instance FromJSON EventType where
        parseJSON
          = withObject "EventType"
              (\ x ->
                 EventType' <$>
                   (x .:? "service") <*> (x .:? "category") <*>
                     (x .:? "code"))

instance Hashable EventType where

instance NFData EventType where

-- | The values to use to filter results from the 'DescribeEventTypes' operation.
--
--
--
-- /See:/ 'eventTypeFilter' smart constructor.
data EventTypeFilter = EventTypeFilter'
  { _etfEventTypeCategories :: !(Maybe (List1 EventTypeCategory))
  , _etfEventTypeCodes      :: !(Maybe (List1 Text))
  , _etfServices            :: !(Maybe (List1 Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventTypeFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etfEventTypeCategories' - A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- * 'etfEventTypeCodes' - A list of event type codes.
--
-- * 'etfServices' - The AWS services associated with the event. For example, @EC2@ , @RDS@ .
eventTypeFilter
    :: EventTypeFilter
eventTypeFilter =
  EventTypeFilter'
    { _etfEventTypeCategories = Nothing
    , _etfEventTypeCodes = Nothing
    , _etfServices = Nothing
    }


-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
etfEventTypeCategories :: Lens' EventTypeFilter (Maybe (NonEmpty EventTypeCategory))
etfEventTypeCategories = lens _etfEventTypeCategories (\ s a -> s{_etfEventTypeCategories = a}) . mapping _List1

-- | A list of event type codes.
etfEventTypeCodes :: Lens' EventTypeFilter (Maybe (NonEmpty Text))
etfEventTypeCodes = lens _etfEventTypeCodes (\ s a -> s{_etfEventTypeCodes = a}) . mapping _List1

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
etfServices :: Lens' EventTypeFilter (Maybe (NonEmpty Text))
etfServices = lens _etfServices (\ s a -> s{_etfServices = a}) . mapping _List1

instance Hashable EventTypeFilter where

instance NFData EventTypeFilter where

instance ToJSON EventTypeFilter where
        toJSON EventTypeFilter'{..}
          = object
              (catMaybes
                 [("eventTypeCategories" .=) <$>
                    _etfEventTypeCategories,
                  ("eventTypeCodes" .=) <$> _etfEventTypeCodes,
                  ("services" .=) <$> _etfServices])
