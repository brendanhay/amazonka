{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventFilter where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The values to use to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents> and <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates> operations.
--
--
--
-- /See:/ 'eventFilter' smart constructor.
data EventFilter = EventFilter'
  { _efEventARNs ::
      !(Maybe (List1 Text)),
    _efEventTypeCategories :: !(Maybe (List1 EventTypeCategory)),
    _efEventTypeCodes :: !(Maybe (List1 Text)),
    _efRegions :: !(Maybe (List1 Text)),
    _efEventStatusCodes :: !(Maybe (List1 EventStatusCode)),
    _efEndTimes :: !(Maybe (List1 DateTimeRange)),
    _efAvailabilityZones :: !(Maybe [Text]),
    _efEntityARNs :: !(Maybe (List1 Text)),
    _efEntityValues :: !(Maybe (List1 Text)),
    _efStartTimes :: !(Maybe (List1 DateTimeRange)),
    _efServices :: !(Maybe (List1 Text)),
    _efTags :: !(Maybe [Map Text (Text)]),
    _efLastUpdatedTimes :: !(Maybe (List1 DateTimeRange))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efEventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
--
-- * 'efEventTypeCategories' - A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- * 'efEventTypeCodes' - A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
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
eventFilter ::
  EventFilter
eventFilter =
  EventFilter'
    { _efEventARNs = Nothing,
      _efEventTypeCategories = Nothing,
      _efEventTypeCodes = Nothing,
      _efRegions = Nothing,
      _efEventStatusCodes = Nothing,
      _efEndTimes = Nothing,
      _efAvailabilityZones = Nothing,
      _efEntityARNs = Nothing,
      _efEntityValues = Nothing,
      _efStartTimes = Nothing,
      _efServices = Nothing,
      _efTags = Nothing,
      _efLastUpdatedTimes = Nothing
    }

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
efEventARNs :: Lens' EventFilter (Maybe (NonEmpty Text))
efEventARNs = lens _efEventARNs (\s a -> s {_efEventARNs = a}) . mapping _List1

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
efEventTypeCategories :: Lens' EventFilter (Maybe (NonEmpty EventTypeCategory))
efEventTypeCategories = lens _efEventTypeCategories (\s a -> s {_efEventTypeCategories = a}) . mapping _List1

-- | A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
efEventTypeCodes :: Lens' EventFilter (Maybe (NonEmpty Text))
efEventTypeCodes = lens _efEventTypeCodes (\s a -> s {_efEventTypeCodes = a}) . mapping _List1

-- | A list of AWS regions.
efRegions :: Lens' EventFilter (Maybe (NonEmpty Text))
efRegions = lens _efRegions (\s a -> s {_efRegions = a}) . mapping _List1

-- | A list of event status codes.
efEventStatusCodes :: Lens' EventFilter (Maybe (NonEmpty EventStatusCode))
efEventStatusCodes = lens _efEventStatusCodes (\s a -> s {_efEventStatusCodes = a}) . mapping _List1

-- | A list of dates and times that the event ended.
efEndTimes :: Lens' EventFilter (Maybe (NonEmpty DateTimeRange))
efEndTimes = lens _efEndTimes (\s a -> s {_efEndTimes = a}) . mapping _List1

-- | A list of AWS availability zones.
efAvailabilityZones :: Lens' EventFilter [Text]
efAvailabilityZones = lens _efAvailabilityZones (\s a -> s {_efAvailabilityZones = a}) . _Default . _Coerce

-- | A list of entity ARNs (unique identifiers).
efEntityARNs :: Lens' EventFilter (Maybe (NonEmpty Text))
efEntityARNs = lens _efEntityARNs (\s a -> s {_efEntityARNs = a}) . mapping _List1

-- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@ ) or EBS volumes (@vol-426ab23e@ ).
efEntityValues :: Lens' EventFilter (Maybe (NonEmpty Text))
efEntityValues = lens _efEntityValues (\s a -> s {_efEntityValues = a}) . mapping _List1

-- | A list of dates and times that the event began.
efStartTimes :: Lens' EventFilter (Maybe (NonEmpty DateTimeRange))
efStartTimes = lens _efStartTimes (\s a -> s {_efStartTimes = a}) . mapping _List1

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
efServices :: Lens' EventFilter (Maybe (NonEmpty Text))
efServices = lens _efServices (\s a -> s {_efServices = a}) . mapping _List1

-- | A map of entity tags attached to the affected entity.
efTags :: Lens' EventFilter [HashMap Text (Text)]
efTags = lens _efTags (\s a -> s {_efTags = a}) . _Default . _Coerce

-- | A list of dates and times that the event was last updated.
efLastUpdatedTimes :: Lens' EventFilter (Maybe (NonEmpty DateTimeRange))
efLastUpdatedTimes = lens _efLastUpdatedTimes (\s a -> s {_efLastUpdatedTimes = a}) . mapping _List1

instance Hashable EventFilter

instance NFData EventFilter

instance ToJSON EventFilter where
  toJSON EventFilter' {..} =
    object
      ( catMaybes
          [ ("eventArns" .=) <$> _efEventARNs,
            ("eventTypeCategories" .=) <$> _efEventTypeCategories,
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
            ("lastUpdatedTimes" .=) <$> _efLastUpdatedTimes
          ]
      )
