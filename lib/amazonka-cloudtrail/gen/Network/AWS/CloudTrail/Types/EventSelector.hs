{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.EventSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.EventSelector where

import Network.AWS.CloudTrail.Types.DataResource
import Network.AWS.CloudTrail.Types.ReadWriteType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use event selectors to further specify the management and data event settings for your trail. By default, trails created without specific event selectors will be configured to log all read and write management events, and no data events. When an event occurs in your account, CloudTrail evaluates the event selector for all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event.
--
--
-- You can configure up to five event selectors for a trail.
--
--
-- /See:/ 'eventSelector' smart constructor.
data EventSelector = EventSelector'
  { _esDataResources ::
      !(Maybe [DataResource]),
    _esReadWriteType :: !(Maybe ReadWriteType),
    _esExcludeManagementEventSources :: !(Maybe [Text]),
    _esIncludeManagementEvents :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esDataResources' - CloudTrail supports data event logging for Amazon S3 objects and AWS Lambda functions. You can specify up to 250 resources for an individual event selector, but the total number of data resources cannot exceed 250 across all event selectors in a trail. This limit does not apply if you configure resource logging for all data events.  For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events> and <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail> in the /AWS CloudTrail User Guide/ .
--
-- * 'esReadWriteType' - Specify if you want your trail to log read-only events, write-only events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only API operation and @RunInstances@ is a write-only API operation. By default, the value is @All@ .
--
-- * 'esExcludeManagementEventSources' - An optional list of service event sources from which you do not want management events to be logged on your trail. In this release, the list can be empty (disables the filter), or it can filter out AWS Key Management Service events by containing @"kms.amazonaws.com"@ . By default, @ExcludeManagementEventSources@ is empty, and AWS KMS events are included in events that are logged to your trail.
--
-- * 'esIncludeManagementEvents' - Specify if you want your event selector to include management events for your trail. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events> in the /AWS CloudTrail User Guide/ . By default, the value is @true@ . The first copy of management events is free. You are charged for additional copies of management events that you are logging on any subsequent trail in the same region. For more information about CloudTrail pricing, see <http://aws.amazon.com/cloudtrail/pricing/ AWS CloudTrail Pricing> .
eventSelector ::
  EventSelector
eventSelector =
  EventSelector'
    { _esDataResources = Nothing,
      _esReadWriteType = Nothing,
      _esExcludeManagementEventSources = Nothing,
      _esIncludeManagementEvents = Nothing
    }

-- | CloudTrail supports data event logging for Amazon S3 objects and AWS Lambda functions. You can specify up to 250 resources for an individual event selector, but the total number of data resources cannot exceed 250 across all event selectors in a trail. This limit does not apply if you configure resource logging for all data events.  For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events> and <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail> in the /AWS CloudTrail User Guide/ .
esDataResources :: Lens' EventSelector [DataResource]
esDataResources = lens _esDataResources (\s a -> s {_esDataResources = a}) . _Default . _Coerce

-- | Specify if you want your trail to log read-only events, write-only events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only API operation and @RunInstances@ is a write-only API operation. By default, the value is @All@ .
esReadWriteType :: Lens' EventSelector (Maybe ReadWriteType)
esReadWriteType = lens _esReadWriteType (\s a -> s {_esReadWriteType = a})

-- | An optional list of service event sources from which you do not want management events to be logged on your trail. In this release, the list can be empty (disables the filter), or it can filter out AWS Key Management Service events by containing @"kms.amazonaws.com"@ . By default, @ExcludeManagementEventSources@ is empty, and AWS KMS events are included in events that are logged to your trail.
esExcludeManagementEventSources :: Lens' EventSelector [Text]
esExcludeManagementEventSources = lens _esExcludeManagementEventSources (\s a -> s {_esExcludeManagementEventSources = a}) . _Default . _Coerce

-- | Specify if you want your event selector to include management events for your trail. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events> in the /AWS CloudTrail User Guide/ . By default, the value is @true@ . The first copy of management events is free. You are charged for additional copies of management events that you are logging on any subsequent trail in the same region. For more information about CloudTrail pricing, see <http://aws.amazon.com/cloudtrail/pricing/ AWS CloudTrail Pricing> .
esIncludeManagementEvents :: Lens' EventSelector (Maybe Bool)
esIncludeManagementEvents = lens _esIncludeManagementEvents (\s a -> s {_esIncludeManagementEvents = a})

instance FromJSON EventSelector where
  parseJSON =
    withObject
      "EventSelector"
      ( \x ->
          EventSelector'
            <$> (x .:? "DataResources" .!= mempty)
            <*> (x .:? "ReadWriteType")
            <*> (x .:? "ExcludeManagementEventSources" .!= mempty)
            <*> (x .:? "IncludeManagementEvents")
      )

instance Hashable EventSelector

instance NFData EventSelector

instance ToJSON EventSelector where
  toJSON EventSelector' {..} =
    object
      ( catMaybes
          [ ("DataResources" .=) <$> _esDataResources,
            ("ReadWriteType" .=) <$> _esReadWriteType,
            ("ExcludeManagementEventSources" .=)
              <$> _esExcludeManagementEventSources,
            ("IncludeManagementEvents" .=) <$> _esIncludeManagementEvents
          ]
      )
