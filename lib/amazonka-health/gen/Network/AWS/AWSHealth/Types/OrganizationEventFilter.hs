{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventFilter where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The values to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization> operation.
--
--
--
-- /See:/ 'organizationEventFilter' smart constructor.
data OrganizationEventFilter = OrganizationEventFilter'
  { _oefLastUpdatedTime ::
      !(Maybe DateTimeRange),
    _oefAwsAccountIds :: !(Maybe (List1 Text)),
    _oefEventTypeCategories ::
      !(Maybe (List1 EventTypeCategory)),
    _oefEventTypeCodes :: !(Maybe (List1 Text)),
    _oefStartTime :: !(Maybe DateTimeRange),
    _oefRegions :: !(Maybe (List1 Text)),
    _oefEventStatusCodes ::
      !(Maybe (List1 EventStatusCode)),
    _oefEndTime :: !(Maybe DateTimeRange),
    _oefEntityARNs :: !(Maybe (List1 Text)),
    _oefEntityValues :: !(Maybe (List1 Text)),
    _oefServices :: !(Maybe (List1 Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationEventFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oefLastUpdatedTime' - Undocumented member.
--
-- * 'oefAwsAccountIds' - A list of 12-digit AWS account numbers that contains the affected entities.
--
-- * 'oefEventTypeCategories' - A list of event type category codes (issue, scheduledChange, or accountNotification).
--
-- * 'oefEventTypeCodes' - A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
--
-- * 'oefStartTime' - Undocumented member.
--
-- * 'oefRegions' - A list of AWS Regions.
--
-- * 'oefEventStatusCodes' - A list of event status codes.
--
-- * 'oefEndTime' - Undocumented member.
--
-- * 'oefEntityARNs' - A list of entity ARNs (unique identifiers).
--
-- * 'oefEntityValues' - A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or EBS volumes (vol-426ab23e).
--
-- * 'oefServices' - The AWS services associated with the event. For example, @EC2@ , @RDS@ .
organizationEventFilter ::
  OrganizationEventFilter
organizationEventFilter =
  OrganizationEventFilter'
    { _oefLastUpdatedTime = Nothing,
      _oefAwsAccountIds = Nothing,
      _oefEventTypeCategories = Nothing,
      _oefEventTypeCodes = Nothing,
      _oefStartTime = Nothing,
      _oefRegions = Nothing,
      _oefEventStatusCodes = Nothing,
      _oefEndTime = Nothing,
      _oefEntityARNs = Nothing,
      _oefEntityValues = Nothing,
      _oefServices = Nothing
    }

-- | Undocumented member.
oefLastUpdatedTime :: Lens' OrganizationEventFilter (Maybe DateTimeRange)
oefLastUpdatedTime = lens _oefLastUpdatedTime (\s a -> s {_oefLastUpdatedTime = a})

-- | A list of 12-digit AWS account numbers that contains the affected entities.
oefAwsAccountIds :: Lens' OrganizationEventFilter (Maybe (NonEmpty Text))
oefAwsAccountIds = lens _oefAwsAccountIds (\s a -> s {_oefAwsAccountIds = a}) . mapping _List1

-- | A list of event type category codes (issue, scheduledChange, or accountNotification).
oefEventTypeCategories :: Lens' OrganizationEventFilter (Maybe (NonEmpty EventTypeCategory))
oefEventTypeCategories = lens _oefEventTypeCategories (\s a -> s {_oefEventTypeCategories = a}) . mapping _List1

-- | A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
oefEventTypeCodes :: Lens' OrganizationEventFilter (Maybe (NonEmpty Text))
oefEventTypeCodes = lens _oefEventTypeCodes (\s a -> s {_oefEventTypeCodes = a}) . mapping _List1

-- | Undocumented member.
oefStartTime :: Lens' OrganizationEventFilter (Maybe DateTimeRange)
oefStartTime = lens _oefStartTime (\s a -> s {_oefStartTime = a})

-- | A list of AWS Regions.
oefRegions :: Lens' OrganizationEventFilter (Maybe (NonEmpty Text))
oefRegions = lens _oefRegions (\s a -> s {_oefRegions = a}) . mapping _List1

-- | A list of event status codes.
oefEventStatusCodes :: Lens' OrganizationEventFilter (Maybe (NonEmpty EventStatusCode))
oefEventStatusCodes = lens _oefEventStatusCodes (\s a -> s {_oefEventStatusCodes = a}) . mapping _List1

-- | Undocumented member.
oefEndTime :: Lens' OrganizationEventFilter (Maybe DateTimeRange)
oefEndTime = lens _oefEndTime (\s a -> s {_oefEndTime = a})

-- | A list of entity ARNs (unique identifiers).
oefEntityARNs :: Lens' OrganizationEventFilter (Maybe (NonEmpty Text))
oefEntityARNs = lens _oefEntityARNs (\s a -> s {_oefEntityARNs = a}) . mapping _List1

-- | A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or EBS volumes (vol-426ab23e).
oefEntityValues :: Lens' OrganizationEventFilter (Maybe (NonEmpty Text))
oefEntityValues = lens _oefEntityValues (\s a -> s {_oefEntityValues = a}) . mapping _List1

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
oefServices :: Lens' OrganizationEventFilter (Maybe (NonEmpty Text))
oefServices = lens _oefServices (\s a -> s {_oefServices = a}) . mapping _List1

instance Hashable OrganizationEventFilter

instance NFData OrganizationEventFilter

instance ToJSON OrganizationEventFilter where
  toJSON OrganizationEventFilter' {..} =
    object
      ( catMaybes
          [ ("lastUpdatedTime" .=) <$> _oefLastUpdatedTime,
            ("awsAccountIds" .=) <$> _oefAwsAccountIds,
            ("eventTypeCategories" .=) <$> _oefEventTypeCategories,
            ("eventTypeCodes" .=) <$> _oefEventTypeCodes,
            ("startTime" .=) <$> _oefStartTime,
            ("regions" .=) <$> _oefRegions,
            ("eventStatusCodes" .=) <$> _oefEventStatusCodes,
            ("endTime" .=) <$> _oefEndTime,
            ("entityArns" .=) <$> _oefEntityARNs,
            ("entityValues" .=) <$> _oefEntityValues,
            ("services" .=) <$> _oefServices
          ]
      )
