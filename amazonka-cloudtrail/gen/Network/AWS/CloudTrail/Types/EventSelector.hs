{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.EventSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.EventSelector where

import Network.AWS.CloudTrail.Types.DataResource
import Network.AWS.CloudTrail.Types.ReadWriteType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Use event selectors to further specify the management and data event
-- settings for your trail. By default, trails created without specific
-- event selectors will be configured to log all read and write management
-- events, and no data events. When an event occurs in your account,
-- CloudTrail evaluates the event selector for all trails. For each trail,
-- if the event matches any event selector, the trail processes and logs
-- the event. If the event doesn\'t match any event selector, the trail
-- doesn\'t log the event.
--
-- You can configure up to five event selectors for a trail.
--
-- You cannot apply both event selectors and advanced event selectors to a
-- trail.
--
-- /See:/ 'newEventSelector' smart constructor.
data EventSelector = EventSelector'
  { -- | Specify if you want your trail to log read-only events, write-only
    -- events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only
    -- API operation and @RunInstances@ is a write-only API operation.
    --
    -- By default, the value is @All@.
    readWriteType :: Prelude.Maybe ReadWriteType,
    -- | An optional list of service event sources from which you do not want
    -- management events to be logged on your trail. In this release, the list
    -- can be empty (disables the filter), or it can filter out AWS Key
    -- Management Service events by containing @\"kms.amazonaws.com\"@. By
    -- default, @ExcludeManagementEventSources@ is empty, and AWS KMS events
    -- are included in events that are logged to your trail.
    excludeManagementEventSources :: Prelude.Maybe [Prelude.Text],
    -- | Specify if you want your event selector to include management events for
    -- your trail.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events>
    -- in the /AWS CloudTrail User Guide/.
    --
    -- By default, the value is @true@.
    --
    -- The first copy of management events is free. You are charged for
    -- additional copies of management events that you are logging on any
    -- subsequent trail in the same region. For more information about
    -- CloudTrail pricing, see
    -- <http://aws.amazon.com/cloudtrail/pricing/ AWS CloudTrail Pricing>.
    includeManagementEvents :: Prelude.Maybe Prelude.Bool,
    -- | CloudTrail supports data event logging for Amazon S3 objects and AWS
    -- Lambda functions. You can specify up to 250 resources for an individual
    -- event selector, but the total number of data resources cannot exceed 250
    -- across all event selectors in a trail. This limit does not apply if you
    -- configure resource logging for all data events.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events>
    -- and
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail>
    -- in the /AWS CloudTrail User Guide/.
    dataResources :: Prelude.Maybe [DataResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readWriteType', 'eventSelector_readWriteType' - Specify if you want your trail to log read-only events, write-only
-- events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only
-- API operation and @RunInstances@ is a write-only API operation.
--
-- By default, the value is @All@.
--
-- 'excludeManagementEventSources', 'eventSelector_excludeManagementEventSources' - An optional list of service event sources from which you do not want
-- management events to be logged on your trail. In this release, the list
-- can be empty (disables the filter), or it can filter out AWS Key
-- Management Service events by containing @\"kms.amazonaws.com\"@. By
-- default, @ExcludeManagementEventSources@ is empty, and AWS KMS events
-- are included in events that are logged to your trail.
--
-- 'includeManagementEvents', 'eventSelector_includeManagementEvents' - Specify if you want your event selector to include management events for
-- your trail.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events>
-- in the /AWS CloudTrail User Guide/.
--
-- By default, the value is @true@.
--
-- The first copy of management events is free. You are charged for
-- additional copies of management events that you are logging on any
-- subsequent trail in the same region. For more information about
-- CloudTrail pricing, see
-- <http://aws.amazon.com/cloudtrail/pricing/ AWS CloudTrail Pricing>.
--
-- 'dataResources', 'eventSelector_dataResources' - CloudTrail supports data event logging for Amazon S3 objects and AWS
-- Lambda functions. You can specify up to 250 resources for an individual
-- event selector, but the total number of data resources cannot exceed 250
-- across all event selectors in a trail. This limit does not apply if you
-- configure resource logging for all data events.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events>
-- and
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail>
-- in the /AWS CloudTrail User Guide/.
newEventSelector ::
  EventSelector
newEventSelector =
  EventSelector'
    { readWriteType = Prelude.Nothing,
      excludeManagementEventSources = Prelude.Nothing,
      includeManagementEvents = Prelude.Nothing,
      dataResources = Prelude.Nothing
    }

-- | Specify if you want your trail to log read-only events, write-only
-- events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only
-- API operation and @RunInstances@ is a write-only API operation.
--
-- By default, the value is @All@.
eventSelector_readWriteType :: Lens.Lens' EventSelector (Prelude.Maybe ReadWriteType)
eventSelector_readWriteType = Lens.lens (\EventSelector' {readWriteType} -> readWriteType) (\s@EventSelector' {} a -> s {readWriteType = a} :: EventSelector)

-- | An optional list of service event sources from which you do not want
-- management events to be logged on your trail. In this release, the list
-- can be empty (disables the filter), or it can filter out AWS Key
-- Management Service events by containing @\"kms.amazonaws.com\"@. By
-- default, @ExcludeManagementEventSources@ is empty, and AWS KMS events
-- are included in events that are logged to your trail.
eventSelector_excludeManagementEventSources :: Lens.Lens' EventSelector (Prelude.Maybe [Prelude.Text])
eventSelector_excludeManagementEventSources = Lens.lens (\EventSelector' {excludeManagementEventSources} -> excludeManagementEventSources) (\s@EventSelector' {} a -> s {excludeManagementEventSources = a} :: EventSelector) Prelude.. Lens.mapping Prelude._Coerce

-- | Specify if you want your event selector to include management events for
-- your trail.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events>
-- in the /AWS CloudTrail User Guide/.
--
-- By default, the value is @true@.
--
-- The first copy of management events is free. You are charged for
-- additional copies of management events that you are logging on any
-- subsequent trail in the same region. For more information about
-- CloudTrail pricing, see
-- <http://aws.amazon.com/cloudtrail/pricing/ AWS CloudTrail Pricing>.
eventSelector_includeManagementEvents :: Lens.Lens' EventSelector (Prelude.Maybe Prelude.Bool)
eventSelector_includeManagementEvents = Lens.lens (\EventSelector' {includeManagementEvents} -> includeManagementEvents) (\s@EventSelector' {} a -> s {includeManagementEvents = a} :: EventSelector)

-- | CloudTrail supports data event logging for Amazon S3 objects and AWS
-- Lambda functions. You can specify up to 250 resources for an individual
-- event selector, but the total number of data resources cannot exceed 250
-- across all event selectors in a trail. This limit does not apply if you
-- configure resource logging for all data events.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events>
-- and
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail>
-- in the /AWS CloudTrail User Guide/.
eventSelector_dataResources :: Lens.Lens' EventSelector (Prelude.Maybe [DataResource])
eventSelector_dataResources = Lens.lens (\EventSelector' {dataResources} -> dataResources) (\s@EventSelector' {} a -> s {dataResources = a} :: EventSelector) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON EventSelector where
  parseJSON =
    Prelude.withObject
      "EventSelector"
      ( \x ->
          EventSelector'
            Prelude.<$> (x Prelude..:? "ReadWriteType")
            Prelude.<*> ( x Prelude..:? "ExcludeManagementEventSources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "IncludeManagementEvents")
            Prelude.<*> ( x Prelude..:? "DataResources"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EventSelector

instance Prelude.NFData EventSelector

instance Prelude.ToJSON EventSelector where
  toJSON EventSelector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ReadWriteType" Prelude..=)
              Prelude.<$> readWriteType,
            ("ExcludeManagementEventSources" Prelude..=)
              Prelude.<$> excludeManagementEventSources,
            ("IncludeManagementEvents" Prelude..=)
              Prelude.<$> includeManagementEvents,
            ("DataResources" Prelude..=)
              Prelude.<$> dataResources
          ]
      )
