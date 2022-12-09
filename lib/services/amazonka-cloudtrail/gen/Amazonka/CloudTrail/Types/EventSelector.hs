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
-- Module      : Amazonka.CloudTrail.Types.EventSelector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.EventSelector where

import Amazonka.CloudTrail.Types.DataResource
import Amazonka.CloudTrail.Types.ReadWriteType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | CloudTrail supports data event logging for Amazon S3 objects, Lambda
    -- functions, and Amazon DynamoDB tables with basic event selectors. You
    -- can specify up to 250 resources for an individual event selector, but
    -- the total number of data resources cannot exceed 250 across all event
    -- selectors in a trail. This limit does not apply if you configure
    -- resource logging for all data events.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Data Events>
    -- and
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in CloudTrail>
    -- in the /CloudTrail User Guide/.
    dataResources :: Prelude.Maybe [DataResource],
    -- | An optional list of service event sources from which you do not want
    -- management events to be logged on your trail. In this release, the list
    -- can be empty (disables the filter), or it can filter out Key Management
    -- Service or Amazon RDS Data API events by containing @kms.amazonaws.com@
    -- or @rdsdata.amazonaws.com@. By default, @ExcludeManagementEventSources@
    -- is empty, and KMS and Amazon RDS Data API events are logged to your
    -- trail. You can exclude management event sources only in regions that
    -- support the event source.
    excludeManagementEventSources :: Prelude.Maybe [Prelude.Text],
    -- | Specify if you want your event selector to include management events for
    -- your trail.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-events-with-cloudtrail.html Management Events>
    -- in the /CloudTrail User Guide/.
    --
    -- By default, the value is @true@.
    --
    -- The first copy of management events is free. You are charged for
    -- additional copies of management events that you are logging on any
    -- subsequent trail in the same region. For more information about
    -- CloudTrail pricing, see
    -- <http://aws.amazon.com/cloudtrail/pricing/ CloudTrail Pricing>.
    includeManagementEvents :: Prelude.Maybe Prelude.Bool,
    -- | Specify if you want your trail to log read-only events, write-only
    -- events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only
    -- API operation and @RunInstances@ is a write-only API operation.
    --
    -- By default, the value is @All@.
    readWriteType :: Prelude.Maybe ReadWriteType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataResources', 'eventSelector_dataResources' - CloudTrail supports data event logging for Amazon S3 objects, Lambda
-- functions, and Amazon DynamoDB tables with basic event selectors. You
-- can specify up to 250 resources for an individual event selector, but
-- the total number of data resources cannot exceed 250 across all event
-- selectors in a trail. This limit does not apply if you configure
-- resource logging for all data events.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Data Events>
-- and
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in CloudTrail>
-- in the /CloudTrail User Guide/.
--
-- 'excludeManagementEventSources', 'eventSelector_excludeManagementEventSources' - An optional list of service event sources from which you do not want
-- management events to be logged on your trail. In this release, the list
-- can be empty (disables the filter), or it can filter out Key Management
-- Service or Amazon RDS Data API events by containing @kms.amazonaws.com@
-- or @rdsdata.amazonaws.com@. By default, @ExcludeManagementEventSources@
-- is empty, and KMS and Amazon RDS Data API events are logged to your
-- trail. You can exclude management event sources only in regions that
-- support the event source.
--
-- 'includeManagementEvents', 'eventSelector_includeManagementEvents' - Specify if you want your event selector to include management events for
-- your trail.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-events-with-cloudtrail.html Management Events>
-- in the /CloudTrail User Guide/.
--
-- By default, the value is @true@.
--
-- The first copy of management events is free. You are charged for
-- additional copies of management events that you are logging on any
-- subsequent trail in the same region. For more information about
-- CloudTrail pricing, see
-- <http://aws.amazon.com/cloudtrail/pricing/ CloudTrail Pricing>.
--
-- 'readWriteType', 'eventSelector_readWriteType' - Specify if you want your trail to log read-only events, write-only
-- events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only
-- API operation and @RunInstances@ is a write-only API operation.
--
-- By default, the value is @All@.
newEventSelector ::
  EventSelector
newEventSelector =
  EventSelector'
    { dataResources = Prelude.Nothing,
      excludeManagementEventSources = Prelude.Nothing,
      includeManagementEvents = Prelude.Nothing,
      readWriteType = Prelude.Nothing
    }

-- | CloudTrail supports data event logging for Amazon S3 objects, Lambda
-- functions, and Amazon DynamoDB tables with basic event selectors. You
-- can specify up to 250 resources for an individual event selector, but
-- the total number of data resources cannot exceed 250 across all event
-- selectors in a trail. This limit does not apply if you configure
-- resource logging for all data events.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Data Events>
-- and
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in CloudTrail>
-- in the /CloudTrail User Guide/.
eventSelector_dataResources :: Lens.Lens' EventSelector (Prelude.Maybe [DataResource])
eventSelector_dataResources = Lens.lens (\EventSelector' {dataResources} -> dataResources) (\s@EventSelector' {} a -> s {dataResources = a} :: EventSelector) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of service event sources from which you do not want
-- management events to be logged on your trail. In this release, the list
-- can be empty (disables the filter), or it can filter out Key Management
-- Service or Amazon RDS Data API events by containing @kms.amazonaws.com@
-- or @rdsdata.amazonaws.com@. By default, @ExcludeManagementEventSources@
-- is empty, and KMS and Amazon RDS Data API events are logged to your
-- trail. You can exclude management event sources only in regions that
-- support the event source.
eventSelector_excludeManagementEventSources :: Lens.Lens' EventSelector (Prelude.Maybe [Prelude.Text])
eventSelector_excludeManagementEventSources = Lens.lens (\EventSelector' {excludeManagementEventSources} -> excludeManagementEventSources) (\s@EventSelector' {} a -> s {excludeManagementEventSources = a} :: EventSelector) Prelude.. Lens.mapping Lens.coerced

-- | Specify if you want your event selector to include management events for
-- your trail.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-events-with-cloudtrail.html Management Events>
-- in the /CloudTrail User Guide/.
--
-- By default, the value is @true@.
--
-- The first copy of management events is free. You are charged for
-- additional copies of management events that you are logging on any
-- subsequent trail in the same region. For more information about
-- CloudTrail pricing, see
-- <http://aws.amazon.com/cloudtrail/pricing/ CloudTrail Pricing>.
eventSelector_includeManagementEvents :: Lens.Lens' EventSelector (Prelude.Maybe Prelude.Bool)
eventSelector_includeManagementEvents = Lens.lens (\EventSelector' {includeManagementEvents} -> includeManagementEvents) (\s@EventSelector' {} a -> s {includeManagementEvents = a} :: EventSelector)

-- | Specify if you want your trail to log read-only events, write-only
-- events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only
-- API operation and @RunInstances@ is a write-only API operation.
--
-- By default, the value is @All@.
eventSelector_readWriteType :: Lens.Lens' EventSelector (Prelude.Maybe ReadWriteType)
eventSelector_readWriteType = Lens.lens (\EventSelector' {readWriteType} -> readWriteType) (\s@EventSelector' {} a -> s {readWriteType = a} :: EventSelector)

instance Data.FromJSON EventSelector where
  parseJSON =
    Data.withObject
      "EventSelector"
      ( \x ->
          EventSelector'
            Prelude.<$> (x Data..:? "DataResources" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ExcludeManagementEventSources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "IncludeManagementEvents")
            Prelude.<*> (x Data..:? "ReadWriteType")
      )

instance Prelude.Hashable EventSelector where
  hashWithSalt _salt EventSelector' {..} =
    _salt `Prelude.hashWithSalt` dataResources
      `Prelude.hashWithSalt` excludeManagementEventSources
      `Prelude.hashWithSalt` includeManagementEvents
      `Prelude.hashWithSalt` readWriteType

instance Prelude.NFData EventSelector where
  rnf EventSelector' {..} =
    Prelude.rnf dataResources
      `Prelude.seq` Prelude.rnf excludeManagementEventSources
      `Prelude.seq` Prelude.rnf includeManagementEvents
      `Prelude.seq` Prelude.rnf readWriteType

instance Data.ToJSON EventSelector where
  toJSON EventSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataResources" Data..=) Prelude.<$> dataResources,
            ("ExcludeManagementEventSources" Data..=)
              Prelude.<$> excludeManagementEventSources,
            ("IncludeManagementEvents" Data..=)
              Prelude.<$> includeManagementEvents,
            ("ReadWriteType" Data..=) Prelude.<$> readWriteType
          ]
      )
