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
-- Module      : Network.AWS.AWSHealth.Types.EventTypeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventTypeFilter where

import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The values to use to filter results from the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventTypes.html DescribeEventTypes>
-- operation.
--
-- /See:/ 'newEventTypeFilter' smart constructor.
data EventTypeFilter = EventTypeFilter'
  { -- | The AWS services associated with the event. For example, @EC2@, @RDS@.
    services :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of event type codes.
    eventTypeCodes :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of event type category codes (@issue@, @scheduledChange@, or
    -- @accountNotification@).
    eventTypeCategories :: Core.Maybe (Core.NonEmpty EventTypeCategory)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventTypeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'services', 'eventTypeFilter_services' - The AWS services associated with the event. For example, @EC2@, @RDS@.
--
-- 'eventTypeCodes', 'eventTypeFilter_eventTypeCodes' - A list of event type codes.
--
-- 'eventTypeCategories', 'eventTypeFilter_eventTypeCategories' - A list of event type category codes (@issue@, @scheduledChange@, or
-- @accountNotification@).
newEventTypeFilter ::
  EventTypeFilter
newEventTypeFilter =
  EventTypeFilter'
    { services = Core.Nothing,
      eventTypeCodes = Core.Nothing,
      eventTypeCategories = Core.Nothing
    }

-- | The AWS services associated with the event. For example, @EC2@, @RDS@.
eventTypeFilter_services :: Lens.Lens' EventTypeFilter (Core.Maybe (Core.NonEmpty Core.Text))
eventTypeFilter_services = Lens.lens (\EventTypeFilter' {services} -> services) (\s@EventTypeFilter' {} a -> s {services = a} :: EventTypeFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of event type codes.
eventTypeFilter_eventTypeCodes :: Lens.Lens' EventTypeFilter (Core.Maybe (Core.NonEmpty Core.Text))
eventTypeFilter_eventTypeCodes = Lens.lens (\EventTypeFilter' {eventTypeCodes} -> eventTypeCodes) (\s@EventTypeFilter' {} a -> s {eventTypeCodes = a} :: EventTypeFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of event type category codes (@issue@, @scheduledChange@, or
-- @accountNotification@).
eventTypeFilter_eventTypeCategories :: Lens.Lens' EventTypeFilter (Core.Maybe (Core.NonEmpty EventTypeCategory))
eventTypeFilter_eventTypeCategories = Lens.lens (\EventTypeFilter' {eventTypeCategories} -> eventTypeCategories) (\s@EventTypeFilter' {} a -> s {eventTypeCategories = a} :: EventTypeFilter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable EventTypeFilter

instance Core.NFData EventTypeFilter

instance Core.ToJSON EventTypeFilter where
  toJSON EventTypeFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("services" Core..=) Core.<$> services,
            ("eventTypeCodes" Core..=) Core.<$> eventTypeCodes,
            ("eventTypeCategories" Core..=)
              Core.<$> eventTypeCategories
          ]
      )
