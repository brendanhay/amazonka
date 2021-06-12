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
-- Module      : Network.AWS.AWSHealth.Types.EventDetailsErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventDetailsErrorItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails>
-- operation cannot find a specified event.
--
-- /See:/ 'newEventDetailsErrorItem' smart constructor.
data EventDetailsErrorItem = EventDetailsErrorItem'
  { -- | The name of the error.
    errorName :: Core.Maybe Core.Text,
    -- | The unique identifier for the event. Format:
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
    -- Example:
    -- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Core.Maybe Core.Text,
    -- | A message that describes the error.
    errorMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventDetailsErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorName', 'eventDetailsErrorItem_errorName' - The name of the error.
--
-- 'eventArn', 'eventDetailsErrorItem_eventArn' - The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- 'errorMessage', 'eventDetailsErrorItem_errorMessage' - A message that describes the error.
newEventDetailsErrorItem ::
  EventDetailsErrorItem
newEventDetailsErrorItem =
  EventDetailsErrorItem'
    { errorName = Core.Nothing,
      eventArn = Core.Nothing,
      errorMessage = Core.Nothing
    }

-- | The name of the error.
eventDetailsErrorItem_errorName :: Lens.Lens' EventDetailsErrorItem (Core.Maybe Core.Text)
eventDetailsErrorItem_errorName = Lens.lens (\EventDetailsErrorItem' {errorName} -> errorName) (\s@EventDetailsErrorItem' {} a -> s {errorName = a} :: EventDetailsErrorItem)

-- | The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
eventDetailsErrorItem_eventArn :: Lens.Lens' EventDetailsErrorItem (Core.Maybe Core.Text)
eventDetailsErrorItem_eventArn = Lens.lens (\EventDetailsErrorItem' {eventArn} -> eventArn) (\s@EventDetailsErrorItem' {} a -> s {eventArn = a} :: EventDetailsErrorItem)

-- | A message that describes the error.
eventDetailsErrorItem_errorMessage :: Lens.Lens' EventDetailsErrorItem (Core.Maybe Core.Text)
eventDetailsErrorItem_errorMessage = Lens.lens (\EventDetailsErrorItem' {errorMessage} -> errorMessage) (\s@EventDetailsErrorItem' {} a -> s {errorMessage = a} :: EventDetailsErrorItem)

instance Core.FromJSON EventDetailsErrorItem where
  parseJSON =
    Core.withObject
      "EventDetailsErrorItem"
      ( \x ->
          EventDetailsErrorItem'
            Core.<$> (x Core..:? "errorName")
            Core.<*> (x Core..:? "eventArn")
            Core.<*> (x Core..:? "errorMessage")
      )

instance Core.Hashable EventDetailsErrorItem

instance Core.NFData EventDetailsErrorItem
