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
import qualified Network.AWS.Prelude as Prelude

-- | Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails>
-- operation can\'t find a specified event.
--
-- /See:/ 'newEventDetailsErrorItem' smart constructor.
data EventDetailsErrorItem = EventDetailsErrorItem'
  { -- | The unique identifier for the event. The event ARN has the
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
    -- format.
    --
    -- For example, an event ARN might look like the following:
    --
    -- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the error.
    errorName :: Prelude.Maybe Prelude.Text,
    -- | A message that describes the error.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventDetailsErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventArn', 'eventDetailsErrorItem_eventArn' - The unique identifier for the event. The event ARN has the
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- 'errorName', 'eventDetailsErrorItem_errorName' - The name of the error.
--
-- 'errorMessage', 'eventDetailsErrorItem_errorMessage' - A message that describes the error.
newEventDetailsErrorItem ::
  EventDetailsErrorItem
newEventDetailsErrorItem =
  EventDetailsErrorItem'
    { eventArn = Prelude.Nothing,
      errorName = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The unique identifier for the event. The event ARN has the
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
eventDetailsErrorItem_eventArn :: Lens.Lens' EventDetailsErrorItem (Prelude.Maybe Prelude.Text)
eventDetailsErrorItem_eventArn = Lens.lens (\EventDetailsErrorItem' {eventArn} -> eventArn) (\s@EventDetailsErrorItem' {} a -> s {eventArn = a} :: EventDetailsErrorItem)

-- | The name of the error.
eventDetailsErrorItem_errorName :: Lens.Lens' EventDetailsErrorItem (Prelude.Maybe Prelude.Text)
eventDetailsErrorItem_errorName = Lens.lens (\EventDetailsErrorItem' {errorName} -> errorName) (\s@EventDetailsErrorItem' {} a -> s {errorName = a} :: EventDetailsErrorItem)

-- | A message that describes the error.
eventDetailsErrorItem_errorMessage :: Lens.Lens' EventDetailsErrorItem (Prelude.Maybe Prelude.Text)
eventDetailsErrorItem_errorMessage = Lens.lens (\EventDetailsErrorItem' {errorMessage} -> errorMessage) (\s@EventDetailsErrorItem' {} a -> s {errorMessage = a} :: EventDetailsErrorItem)

instance Core.FromJSON EventDetailsErrorItem where
  parseJSON =
    Core.withObject
      "EventDetailsErrorItem"
      ( \x ->
          EventDetailsErrorItem'
            Prelude.<$> (x Core..:? "eventArn")
            Prelude.<*> (x Core..:? "errorName")
            Prelude.<*> (x Core..:? "errorMessage")
      )

instance Prelude.Hashable EventDetailsErrorItem

instance Prelude.NFData EventDetailsErrorItem
