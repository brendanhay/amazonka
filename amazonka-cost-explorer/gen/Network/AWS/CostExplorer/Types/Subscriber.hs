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
-- Module      : Network.AWS.CostExplorer.Types.Subscriber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Subscriber where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.SubscriberStatus
import Network.AWS.CostExplorer.Types.SubscriberType
import qualified Network.AWS.Lens as Lens

-- | The recipient of @AnomalySubscription@ notifications.
--
-- /See:/ 'newSubscriber' smart constructor.
data Subscriber = Subscriber'
  { -- | Indicates if the subscriber accepts the notifications.
    status :: Core.Maybe SubscriberStatus,
    -- | The email address or SNS Amazon Resource Name (ARN), depending on the
    -- @Type@.
    address :: Core.Maybe Core.Text,
    -- | The notification delivery channel.
    type' :: Core.Maybe SubscriberType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Subscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'subscriber_status' - Indicates if the subscriber accepts the notifications.
--
-- 'address', 'subscriber_address' - The email address or SNS Amazon Resource Name (ARN), depending on the
-- @Type@.
--
-- 'type'', 'subscriber_type' - The notification delivery channel.
newSubscriber ::
  Subscriber
newSubscriber =
  Subscriber'
    { status = Core.Nothing,
      address = Core.Nothing,
      type' = Core.Nothing
    }

-- | Indicates if the subscriber accepts the notifications.
subscriber_status :: Lens.Lens' Subscriber (Core.Maybe SubscriberStatus)
subscriber_status = Lens.lens (\Subscriber' {status} -> status) (\s@Subscriber' {} a -> s {status = a} :: Subscriber)

-- | The email address or SNS Amazon Resource Name (ARN), depending on the
-- @Type@.
subscriber_address :: Lens.Lens' Subscriber (Core.Maybe Core.Text)
subscriber_address = Lens.lens (\Subscriber' {address} -> address) (\s@Subscriber' {} a -> s {address = a} :: Subscriber)

-- | The notification delivery channel.
subscriber_type :: Lens.Lens' Subscriber (Core.Maybe SubscriberType)
subscriber_type = Lens.lens (\Subscriber' {type'} -> type') (\s@Subscriber' {} a -> s {type' = a} :: Subscriber)

instance Core.FromJSON Subscriber where
  parseJSON =
    Core.withObject
      "Subscriber"
      ( \x ->
          Subscriber'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "Address")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Subscriber

instance Core.NFData Subscriber

instance Core.ToJSON Subscriber where
  toJSON Subscriber' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("Address" Core..=) Core.<$> address,
            ("Type" Core..=) Core.<$> type'
          ]
      )
