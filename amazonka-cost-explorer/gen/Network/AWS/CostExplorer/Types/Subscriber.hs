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
-- Module      : Network.AWS.CostExplorer.Types.Subscriber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Subscriber where

import Network.AWS.CostExplorer.Types.SubscriberStatus
import Network.AWS.CostExplorer.Types.SubscriberType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The recipient of @AnomalySubscription@ notifications.
--
-- /See:/ 'newSubscriber' smart constructor.
data Subscriber = Subscriber'
  { -- | Indicates if the subscriber accepts the notifications.
    status :: Prelude.Maybe SubscriberStatus,
    -- | The email address or SNS Amazon Resource Name (ARN), depending on the
    -- @Type@.
    address :: Prelude.Maybe Prelude.Text,
    -- | The notification delivery channel.
    type' :: Prelude.Maybe SubscriberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      address = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Indicates if the subscriber accepts the notifications.
subscriber_status :: Lens.Lens' Subscriber (Prelude.Maybe SubscriberStatus)
subscriber_status = Lens.lens (\Subscriber' {status} -> status) (\s@Subscriber' {} a -> s {status = a} :: Subscriber)

-- | The email address or SNS Amazon Resource Name (ARN), depending on the
-- @Type@.
subscriber_address :: Lens.Lens' Subscriber (Prelude.Maybe Prelude.Text)
subscriber_address = Lens.lens (\Subscriber' {address} -> address) (\s@Subscriber' {} a -> s {address = a} :: Subscriber)

-- | The notification delivery channel.
subscriber_type :: Lens.Lens' Subscriber (Prelude.Maybe SubscriberType)
subscriber_type = Lens.lens (\Subscriber' {type'} -> type') (\s@Subscriber' {} a -> s {type' = a} :: Subscriber)

instance Prelude.FromJSON Subscriber where
  parseJSON =
    Prelude.withObject
      "Subscriber"
      ( \x ->
          Subscriber'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Address")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Subscriber

instance Prelude.NFData Subscriber

instance Prelude.ToJSON Subscriber where
  toJSON Subscriber' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Status" Prelude..=) Prelude.<$> status,
            ("Address" Prelude..=) Prelude.<$> address,
            ("Type" Prelude..=) Prelude.<$> type'
          ]
      )
