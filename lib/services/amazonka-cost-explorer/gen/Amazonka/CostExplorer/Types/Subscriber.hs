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
-- Module      : Amazonka.CostExplorer.Types.Subscriber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Subscriber where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.SubscriberStatus
import Amazonka.CostExplorer.Types.SubscriberType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The recipient of @AnomalySubscription@ notifications.
--
-- /See:/ 'newSubscriber' smart constructor.
data Subscriber = Subscriber'
  { -- | The email address or SNS Amazon Resource Name (ARN). This depends on the
    -- @Type@.
    address :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the subscriber accepts the notifications.
    status :: Prelude.Maybe SubscriberStatus,
    -- | The notification delivery channel.
    type' :: Prelude.Maybe SubscriberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'subscriber_address' - The email address or SNS Amazon Resource Name (ARN). This depends on the
-- @Type@.
--
-- 'status', 'subscriber_status' - Indicates if the subscriber accepts the notifications.
--
-- 'type'', 'subscriber_type' - The notification delivery channel.
newSubscriber ::
  Subscriber
newSubscriber =
  Subscriber'
    { address = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The email address or SNS Amazon Resource Name (ARN). This depends on the
-- @Type@.
subscriber_address :: Lens.Lens' Subscriber (Prelude.Maybe Prelude.Text)
subscriber_address = Lens.lens (\Subscriber' {address} -> address) (\s@Subscriber' {} a -> s {address = a} :: Subscriber)

-- | Indicates if the subscriber accepts the notifications.
subscriber_status :: Lens.Lens' Subscriber (Prelude.Maybe SubscriberStatus)
subscriber_status = Lens.lens (\Subscriber' {status} -> status) (\s@Subscriber' {} a -> s {status = a} :: Subscriber)

-- | The notification delivery channel.
subscriber_type :: Lens.Lens' Subscriber (Prelude.Maybe SubscriberType)
subscriber_type = Lens.lens (\Subscriber' {type'} -> type') (\s@Subscriber' {} a -> s {type' = a} :: Subscriber)

instance Data.FromJSON Subscriber where
  parseJSON =
    Data.withObject
      "Subscriber"
      ( \x ->
          Subscriber'
            Prelude.<$> (x Data..:? "Address")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Subscriber where
  hashWithSalt _salt Subscriber' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Subscriber where
  rnf Subscriber' {..} =
    Prelude.rnf address `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf type'

instance Data.ToJSON Subscriber where
  toJSON Subscriber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Address" Data..=) Prelude.<$> address,
            ("Status" Data..=) Prelude.<$> status,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
