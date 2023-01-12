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
-- Module      : Amazonka.Budgets.Types.Subscriber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.Subscriber where

import Amazonka.Budgets.Types.SubscriptionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The subscriber to a budget notification. The subscriber consists of a
-- subscription type and either an Amazon SNS topic or an email address.
--
-- For example, an email subscriber has the following parameters:
--
-- -   A @subscriptionType@ of @EMAIL@
--
-- -   An @address@ of @example\@example.com@
--
-- /See:/ 'newSubscriber' smart constructor.
data Subscriber = Subscriber'
  { -- | The type of notification that Amazon Web Services sends to a subscriber.
    subscriptionType :: SubscriptionType,
    -- | The address that Amazon Web Services sends budget notifications to,
    -- either an SNS topic or an email.
    --
    -- When you create a subscriber, the value of @Address@ can\'t contain line
    -- breaks.
    address :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionType', 'subscriber_subscriptionType' - The type of notification that Amazon Web Services sends to a subscriber.
--
-- 'address', 'subscriber_address' - The address that Amazon Web Services sends budget notifications to,
-- either an SNS topic or an email.
--
-- When you create a subscriber, the value of @Address@ can\'t contain line
-- breaks.
newSubscriber ::
  -- | 'subscriptionType'
  SubscriptionType ->
  -- | 'address'
  Prelude.Text ->
  Subscriber
newSubscriber pSubscriptionType_ pAddress_ =
  Subscriber'
    { subscriptionType = pSubscriptionType_,
      address = Data._Sensitive Lens.# pAddress_
    }

-- | The type of notification that Amazon Web Services sends to a subscriber.
subscriber_subscriptionType :: Lens.Lens' Subscriber SubscriptionType
subscriber_subscriptionType = Lens.lens (\Subscriber' {subscriptionType} -> subscriptionType) (\s@Subscriber' {} a -> s {subscriptionType = a} :: Subscriber)

-- | The address that Amazon Web Services sends budget notifications to,
-- either an SNS topic or an email.
--
-- When you create a subscriber, the value of @Address@ can\'t contain line
-- breaks.
subscriber_address :: Lens.Lens' Subscriber Prelude.Text
subscriber_address = Lens.lens (\Subscriber' {address} -> address) (\s@Subscriber' {} a -> s {address = a} :: Subscriber) Prelude.. Data._Sensitive

instance Data.FromJSON Subscriber where
  parseJSON =
    Data.withObject
      "Subscriber"
      ( \x ->
          Subscriber'
            Prelude.<$> (x Data..: "SubscriptionType")
            Prelude.<*> (x Data..: "Address")
      )

instance Prelude.Hashable Subscriber where
  hashWithSalt _salt Subscriber' {..} =
    _salt `Prelude.hashWithSalt` subscriptionType
      `Prelude.hashWithSalt` address

instance Prelude.NFData Subscriber where
  rnf Subscriber' {..} =
    Prelude.rnf subscriptionType
      `Prelude.seq` Prelude.rnf address

instance Data.ToJSON Subscriber where
  toJSON Subscriber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SubscriptionType" Data..= subscriptionType),
            Prelude.Just ("Address" Data..= address)
          ]
      )
