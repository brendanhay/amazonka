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
-- Module      : Amazonka.WorkDocs.Types.Subscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.Subscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.SubscriptionProtocolType

-- | Describes a subscription.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | The protocol of the subscription.
    protocol :: Prelude.Maybe SubscriptionProtocolType,
    -- | The endpoint of the subscription.
    endPoint :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subscription.
    subscriptionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'subscription_protocol' - The protocol of the subscription.
--
-- 'endPoint', 'subscription_endPoint' - The endpoint of the subscription.
--
-- 'subscriptionId', 'subscription_subscriptionId' - The ID of the subscription.
newSubscription ::
  Subscription
newSubscription =
  Subscription'
    { protocol = Prelude.Nothing,
      endPoint = Prelude.Nothing,
      subscriptionId = Prelude.Nothing
    }

-- | The protocol of the subscription.
subscription_protocol :: Lens.Lens' Subscription (Prelude.Maybe SubscriptionProtocolType)
subscription_protocol = Lens.lens (\Subscription' {protocol} -> protocol) (\s@Subscription' {} a -> s {protocol = a} :: Subscription)

-- | The endpoint of the subscription.
subscription_endPoint :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_endPoint = Lens.lens (\Subscription' {endPoint} -> endPoint) (\s@Subscription' {} a -> s {endPoint = a} :: Subscription)

-- | The ID of the subscription.
subscription_subscriptionId :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_subscriptionId = Lens.lens (\Subscription' {subscriptionId} -> subscriptionId) (\s@Subscription' {} a -> s {subscriptionId = a} :: Subscription)

instance Core.FromJSON Subscription where
  parseJSON =
    Core.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Prelude.<$> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "EndPoint")
            Prelude.<*> (x Core..:? "SubscriptionId")
      )

instance Prelude.Hashable Subscription where
  hashWithSalt salt' Subscription' {..} =
    salt' `Prelude.hashWithSalt` subscriptionId
      `Prelude.hashWithSalt` endPoint
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData Subscription where
  rnf Subscription' {..} =
    Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf subscriptionId
      `Prelude.seq` Prelude.rnf endPoint
