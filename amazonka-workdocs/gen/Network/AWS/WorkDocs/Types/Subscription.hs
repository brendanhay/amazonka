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
-- Module      : Network.AWS.WorkDocs.Types.Subscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Subscription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.SubscriptionProtocolType

-- | Describes a subscription.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | The ID of the subscription.
    subscriptionId :: Core.Maybe Core.Text,
    -- | The protocol of the subscription.
    protocol :: Core.Maybe SubscriptionProtocolType,
    -- | The endpoint of the subscription.
    endPoint :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Subscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionId', 'subscription_subscriptionId' - The ID of the subscription.
--
-- 'protocol', 'subscription_protocol' - The protocol of the subscription.
--
-- 'endPoint', 'subscription_endPoint' - The endpoint of the subscription.
newSubscription ::
  Subscription
newSubscription =
  Subscription'
    { subscriptionId = Core.Nothing,
      protocol = Core.Nothing,
      endPoint = Core.Nothing
    }

-- | The ID of the subscription.
subscription_subscriptionId :: Lens.Lens' Subscription (Core.Maybe Core.Text)
subscription_subscriptionId = Lens.lens (\Subscription' {subscriptionId} -> subscriptionId) (\s@Subscription' {} a -> s {subscriptionId = a} :: Subscription)

-- | The protocol of the subscription.
subscription_protocol :: Lens.Lens' Subscription (Core.Maybe SubscriptionProtocolType)
subscription_protocol = Lens.lens (\Subscription' {protocol} -> protocol) (\s@Subscription' {} a -> s {protocol = a} :: Subscription)

-- | The endpoint of the subscription.
subscription_endPoint :: Lens.Lens' Subscription (Core.Maybe Core.Text)
subscription_endPoint = Lens.lens (\Subscription' {endPoint} -> endPoint) (\s@Subscription' {} a -> s {endPoint = a} :: Subscription)

instance Core.FromJSON Subscription where
  parseJSON =
    Core.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Core.<$> (x Core..:? "SubscriptionId")
            Core.<*> (x Core..:? "Protocol")
            Core.<*> (x Core..:? "EndPoint")
      )

instance Core.Hashable Subscription

instance Core.NFData Subscription
