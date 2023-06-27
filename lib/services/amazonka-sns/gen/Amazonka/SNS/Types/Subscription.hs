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
-- Module      : Amazonka.SNS.Types.Subscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.Subscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A wrapper type for the attributes of an Amazon SNS subscription.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | The subscription\'s endpoint (format depends on the protocol).
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The subscription\'s owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The subscription\'s protocol.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The subscription\'s ARN.
    subscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the subscription\'s topic.
    topicArn :: Prelude.Maybe Prelude.Text
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
-- 'endpoint', 'subscription_endpoint' - The subscription\'s endpoint (format depends on the protocol).
--
-- 'owner', 'subscription_owner' - The subscription\'s owner.
--
-- 'protocol', 'subscription_protocol' - The subscription\'s protocol.
--
-- 'subscriptionArn', 'subscription_subscriptionArn' - The subscription\'s ARN.
--
-- 'topicArn', 'subscription_topicArn' - The ARN of the subscription\'s topic.
newSubscription ::
  Subscription
newSubscription =
  Subscription'
    { endpoint = Prelude.Nothing,
      owner = Prelude.Nothing,
      protocol = Prelude.Nothing,
      subscriptionArn = Prelude.Nothing,
      topicArn = Prelude.Nothing
    }

-- | The subscription\'s endpoint (format depends on the protocol).
subscription_endpoint :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_endpoint = Lens.lens (\Subscription' {endpoint} -> endpoint) (\s@Subscription' {} a -> s {endpoint = a} :: Subscription)

-- | The subscription\'s owner.
subscription_owner :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_owner = Lens.lens (\Subscription' {owner} -> owner) (\s@Subscription' {} a -> s {owner = a} :: Subscription)

-- | The subscription\'s protocol.
subscription_protocol :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_protocol = Lens.lens (\Subscription' {protocol} -> protocol) (\s@Subscription' {} a -> s {protocol = a} :: Subscription)

-- | The subscription\'s ARN.
subscription_subscriptionArn :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_subscriptionArn = Lens.lens (\Subscription' {subscriptionArn} -> subscriptionArn) (\s@Subscription' {} a -> s {subscriptionArn = a} :: Subscription)

-- | The ARN of the subscription\'s topic.
subscription_topicArn :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_topicArn = Lens.lens (\Subscription' {topicArn} -> topicArn) (\s@Subscription' {} a -> s {topicArn = a} :: Subscription)

instance Data.FromXML Subscription where
  parseXML x =
    Subscription'
      Prelude.<$> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "Owner")
      Prelude.<*> (x Data..@? "Protocol")
      Prelude.<*> (x Data..@? "SubscriptionArn")
      Prelude.<*> (x Data..@? "TopicArn")

instance Prelude.Hashable Subscription where
  hashWithSalt _salt Subscription' {..} =
    _salt
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` subscriptionArn
      `Prelude.hashWithSalt` topicArn

instance Prelude.NFData Subscription where
  rnf Subscription' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf subscriptionArn
      `Prelude.seq` Prelude.rnf topicArn
