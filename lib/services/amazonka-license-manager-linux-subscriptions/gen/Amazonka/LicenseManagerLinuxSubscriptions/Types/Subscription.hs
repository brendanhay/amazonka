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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.Types.Subscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerLinuxSubscriptions.Types.Subscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object which details a discovered Linux subscription.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | The total amount of running instances using this subscription.
    instanceCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of the subscription.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of subscription. The type can be subscription-included with
    -- Amazon EC2, Bring Your Own Subscription model (BYOS), or from the Amazon
    -- Web Services Marketplace. Certain subscriptions may use licensing from
    -- the Amazon Web Services Marketplace as well as OS licensing from Amazon
    -- EC2 or BYOS.
    type' :: Prelude.Maybe Prelude.Text
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
-- 'instanceCount', 'subscription_instanceCount' - The total amount of running instances using this subscription.
--
-- 'name', 'subscription_name' - The name of the subscription.
--
-- 'type'', 'subscription_type' - The type of subscription. The type can be subscription-included with
-- Amazon EC2, Bring Your Own Subscription model (BYOS), or from the Amazon
-- Web Services Marketplace. Certain subscriptions may use licensing from
-- the Amazon Web Services Marketplace as well as OS licensing from Amazon
-- EC2 or BYOS.
newSubscription ::
  Subscription
newSubscription =
  Subscription'
    { instanceCount = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The total amount of running instances using this subscription.
subscription_instanceCount :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Integer)
subscription_instanceCount = Lens.lens (\Subscription' {instanceCount} -> instanceCount) (\s@Subscription' {} a -> s {instanceCount = a} :: Subscription)

-- | The name of the subscription.
subscription_name :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_name = Lens.lens (\Subscription' {name} -> name) (\s@Subscription' {} a -> s {name = a} :: Subscription)

-- | The type of subscription. The type can be subscription-included with
-- Amazon EC2, Bring Your Own Subscription model (BYOS), or from the Amazon
-- Web Services Marketplace. Certain subscriptions may use licensing from
-- the Amazon Web Services Marketplace as well as OS licensing from Amazon
-- EC2 or BYOS.
subscription_type :: Lens.Lens' Subscription (Prelude.Maybe Prelude.Text)
subscription_type = Lens.lens (\Subscription' {type'} -> type') (\s@Subscription' {} a -> s {type' = a} :: Subscription)

instance Data.FromJSON Subscription where
  parseJSON =
    Data.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Prelude.<$> (x Data..:? "InstanceCount")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Subscription where
  hashWithSalt _salt Subscription' {..} =
    _salt
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Subscription where
  rnf Subscription' {..} =
    Prelude.rnf instanceCount `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf type'
