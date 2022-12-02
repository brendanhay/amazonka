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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.ProductUserSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.ProductUserSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProvider
import qualified Amazonka.Prelude as Prelude

-- | The summary of the user-based subscription products for a user.
--
-- /See:/ 'newProductUserSummary' smart constructor.
data ProductUserSummary = ProductUserSummary'
  { -- | The domain name of the user.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The start date of a subscription.
    subscriptionStartDate :: Prelude.Maybe Prelude.Text,
    -- | The status message for a product for a user.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The end date of a subscription.
    subscriptionEndDate :: Prelude.Maybe Prelude.Text,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The name of the user-based subscription product.
    product :: Prelude.Text,
    -- | The status of a product for a user.
    status :: Prelude.Text,
    -- | The user name from the identity provider of the user.
    username :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductUserSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'productUserSummary_domain' - The domain name of the user.
--
-- 'subscriptionStartDate', 'productUserSummary_subscriptionStartDate' - The start date of a subscription.
--
-- 'statusMessage', 'productUserSummary_statusMessage' - The status message for a product for a user.
--
-- 'subscriptionEndDate', 'productUserSummary_subscriptionEndDate' - The end date of a subscription.
--
-- 'identityProvider', 'productUserSummary_identityProvider' - An object that specifies details for the identity provider.
--
-- 'product', 'productUserSummary_product' - The name of the user-based subscription product.
--
-- 'status', 'productUserSummary_status' - The status of a product for a user.
--
-- 'username', 'productUserSummary_username' - The user name from the identity provider of the user.
newProductUserSummary ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'product'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  ProductUserSummary
newProductUserSummary
  pIdentityProvider_
  pProduct_
  pStatus_
  pUsername_ =
    ProductUserSummary'
      { domain = Prelude.Nothing,
        subscriptionStartDate = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        subscriptionEndDate = Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        product = pProduct_,
        status = pStatus_,
        username = pUsername_
      }

-- | The domain name of the user.
productUserSummary_domain :: Lens.Lens' ProductUserSummary (Prelude.Maybe Prelude.Text)
productUserSummary_domain = Lens.lens (\ProductUserSummary' {domain} -> domain) (\s@ProductUserSummary' {} a -> s {domain = a} :: ProductUserSummary)

-- | The start date of a subscription.
productUserSummary_subscriptionStartDate :: Lens.Lens' ProductUserSummary (Prelude.Maybe Prelude.Text)
productUserSummary_subscriptionStartDate = Lens.lens (\ProductUserSummary' {subscriptionStartDate} -> subscriptionStartDate) (\s@ProductUserSummary' {} a -> s {subscriptionStartDate = a} :: ProductUserSummary)

-- | The status message for a product for a user.
productUserSummary_statusMessage :: Lens.Lens' ProductUserSummary (Prelude.Maybe Prelude.Text)
productUserSummary_statusMessage = Lens.lens (\ProductUserSummary' {statusMessage} -> statusMessage) (\s@ProductUserSummary' {} a -> s {statusMessage = a} :: ProductUserSummary)

-- | The end date of a subscription.
productUserSummary_subscriptionEndDate :: Lens.Lens' ProductUserSummary (Prelude.Maybe Prelude.Text)
productUserSummary_subscriptionEndDate = Lens.lens (\ProductUserSummary' {subscriptionEndDate} -> subscriptionEndDate) (\s@ProductUserSummary' {} a -> s {subscriptionEndDate = a} :: ProductUserSummary)

-- | An object that specifies details for the identity provider.
productUserSummary_identityProvider :: Lens.Lens' ProductUserSummary IdentityProvider
productUserSummary_identityProvider = Lens.lens (\ProductUserSummary' {identityProvider} -> identityProvider) (\s@ProductUserSummary' {} a -> s {identityProvider = a} :: ProductUserSummary)

-- | The name of the user-based subscription product.
productUserSummary_product :: Lens.Lens' ProductUserSummary Prelude.Text
productUserSummary_product = Lens.lens (\ProductUserSummary' {product} -> product) (\s@ProductUserSummary' {} a -> s {product = a} :: ProductUserSummary)

-- | The status of a product for a user.
productUserSummary_status :: Lens.Lens' ProductUserSummary Prelude.Text
productUserSummary_status = Lens.lens (\ProductUserSummary' {status} -> status) (\s@ProductUserSummary' {} a -> s {status = a} :: ProductUserSummary)

-- | The user name from the identity provider of the user.
productUserSummary_username :: Lens.Lens' ProductUserSummary Prelude.Text
productUserSummary_username = Lens.lens (\ProductUserSummary' {username} -> username) (\s@ProductUserSummary' {} a -> s {username = a} :: ProductUserSummary)

instance Data.FromJSON ProductUserSummary where
  parseJSON =
    Data.withObject
      "ProductUserSummary"
      ( \x ->
          ProductUserSummary'
            Prelude.<$> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "SubscriptionStartDate")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "SubscriptionEndDate")
            Prelude.<*> (x Data..: "IdentityProvider")
            Prelude.<*> (x Data..: "Product")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "Username")
      )

instance Prelude.Hashable ProductUserSummary where
  hashWithSalt _salt ProductUserSummary' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` subscriptionStartDate
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` subscriptionEndDate
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` product
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` username

instance Prelude.NFData ProductUserSummary where
  rnf ProductUserSummary' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf subscriptionStartDate
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf subscriptionEndDate
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf product
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf username
