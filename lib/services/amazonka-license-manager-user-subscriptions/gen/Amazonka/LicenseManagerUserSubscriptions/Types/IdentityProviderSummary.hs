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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProviderSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProviderSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.Types.Settings
import qualified Amazonka.Prelude as Prelude

-- | Describes an identity provider.
--
-- /See:/ 'newIdentityProviderSummary' smart constructor.
data IdentityProviderSummary = IdentityProviderSummary'
  { -- | The failure message associated with an identity provider.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The name of the user-based subscription product.
    product :: Prelude.Text,
    -- | An object that details the registered identity provider’s product
    -- related configuration settings such as the subnets to provision VPC
    -- endpoints.
    settings :: Settings,
    -- | The status of an identity provider.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProviderSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureMessage', 'identityProviderSummary_failureMessage' - The failure message associated with an identity provider.
--
-- 'identityProvider', 'identityProviderSummary_identityProvider' - An object that specifies details for the identity provider.
--
-- 'product', 'identityProviderSummary_product' - The name of the user-based subscription product.
--
-- 'settings', 'identityProviderSummary_settings' - An object that details the registered identity provider’s product
-- related configuration settings such as the subnets to provision VPC
-- endpoints.
--
-- 'status', 'identityProviderSummary_status' - The status of an identity provider.
newIdentityProviderSummary ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'product'
  Prelude.Text ->
  -- | 'settings'
  Settings ->
  -- | 'status'
  Prelude.Text ->
  IdentityProviderSummary
newIdentityProviderSummary
  pIdentityProvider_
  pProduct_
  pSettings_
  pStatus_ =
    IdentityProviderSummary'
      { failureMessage =
          Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        product = pProduct_,
        settings = pSettings_,
        status = pStatus_
      }

-- | The failure message associated with an identity provider.
identityProviderSummary_failureMessage :: Lens.Lens' IdentityProviderSummary (Prelude.Maybe Prelude.Text)
identityProviderSummary_failureMessage = Lens.lens (\IdentityProviderSummary' {failureMessage} -> failureMessage) (\s@IdentityProviderSummary' {} a -> s {failureMessage = a} :: IdentityProviderSummary)

-- | An object that specifies details for the identity provider.
identityProviderSummary_identityProvider :: Lens.Lens' IdentityProviderSummary IdentityProvider
identityProviderSummary_identityProvider = Lens.lens (\IdentityProviderSummary' {identityProvider} -> identityProvider) (\s@IdentityProviderSummary' {} a -> s {identityProvider = a} :: IdentityProviderSummary)

-- | The name of the user-based subscription product.
identityProviderSummary_product :: Lens.Lens' IdentityProviderSummary Prelude.Text
identityProviderSummary_product = Lens.lens (\IdentityProviderSummary' {product} -> product) (\s@IdentityProviderSummary' {} a -> s {product = a} :: IdentityProviderSummary)

-- | An object that details the registered identity provider’s product
-- related configuration settings such as the subnets to provision VPC
-- endpoints.
identityProviderSummary_settings :: Lens.Lens' IdentityProviderSummary Settings
identityProviderSummary_settings = Lens.lens (\IdentityProviderSummary' {settings} -> settings) (\s@IdentityProviderSummary' {} a -> s {settings = a} :: IdentityProviderSummary)

-- | The status of an identity provider.
identityProviderSummary_status :: Lens.Lens' IdentityProviderSummary Prelude.Text
identityProviderSummary_status = Lens.lens (\IdentityProviderSummary' {status} -> status) (\s@IdentityProviderSummary' {} a -> s {status = a} :: IdentityProviderSummary)

instance Data.FromJSON IdentityProviderSummary where
  parseJSON =
    Data.withObject
      "IdentityProviderSummary"
      ( \x ->
          IdentityProviderSummary'
            Prelude.<$> (x Data..:? "FailureMessage")
            Prelude.<*> (x Data..: "IdentityProvider")
            Prelude.<*> (x Data..: "Product")
            Prelude.<*> (x Data..: "Settings")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable IdentityProviderSummary where
  hashWithSalt _salt IdentityProviderSummary' {..} =
    _salt `Prelude.hashWithSalt` failureMessage
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` product
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` status

instance Prelude.NFData IdentityProviderSummary where
  rnf IdentityProviderSummary' {..} =
    Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf product
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf status
