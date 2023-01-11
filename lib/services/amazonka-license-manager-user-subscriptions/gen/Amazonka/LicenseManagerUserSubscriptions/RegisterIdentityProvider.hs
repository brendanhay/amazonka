{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManagerUserSubscriptions.RegisterIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an identity provider for user-based subscriptions.
module Amazonka.LicenseManagerUserSubscriptions.RegisterIdentityProvider
  ( -- * Creating a Request
    RegisterIdentityProvider (..),
    newRegisterIdentityProvider,

    -- * Request Lenses
    registerIdentityProvider_settings,
    registerIdentityProvider_identityProvider,
    registerIdentityProvider_product,

    -- * Destructuring the Response
    RegisterIdentityProviderResponse (..),
    newRegisterIdentityProviderResponse,

    -- * Response Lenses
    registerIdentityProviderResponse_httpStatus,
    registerIdentityProviderResponse_identityProviderSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterIdentityProvider' smart constructor.
data RegisterIdentityProvider = RegisterIdentityProvider'
  { -- | The registered identity provider’s product related configuration
    -- settings such as the subnets to provision VPC endpoints.
    settings :: Prelude.Maybe Settings,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The name of the user-based subscription product.
    product :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'registerIdentityProvider_settings' - The registered identity provider’s product related configuration
-- settings such as the subnets to provision VPC endpoints.
--
-- 'identityProvider', 'registerIdentityProvider_identityProvider' - An object that specifies details for the identity provider.
--
-- 'product', 'registerIdentityProvider_product' - The name of the user-based subscription product.
newRegisterIdentityProvider ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'product'
  Prelude.Text ->
  RegisterIdentityProvider
newRegisterIdentityProvider
  pIdentityProvider_
  pProduct_ =
    RegisterIdentityProvider'
      { settings =
          Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        product = pProduct_
      }

-- | The registered identity provider’s product related configuration
-- settings such as the subnets to provision VPC endpoints.
registerIdentityProvider_settings :: Lens.Lens' RegisterIdentityProvider (Prelude.Maybe Settings)
registerIdentityProvider_settings = Lens.lens (\RegisterIdentityProvider' {settings} -> settings) (\s@RegisterIdentityProvider' {} a -> s {settings = a} :: RegisterIdentityProvider)

-- | An object that specifies details for the identity provider.
registerIdentityProvider_identityProvider :: Lens.Lens' RegisterIdentityProvider IdentityProvider
registerIdentityProvider_identityProvider = Lens.lens (\RegisterIdentityProvider' {identityProvider} -> identityProvider) (\s@RegisterIdentityProvider' {} a -> s {identityProvider = a} :: RegisterIdentityProvider)

-- | The name of the user-based subscription product.
registerIdentityProvider_product :: Lens.Lens' RegisterIdentityProvider Prelude.Text
registerIdentityProvider_product = Lens.lens (\RegisterIdentityProvider' {product} -> product) (\s@RegisterIdentityProvider' {} a -> s {product = a} :: RegisterIdentityProvider)

instance Core.AWSRequest RegisterIdentityProvider where
  type
    AWSResponse RegisterIdentityProvider =
      RegisterIdentityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "IdentityProviderSummary")
      )

instance Prelude.Hashable RegisterIdentityProvider where
  hashWithSalt _salt RegisterIdentityProvider' {..} =
    _salt `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` product

instance Prelude.NFData RegisterIdentityProvider where
  rnf RegisterIdentityProvider' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf product

instance Data.ToHeaders RegisterIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterIdentityProvider where
  toJSON RegisterIdentityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Settings" Data..=) Prelude.<$> settings,
            Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("Product" Data..= product)
          ]
      )

instance Data.ToPath RegisterIdentityProvider where
  toPath =
    Prelude.const
      "/identity-provider/RegisterIdentityProvider"

instance Data.ToQuery RegisterIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterIdentityProviderResponse' smart constructor.
data RegisterIdentityProviderResponse = RegisterIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata that describes the results of an identity provider operation.
    identityProviderSummary :: IdentityProviderSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerIdentityProviderResponse_httpStatus' - The response's http status code.
--
-- 'identityProviderSummary', 'registerIdentityProviderResponse_identityProviderSummary' - Metadata that describes the results of an identity provider operation.
newRegisterIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'identityProviderSummary'
  IdentityProviderSummary ->
  RegisterIdentityProviderResponse
newRegisterIdentityProviderResponse
  pHttpStatus_
  pIdentityProviderSummary_ =
    RegisterIdentityProviderResponse'
      { httpStatus =
          pHttpStatus_,
        identityProviderSummary =
          pIdentityProviderSummary_
      }

-- | The response's http status code.
registerIdentityProviderResponse_httpStatus :: Lens.Lens' RegisterIdentityProviderResponse Prelude.Int
registerIdentityProviderResponse_httpStatus = Lens.lens (\RegisterIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@RegisterIdentityProviderResponse' {} a -> s {httpStatus = a} :: RegisterIdentityProviderResponse)

-- | Metadata that describes the results of an identity provider operation.
registerIdentityProviderResponse_identityProviderSummary :: Lens.Lens' RegisterIdentityProviderResponse IdentityProviderSummary
registerIdentityProviderResponse_identityProviderSummary = Lens.lens (\RegisterIdentityProviderResponse' {identityProviderSummary} -> identityProviderSummary) (\s@RegisterIdentityProviderResponse' {} a -> s {identityProviderSummary = a} :: RegisterIdentityProviderResponse)

instance
  Prelude.NFData
    RegisterIdentityProviderResponse
  where
  rnf RegisterIdentityProviderResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf identityProviderSummary
