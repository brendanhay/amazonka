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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.DeregisterIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the identity provider from providing user-based
-- subscriptions.
module Amazonka.LicenseManagerUserSubscriptions.DeregisterIdentityProvider
  ( -- * Creating a Request
    DeregisterIdentityProvider (..),
    newDeregisterIdentityProvider,

    -- * Request Lenses
    deregisterIdentityProvider_identityProvider,
    deregisterIdentityProvider_product,

    -- * Destructuring the Response
    DeregisterIdentityProviderResponse (..),
    newDeregisterIdentityProviderResponse,

    -- * Response Lenses
    deregisterIdentityProviderResponse_httpStatus,
    deregisterIdentityProviderResponse_identityProviderSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterIdentityProvider' smart constructor.
data DeregisterIdentityProvider = DeregisterIdentityProvider'
  { -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The name of the user-based subscription product.
    product :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProvider', 'deregisterIdentityProvider_identityProvider' - An object that specifies details for the identity provider.
--
-- 'product', 'deregisterIdentityProvider_product' - The name of the user-based subscription product.
newDeregisterIdentityProvider ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'product'
  Prelude.Text ->
  DeregisterIdentityProvider
newDeregisterIdentityProvider
  pIdentityProvider_
  pProduct_ =
    DeregisterIdentityProvider'
      { identityProvider =
          pIdentityProvider_,
        product = pProduct_
      }

-- | An object that specifies details for the identity provider.
deregisterIdentityProvider_identityProvider :: Lens.Lens' DeregisterIdentityProvider IdentityProvider
deregisterIdentityProvider_identityProvider = Lens.lens (\DeregisterIdentityProvider' {identityProvider} -> identityProvider) (\s@DeregisterIdentityProvider' {} a -> s {identityProvider = a} :: DeregisterIdentityProvider)

-- | The name of the user-based subscription product.
deregisterIdentityProvider_product :: Lens.Lens' DeregisterIdentityProvider Prelude.Text
deregisterIdentityProvider_product = Lens.lens (\DeregisterIdentityProvider' {product} -> product) (\s@DeregisterIdentityProvider' {} a -> s {product = a} :: DeregisterIdentityProvider)

instance Core.AWSRequest DeregisterIdentityProvider where
  type
    AWSResponse DeregisterIdentityProvider =
      DeregisterIdentityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "IdentityProviderSummary")
      )

instance Prelude.Hashable DeregisterIdentityProvider where
  hashWithSalt _salt DeregisterIdentityProvider' {..} =
    _salt `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` product

instance Prelude.NFData DeregisterIdentityProvider where
  rnf DeregisterIdentityProvider' {..} =
    Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf product

instance Data.ToHeaders DeregisterIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterIdentityProvider where
  toJSON DeregisterIdentityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("Product" Data..= product)
          ]
      )

instance Data.ToPath DeregisterIdentityProvider where
  toPath =
    Prelude.const
      "/identity-provider/DeregisterIdentityProvider"

instance Data.ToQuery DeregisterIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterIdentityProviderResponse' smart constructor.
data DeregisterIdentityProviderResponse = DeregisterIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata that describes the results of an identity provider operation.
    identityProviderSummary :: IdentityProviderSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterIdentityProviderResponse_httpStatus' - The response's http status code.
--
-- 'identityProviderSummary', 'deregisterIdentityProviderResponse_identityProviderSummary' - Metadata that describes the results of an identity provider operation.
newDeregisterIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'identityProviderSummary'
  IdentityProviderSummary ->
  DeregisterIdentityProviderResponse
newDeregisterIdentityProviderResponse
  pHttpStatus_
  pIdentityProviderSummary_ =
    DeregisterIdentityProviderResponse'
      { httpStatus =
          pHttpStatus_,
        identityProviderSummary =
          pIdentityProviderSummary_
      }

-- | The response's http status code.
deregisterIdentityProviderResponse_httpStatus :: Lens.Lens' DeregisterIdentityProviderResponse Prelude.Int
deregisterIdentityProviderResponse_httpStatus = Lens.lens (\DeregisterIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@DeregisterIdentityProviderResponse' {} a -> s {httpStatus = a} :: DeregisterIdentityProviderResponse)

-- | Metadata that describes the results of an identity provider operation.
deregisterIdentityProviderResponse_identityProviderSummary :: Lens.Lens' DeregisterIdentityProviderResponse IdentityProviderSummary
deregisterIdentityProviderResponse_identityProviderSummary = Lens.lens (\DeregisterIdentityProviderResponse' {identityProviderSummary} -> identityProviderSummary) (\s@DeregisterIdentityProviderResponse' {} a -> s {identityProviderSummary = a} :: DeregisterIdentityProviderResponse)

instance
  Prelude.NFData
    DeregisterIdentityProviderResponse
  where
  rnf DeregisterIdentityProviderResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf identityProviderSummary
