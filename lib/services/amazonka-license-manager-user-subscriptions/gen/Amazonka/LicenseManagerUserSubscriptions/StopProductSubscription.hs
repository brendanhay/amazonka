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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.StopProductSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a product subscription for a user with the specified identity
-- provider.
module Amazonka.LicenseManagerUserSubscriptions.StopProductSubscription
  ( -- * Creating a Request
    StopProductSubscription (..),
    newStopProductSubscription,

    -- * Request Lenses
    stopProductSubscription_domain,
    stopProductSubscription_identityProvider,
    stopProductSubscription_product,
    stopProductSubscription_username,

    -- * Destructuring the Response
    StopProductSubscriptionResponse (..),
    newStopProductSubscriptionResponse,

    -- * Response Lenses
    stopProductSubscriptionResponse_httpStatus,
    stopProductSubscriptionResponse_productUserSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopProductSubscription' smart constructor.
data StopProductSubscription = StopProductSubscription'
  { -- | The domain name of the user.
    domain :: Prelude.Maybe Prelude.Text,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The name of the user-based subscription product.
    product :: Prelude.Text,
    -- | The user name from the identity provider for the user.
    username :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopProductSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'stopProductSubscription_domain' - The domain name of the user.
--
-- 'identityProvider', 'stopProductSubscription_identityProvider' - An object that specifies details for the identity provider.
--
-- 'product', 'stopProductSubscription_product' - The name of the user-based subscription product.
--
-- 'username', 'stopProductSubscription_username' - The user name from the identity provider for the user.
newStopProductSubscription ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'product'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  StopProductSubscription
newStopProductSubscription
  pIdentityProvider_
  pProduct_
  pUsername_ =
    StopProductSubscription'
      { domain = Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        product = pProduct_,
        username = pUsername_
      }

-- | The domain name of the user.
stopProductSubscription_domain :: Lens.Lens' StopProductSubscription (Prelude.Maybe Prelude.Text)
stopProductSubscription_domain = Lens.lens (\StopProductSubscription' {domain} -> domain) (\s@StopProductSubscription' {} a -> s {domain = a} :: StopProductSubscription)

-- | An object that specifies details for the identity provider.
stopProductSubscription_identityProvider :: Lens.Lens' StopProductSubscription IdentityProvider
stopProductSubscription_identityProvider = Lens.lens (\StopProductSubscription' {identityProvider} -> identityProvider) (\s@StopProductSubscription' {} a -> s {identityProvider = a} :: StopProductSubscription)

-- | The name of the user-based subscription product.
stopProductSubscription_product :: Lens.Lens' StopProductSubscription Prelude.Text
stopProductSubscription_product = Lens.lens (\StopProductSubscription' {product} -> product) (\s@StopProductSubscription' {} a -> s {product = a} :: StopProductSubscription)

-- | The user name from the identity provider for the user.
stopProductSubscription_username :: Lens.Lens' StopProductSubscription Prelude.Text
stopProductSubscription_username = Lens.lens (\StopProductSubscription' {username} -> username) (\s@StopProductSubscription' {} a -> s {username = a} :: StopProductSubscription)

instance Core.AWSRequest StopProductSubscription where
  type
    AWSResponse StopProductSubscription =
      StopProductSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopProductSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProductUserSummary")
      )

instance Prelude.Hashable StopProductSubscription where
  hashWithSalt _salt StopProductSubscription' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` product
      `Prelude.hashWithSalt` username

instance Prelude.NFData StopProductSubscription where
  rnf StopProductSubscription' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf product
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders StopProductSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopProductSubscription where
  toJSON StopProductSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("Product" Data..= product),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath StopProductSubscription where
  toPath =
    Prelude.const "/user/StopProductSubscription"

instance Data.ToQuery StopProductSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopProductSubscriptionResponse' smart constructor.
data StopProductSubscriptionResponse = StopProductSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata that describes the start product subscription operation.
    productUserSummary :: ProductUserSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopProductSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopProductSubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'productUserSummary', 'stopProductSubscriptionResponse_productUserSummary' - Metadata that describes the start product subscription operation.
newStopProductSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'productUserSummary'
  ProductUserSummary ->
  StopProductSubscriptionResponse
newStopProductSubscriptionResponse
  pHttpStatus_
  pProductUserSummary_ =
    StopProductSubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        productUserSummary = pProductUserSummary_
      }

-- | The response's http status code.
stopProductSubscriptionResponse_httpStatus :: Lens.Lens' StopProductSubscriptionResponse Prelude.Int
stopProductSubscriptionResponse_httpStatus = Lens.lens (\StopProductSubscriptionResponse' {httpStatus} -> httpStatus) (\s@StopProductSubscriptionResponse' {} a -> s {httpStatus = a} :: StopProductSubscriptionResponse)

-- | Metadata that describes the start product subscription operation.
stopProductSubscriptionResponse_productUserSummary :: Lens.Lens' StopProductSubscriptionResponse ProductUserSummary
stopProductSubscriptionResponse_productUserSummary = Lens.lens (\StopProductSubscriptionResponse' {productUserSummary} -> productUserSummary) (\s@StopProductSubscriptionResponse' {} a -> s {productUserSummary = a} :: StopProductSubscriptionResponse)

instance
  Prelude.NFData
    StopProductSubscriptionResponse
  where
  rnf StopProductSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf productUserSummary
