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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.StartProductSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a product subscription for a user with the specified identity
-- provider.
--
-- Your estimated bill for charges on the number of users and related costs
-- will take 48 hours to appear for billing periods that haven\'t closed
-- (marked as __Pending__ billing status) in Amazon Web Services Billing.
-- For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/invoice.html Viewing your monthly charges>
-- in the /Amazon Web Services Billing User Guide/.
module Amazonka.LicenseManagerUserSubscriptions.StartProductSubscription
  ( -- * Creating a Request
    StartProductSubscription (..),
    newStartProductSubscription,

    -- * Request Lenses
    startProductSubscription_domain,
    startProductSubscription_identityProvider,
    startProductSubscription_product,
    startProductSubscription_username,

    -- * Destructuring the Response
    StartProductSubscriptionResponse (..),
    newStartProductSubscriptionResponse,

    -- * Response Lenses
    startProductSubscriptionResponse_httpStatus,
    startProductSubscriptionResponse_productUserSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartProductSubscription' smart constructor.
data StartProductSubscription = StartProductSubscription'
  { -- | The domain name of the user.
    domain :: Prelude.Maybe Prelude.Text,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The name of the user-based subscription product.
    product :: Prelude.Text,
    -- | The user name from the identity provider of the user.
    username :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartProductSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'startProductSubscription_domain' - The domain name of the user.
--
-- 'identityProvider', 'startProductSubscription_identityProvider' - An object that specifies details for the identity provider.
--
-- 'product', 'startProductSubscription_product' - The name of the user-based subscription product.
--
-- 'username', 'startProductSubscription_username' - The user name from the identity provider of the user.
newStartProductSubscription ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'product'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  StartProductSubscription
newStartProductSubscription
  pIdentityProvider_
  pProduct_
  pUsername_ =
    StartProductSubscription'
      { domain = Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        product = pProduct_,
        username = pUsername_
      }

-- | The domain name of the user.
startProductSubscription_domain :: Lens.Lens' StartProductSubscription (Prelude.Maybe Prelude.Text)
startProductSubscription_domain = Lens.lens (\StartProductSubscription' {domain} -> domain) (\s@StartProductSubscription' {} a -> s {domain = a} :: StartProductSubscription)

-- | An object that specifies details for the identity provider.
startProductSubscription_identityProvider :: Lens.Lens' StartProductSubscription IdentityProvider
startProductSubscription_identityProvider = Lens.lens (\StartProductSubscription' {identityProvider} -> identityProvider) (\s@StartProductSubscription' {} a -> s {identityProvider = a} :: StartProductSubscription)

-- | The name of the user-based subscription product.
startProductSubscription_product :: Lens.Lens' StartProductSubscription Prelude.Text
startProductSubscription_product = Lens.lens (\StartProductSubscription' {product} -> product) (\s@StartProductSubscription' {} a -> s {product = a} :: StartProductSubscription)

-- | The user name from the identity provider of the user.
startProductSubscription_username :: Lens.Lens' StartProductSubscription Prelude.Text
startProductSubscription_username = Lens.lens (\StartProductSubscription' {username} -> username) (\s@StartProductSubscription' {} a -> s {username = a} :: StartProductSubscription)

instance Core.AWSRequest StartProductSubscription where
  type
    AWSResponse StartProductSubscription =
      StartProductSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartProductSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProductUserSummary")
      )

instance Prelude.Hashable StartProductSubscription where
  hashWithSalt _salt StartProductSubscription' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` product
      `Prelude.hashWithSalt` username

instance Prelude.NFData StartProductSubscription where
  rnf StartProductSubscription' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf product
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders StartProductSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartProductSubscription where
  toJSON StartProductSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("Product" Data..= product),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath StartProductSubscription where
  toPath =
    Prelude.const "/user/StartProductSubscription"

instance Data.ToQuery StartProductSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartProductSubscriptionResponse' smart constructor.
data StartProductSubscriptionResponse = StartProductSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata that describes the start product subscription operation.
    productUserSummary :: ProductUserSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartProductSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startProductSubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'productUserSummary', 'startProductSubscriptionResponse_productUserSummary' - Metadata that describes the start product subscription operation.
newStartProductSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'productUserSummary'
  ProductUserSummary ->
  StartProductSubscriptionResponse
newStartProductSubscriptionResponse
  pHttpStatus_
  pProductUserSummary_ =
    StartProductSubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        productUserSummary = pProductUserSummary_
      }

-- | The response's http status code.
startProductSubscriptionResponse_httpStatus :: Lens.Lens' StartProductSubscriptionResponse Prelude.Int
startProductSubscriptionResponse_httpStatus = Lens.lens (\StartProductSubscriptionResponse' {httpStatus} -> httpStatus) (\s@StartProductSubscriptionResponse' {} a -> s {httpStatus = a} :: StartProductSubscriptionResponse)

-- | Metadata that describes the start product subscription operation.
startProductSubscriptionResponse_productUserSummary :: Lens.Lens' StartProductSubscriptionResponse ProductUserSummary
startProductSubscriptionResponse_productUserSummary = Lens.lens (\StartProductSubscriptionResponse' {productUserSummary} -> productUserSummary) (\s@StartProductSubscriptionResponse' {} a -> s {productUserSummary = a} :: StartProductSubscriptionResponse)

instance
  Prelude.NFData
    StartProductSubscriptionResponse
  where
  rnf StartProductSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf productUserSummary
