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
-- Module      : Amazonka.MarketplaceMetering.ResolveCustomer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ResolveCustomer is called by a SaaS application during the registration
-- process. When a buyer visits your website during the registration
-- process, the buyer submits a registration token through their browser.
-- The registration token is resolved through this API to obtain a
-- CustomerIdentifier and product code.
module Amazonka.MarketplaceMetering.ResolveCustomer
  ( -- * Creating a Request
    ResolveCustomer (..),
    newResolveCustomer,

    -- * Request Lenses
    resolveCustomer_registrationToken,

    -- * Destructuring the Response
    ResolveCustomerResponse (..),
    newResolveCustomerResponse,

    -- * Response Lenses
    resolveCustomerResponse_customerIdentifier,
    resolveCustomerResponse_productCode,
    resolveCustomerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MarketplaceMetering.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains input to the ResolveCustomer operation.
--
-- /See:/ 'newResolveCustomer' smart constructor.
data ResolveCustomer = ResolveCustomer'
  { -- | When a buyer visits your website during the registration process, the
    -- buyer submits a registration token through the browser. The registration
    -- token is resolved to obtain a CustomerIdentifier and product code.
    registrationToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveCustomer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registrationToken', 'resolveCustomer_registrationToken' - When a buyer visits your website during the registration process, the
-- buyer submits a registration token through the browser. The registration
-- token is resolved to obtain a CustomerIdentifier and product code.
newResolveCustomer ::
  -- | 'registrationToken'
  Prelude.Text ->
  ResolveCustomer
newResolveCustomer pRegistrationToken_ =
  ResolveCustomer'
    { registrationToken =
        pRegistrationToken_
    }

-- | When a buyer visits your website during the registration process, the
-- buyer submits a registration token through the browser. The registration
-- token is resolved to obtain a CustomerIdentifier and product code.
resolveCustomer_registrationToken :: Lens.Lens' ResolveCustomer Prelude.Text
resolveCustomer_registrationToken = Lens.lens (\ResolveCustomer' {registrationToken} -> registrationToken) (\s@ResolveCustomer' {} a -> s {registrationToken = a} :: ResolveCustomer)

instance Core.AWSRequest ResolveCustomer where
  type
    AWSResponse ResolveCustomer =
      ResolveCustomerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveCustomerResponse'
            Prelude.<$> (x Core..?> "CustomerIdentifier")
            Prelude.<*> (x Core..?> "ProductCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResolveCustomer

instance Prelude.NFData ResolveCustomer

instance Core.ToHeaders ResolveCustomer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMPMeteringService.ResolveCustomer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ResolveCustomer where
  toJSON ResolveCustomer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RegistrationToken" Core..= registrationToken)
          ]
      )

instance Core.ToPath ResolveCustomer where
  toPath = Prelude.const "/"

instance Core.ToQuery ResolveCustomer where
  toQuery = Prelude.const Prelude.mempty

-- | The result of the ResolveCustomer operation. Contains the
-- CustomerIdentifier and product code.
--
-- /See:/ 'newResolveCustomerResponse' smart constructor.
data ResolveCustomerResponse = ResolveCustomerResponse'
  { -- | The CustomerIdentifier is used to identify an individual customer in
    -- your application. Calls to BatchMeterUsage require CustomerIdentifiers
    -- for each UsageRecord.
    customerIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The product code is returned to confirm that the buyer is registering
    -- for your product. Subsequent BatchMeterUsage calls should be made using
    -- this product code.
    productCode :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveCustomerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerIdentifier', 'resolveCustomerResponse_customerIdentifier' - The CustomerIdentifier is used to identify an individual customer in
-- your application. Calls to BatchMeterUsage require CustomerIdentifiers
-- for each UsageRecord.
--
-- 'productCode', 'resolveCustomerResponse_productCode' - The product code is returned to confirm that the buyer is registering
-- for your product. Subsequent BatchMeterUsage calls should be made using
-- this product code.
--
-- 'httpStatus', 'resolveCustomerResponse_httpStatus' - The response's http status code.
newResolveCustomerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResolveCustomerResponse
newResolveCustomerResponse pHttpStatus_ =
  ResolveCustomerResponse'
    { customerIdentifier =
        Prelude.Nothing,
      productCode = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The CustomerIdentifier is used to identify an individual customer in
-- your application. Calls to BatchMeterUsage require CustomerIdentifiers
-- for each UsageRecord.
resolveCustomerResponse_customerIdentifier :: Lens.Lens' ResolveCustomerResponse (Prelude.Maybe Prelude.Text)
resolveCustomerResponse_customerIdentifier = Lens.lens (\ResolveCustomerResponse' {customerIdentifier} -> customerIdentifier) (\s@ResolveCustomerResponse' {} a -> s {customerIdentifier = a} :: ResolveCustomerResponse)

-- | The product code is returned to confirm that the buyer is registering
-- for your product. Subsequent BatchMeterUsage calls should be made using
-- this product code.
resolveCustomerResponse_productCode :: Lens.Lens' ResolveCustomerResponse (Prelude.Maybe Prelude.Text)
resolveCustomerResponse_productCode = Lens.lens (\ResolveCustomerResponse' {productCode} -> productCode) (\s@ResolveCustomerResponse' {} a -> s {productCode = a} :: ResolveCustomerResponse)

-- | The response's http status code.
resolveCustomerResponse_httpStatus :: Lens.Lens' ResolveCustomerResponse Prelude.Int
resolveCustomerResponse_httpStatus = Lens.lens (\ResolveCustomerResponse' {httpStatus} -> httpStatus) (\s@ResolveCustomerResponse' {} a -> s {httpStatus = a} :: ResolveCustomerResponse)

instance Prelude.NFData ResolveCustomerResponse
