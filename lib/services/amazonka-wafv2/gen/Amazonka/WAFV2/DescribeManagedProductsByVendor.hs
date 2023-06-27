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
-- Module      : Amazonka.WAFV2.DescribeManagedProductsByVendor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides high-level information for the managed rule groups owned by a
-- specific vendor.
module Amazonka.WAFV2.DescribeManagedProductsByVendor
  ( -- * Creating a Request
    DescribeManagedProductsByVendor (..),
    newDescribeManagedProductsByVendor,

    -- * Request Lenses
    describeManagedProductsByVendor_vendorName,
    describeManagedProductsByVendor_scope,

    -- * Destructuring the Response
    DescribeManagedProductsByVendorResponse (..),
    newDescribeManagedProductsByVendorResponse,

    -- * Response Lenses
    describeManagedProductsByVendorResponse_managedProducts,
    describeManagedProductsByVendorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDescribeManagedProductsByVendor' smart constructor.
data DescribeManagedProductsByVendor = DescribeManagedProductsByVendor'
  { -- | The name of the managed rule group vendor. You use this, along with the
    -- rule group name, to identify a rule group.
    vendorName :: Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
    -- Services Verified Access instance.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedProductsByVendor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vendorName', 'describeManagedProductsByVendor_vendorName' - The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify a rule group.
--
-- 'scope', 'describeManagedProductsByVendor_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
newDescribeManagedProductsByVendor ::
  -- | 'vendorName'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  DescribeManagedProductsByVendor
newDescribeManagedProductsByVendor
  pVendorName_
  pScope_ =
    DescribeManagedProductsByVendor'
      { vendorName =
          pVendorName_,
        scope = pScope_
      }

-- | The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify a rule group.
describeManagedProductsByVendor_vendorName :: Lens.Lens' DescribeManagedProductsByVendor Prelude.Text
describeManagedProductsByVendor_vendorName = Lens.lens (\DescribeManagedProductsByVendor' {vendorName} -> vendorName) (\s@DescribeManagedProductsByVendor' {} a -> s {vendorName = a} :: DescribeManagedProductsByVendor)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
describeManagedProductsByVendor_scope :: Lens.Lens' DescribeManagedProductsByVendor Scope
describeManagedProductsByVendor_scope = Lens.lens (\DescribeManagedProductsByVendor' {scope} -> scope) (\s@DescribeManagedProductsByVendor' {} a -> s {scope = a} :: DescribeManagedProductsByVendor)

instance
  Core.AWSRequest
    DescribeManagedProductsByVendor
  where
  type
    AWSResponse DescribeManagedProductsByVendor =
      DescribeManagedProductsByVendorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeManagedProductsByVendorResponse'
            Prelude.<$> ( x
                            Data..?> "ManagedProducts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeManagedProductsByVendor
  where
  hashWithSalt
    _salt
    DescribeManagedProductsByVendor' {..} =
      _salt
        `Prelude.hashWithSalt` vendorName
        `Prelude.hashWithSalt` scope

instance
  Prelude.NFData
    DescribeManagedProductsByVendor
  where
  rnf DescribeManagedProductsByVendor' {..} =
    Prelude.rnf vendorName
      `Prelude.seq` Prelude.rnf scope

instance
  Data.ToHeaders
    DescribeManagedProductsByVendor
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DescribeManagedProductsByVendor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeManagedProductsByVendor where
  toJSON DescribeManagedProductsByVendor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("VendorName" Data..= vendorName),
            Prelude.Just ("Scope" Data..= scope)
          ]
      )

instance Data.ToPath DescribeManagedProductsByVendor where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeManagedProductsByVendor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeManagedProductsByVendorResponse' smart constructor.
data DescribeManagedProductsByVendorResponse = DescribeManagedProductsByVendorResponse'
  { -- | High-level information for the managed rule groups owned by the
    -- specified vendor.
    managedProducts :: Prelude.Maybe [ManagedProductDescriptor],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedProductsByVendorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedProducts', 'describeManagedProductsByVendorResponse_managedProducts' - High-level information for the managed rule groups owned by the
-- specified vendor.
--
-- 'httpStatus', 'describeManagedProductsByVendorResponse_httpStatus' - The response's http status code.
newDescribeManagedProductsByVendorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeManagedProductsByVendorResponse
newDescribeManagedProductsByVendorResponse
  pHttpStatus_ =
    DescribeManagedProductsByVendorResponse'
      { managedProducts =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | High-level information for the managed rule groups owned by the
-- specified vendor.
describeManagedProductsByVendorResponse_managedProducts :: Lens.Lens' DescribeManagedProductsByVendorResponse (Prelude.Maybe [ManagedProductDescriptor])
describeManagedProductsByVendorResponse_managedProducts = Lens.lens (\DescribeManagedProductsByVendorResponse' {managedProducts} -> managedProducts) (\s@DescribeManagedProductsByVendorResponse' {} a -> s {managedProducts = a} :: DescribeManagedProductsByVendorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeManagedProductsByVendorResponse_httpStatus :: Lens.Lens' DescribeManagedProductsByVendorResponse Prelude.Int
describeManagedProductsByVendorResponse_httpStatus = Lens.lens (\DescribeManagedProductsByVendorResponse' {httpStatus} -> httpStatus) (\s@DescribeManagedProductsByVendorResponse' {} a -> s {httpStatus = a} :: DescribeManagedProductsByVendorResponse)

instance
  Prelude.NFData
    DescribeManagedProductsByVendorResponse
  where
  rnf DescribeManagedProductsByVendorResponse' {..} =
    Prelude.rnf managedProducts
      `Prelude.seq` Prelude.rnf httpStatus
