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
-- Module      : Amazonka.WAFV2.DescribeAllManagedProducts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides high-level information for the Amazon Web Services Managed
-- Rules rule groups and Amazon Web Services Marketplace managed rule
-- groups.
module Amazonka.WAFV2.DescribeAllManagedProducts
  ( -- * Creating a Request
    DescribeAllManagedProducts (..),
    newDescribeAllManagedProducts,

    -- * Request Lenses
    describeAllManagedProducts_scope,

    -- * Destructuring the Response
    DescribeAllManagedProductsResponse (..),
    newDescribeAllManagedProductsResponse,

    -- * Response Lenses
    describeAllManagedProductsResponse_managedProducts,
    describeAllManagedProductsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDescribeAllManagedProducts' smart constructor.
data DescribeAllManagedProducts = DescribeAllManagedProducts'
  { -- | Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- Create a value of 'DescribeAllManagedProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'describeAllManagedProducts_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
newDescribeAllManagedProducts ::
  -- | 'scope'
  Scope ->
  DescribeAllManagedProducts
newDescribeAllManagedProducts pScope_ =
  DescribeAllManagedProducts' {scope = pScope_}

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
describeAllManagedProducts_scope :: Lens.Lens' DescribeAllManagedProducts Scope
describeAllManagedProducts_scope = Lens.lens (\DescribeAllManagedProducts' {scope} -> scope) (\s@DescribeAllManagedProducts' {} a -> s {scope = a} :: DescribeAllManagedProducts)

instance Core.AWSRequest DescribeAllManagedProducts where
  type
    AWSResponse DescribeAllManagedProducts =
      DescribeAllManagedProductsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAllManagedProductsResponse'
            Prelude.<$> ( x
                            Data..?> "ManagedProducts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAllManagedProducts where
  hashWithSalt _salt DescribeAllManagedProducts' {..} =
    _salt `Prelude.hashWithSalt` scope

instance Prelude.NFData DescribeAllManagedProducts where
  rnf DescribeAllManagedProducts' {..} =
    Prelude.rnf scope

instance Data.ToHeaders DescribeAllManagedProducts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DescribeAllManagedProducts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAllManagedProducts where
  toJSON DescribeAllManagedProducts' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Scope" Data..= scope)]
      )

instance Data.ToPath DescribeAllManagedProducts where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAllManagedProducts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAllManagedProductsResponse' smart constructor.
data DescribeAllManagedProductsResponse = DescribeAllManagedProductsResponse'
  { -- | High-level information for the Amazon Web Services Managed Rules rule
    -- groups and Amazon Web Services Marketplace managed rule groups.
    managedProducts :: Prelude.Maybe [ManagedProductDescriptor],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAllManagedProductsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedProducts', 'describeAllManagedProductsResponse_managedProducts' - High-level information for the Amazon Web Services Managed Rules rule
-- groups and Amazon Web Services Marketplace managed rule groups.
--
-- 'httpStatus', 'describeAllManagedProductsResponse_httpStatus' - The response's http status code.
newDescribeAllManagedProductsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAllManagedProductsResponse
newDescribeAllManagedProductsResponse pHttpStatus_ =
  DescribeAllManagedProductsResponse'
    { managedProducts =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | High-level information for the Amazon Web Services Managed Rules rule
-- groups and Amazon Web Services Marketplace managed rule groups.
describeAllManagedProductsResponse_managedProducts :: Lens.Lens' DescribeAllManagedProductsResponse (Prelude.Maybe [ManagedProductDescriptor])
describeAllManagedProductsResponse_managedProducts = Lens.lens (\DescribeAllManagedProductsResponse' {managedProducts} -> managedProducts) (\s@DescribeAllManagedProductsResponse' {} a -> s {managedProducts = a} :: DescribeAllManagedProductsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAllManagedProductsResponse_httpStatus :: Lens.Lens' DescribeAllManagedProductsResponse Prelude.Int
describeAllManagedProductsResponse_httpStatus = Lens.lens (\DescribeAllManagedProductsResponse' {httpStatus} -> httpStatus) (\s@DescribeAllManagedProductsResponse' {} a -> s {httpStatus = a} :: DescribeAllManagedProductsResponse)

instance
  Prelude.NFData
    DescribeAllManagedProductsResponse
  where
  rnf DescribeAllManagedProductsResponse' {..} =
    Prelude.rnf managedProducts
      `Prelude.seq` Prelude.rnf httpStatus
