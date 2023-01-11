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
-- Module      : Amazonka.SecurityHub.EnableImportFindingsForProduct
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the integration of a partner product with Security Hub.
-- Integrated products send findings to Security Hub.
--
-- When you enable a product integration, a permissions policy that grants
-- permission for the product to send findings to Security Hub is applied.
module Amazonka.SecurityHub.EnableImportFindingsForProduct
  ( -- * Creating a Request
    EnableImportFindingsForProduct (..),
    newEnableImportFindingsForProduct,

    -- * Request Lenses
    enableImportFindingsForProduct_productArn,

    -- * Destructuring the Response
    EnableImportFindingsForProductResponse (..),
    newEnableImportFindingsForProductResponse,

    -- * Response Lenses
    enableImportFindingsForProductResponse_productSubscriptionArn,
    enableImportFindingsForProductResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newEnableImportFindingsForProduct' smart constructor.
data EnableImportFindingsForProduct = EnableImportFindingsForProduct'
  { -- | The ARN of the product to enable the integration for.
    productArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableImportFindingsForProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productArn', 'enableImportFindingsForProduct_productArn' - The ARN of the product to enable the integration for.
newEnableImportFindingsForProduct ::
  -- | 'productArn'
  Prelude.Text ->
  EnableImportFindingsForProduct
newEnableImportFindingsForProduct pProductArn_ =
  EnableImportFindingsForProduct'
    { productArn =
        pProductArn_
    }

-- | The ARN of the product to enable the integration for.
enableImportFindingsForProduct_productArn :: Lens.Lens' EnableImportFindingsForProduct Prelude.Text
enableImportFindingsForProduct_productArn = Lens.lens (\EnableImportFindingsForProduct' {productArn} -> productArn) (\s@EnableImportFindingsForProduct' {} a -> s {productArn = a} :: EnableImportFindingsForProduct)

instance
  Core.AWSRequest
    EnableImportFindingsForProduct
  where
  type
    AWSResponse EnableImportFindingsForProduct =
      EnableImportFindingsForProductResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableImportFindingsForProductResponse'
            Prelude.<$> (x Data..?> "ProductSubscriptionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableImportFindingsForProduct
  where
  hashWithSalt
    _salt
    EnableImportFindingsForProduct' {..} =
      _salt `Prelude.hashWithSalt` productArn

instance
  Prelude.NFData
    EnableImportFindingsForProduct
  where
  rnf EnableImportFindingsForProduct' {..} =
    Prelude.rnf productArn

instance
  Data.ToHeaders
    EnableImportFindingsForProduct
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableImportFindingsForProduct where
  toJSON EnableImportFindingsForProduct' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProductArn" Data..= productArn)]
      )

instance Data.ToPath EnableImportFindingsForProduct where
  toPath = Prelude.const "/productSubscriptions"

instance Data.ToQuery EnableImportFindingsForProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableImportFindingsForProductResponse' smart constructor.
data EnableImportFindingsForProductResponse = EnableImportFindingsForProductResponse'
  { -- | The ARN of your subscription to the product to enable integrations for.
    productSubscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableImportFindingsForProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productSubscriptionArn', 'enableImportFindingsForProductResponse_productSubscriptionArn' - The ARN of your subscription to the product to enable integrations for.
--
-- 'httpStatus', 'enableImportFindingsForProductResponse_httpStatus' - The response's http status code.
newEnableImportFindingsForProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableImportFindingsForProductResponse
newEnableImportFindingsForProductResponse
  pHttpStatus_ =
    EnableImportFindingsForProductResponse'
      { productSubscriptionArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of your subscription to the product to enable integrations for.
enableImportFindingsForProductResponse_productSubscriptionArn :: Lens.Lens' EnableImportFindingsForProductResponse (Prelude.Maybe Prelude.Text)
enableImportFindingsForProductResponse_productSubscriptionArn = Lens.lens (\EnableImportFindingsForProductResponse' {productSubscriptionArn} -> productSubscriptionArn) (\s@EnableImportFindingsForProductResponse' {} a -> s {productSubscriptionArn = a} :: EnableImportFindingsForProductResponse)

-- | The response's http status code.
enableImportFindingsForProductResponse_httpStatus :: Lens.Lens' EnableImportFindingsForProductResponse Prelude.Int
enableImportFindingsForProductResponse_httpStatus = Lens.lens (\EnableImportFindingsForProductResponse' {httpStatus} -> httpStatus) (\s@EnableImportFindingsForProductResponse' {} a -> s {httpStatus = a} :: EnableImportFindingsForProductResponse)

instance
  Prelude.NFData
    EnableImportFindingsForProductResponse
  where
  rnf EnableImportFindingsForProductResponse' {..} =
    Prelude.rnf productSubscriptionArn
      `Prelude.seq` Prelude.rnf httpStatus
