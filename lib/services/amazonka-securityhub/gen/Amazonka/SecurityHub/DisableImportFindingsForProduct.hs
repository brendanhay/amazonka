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
-- Module      : Amazonka.SecurityHub.DisableImportFindingsForProduct
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the integration of the specified product with Security Hub.
-- After the integration is disabled, findings from that product are no
-- longer sent to Security Hub.
module Amazonka.SecurityHub.DisableImportFindingsForProduct
  ( -- * Creating a Request
    DisableImportFindingsForProduct (..),
    newDisableImportFindingsForProduct,

    -- * Request Lenses
    disableImportFindingsForProduct_productSubscriptionArn,

    -- * Destructuring the Response
    DisableImportFindingsForProductResponse (..),
    newDisableImportFindingsForProductResponse,

    -- * Response Lenses
    disableImportFindingsForProductResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDisableImportFindingsForProduct' smart constructor.
data DisableImportFindingsForProduct = DisableImportFindingsForProduct'
  { -- | The ARN of the integrated product to disable the integration for.
    productSubscriptionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableImportFindingsForProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productSubscriptionArn', 'disableImportFindingsForProduct_productSubscriptionArn' - The ARN of the integrated product to disable the integration for.
newDisableImportFindingsForProduct ::
  -- | 'productSubscriptionArn'
  Prelude.Text ->
  DisableImportFindingsForProduct
newDisableImportFindingsForProduct
  pProductSubscriptionArn_ =
    DisableImportFindingsForProduct'
      { productSubscriptionArn =
          pProductSubscriptionArn_
      }

-- | The ARN of the integrated product to disable the integration for.
disableImportFindingsForProduct_productSubscriptionArn :: Lens.Lens' DisableImportFindingsForProduct Prelude.Text
disableImportFindingsForProduct_productSubscriptionArn = Lens.lens (\DisableImportFindingsForProduct' {productSubscriptionArn} -> productSubscriptionArn) (\s@DisableImportFindingsForProduct' {} a -> s {productSubscriptionArn = a} :: DisableImportFindingsForProduct)

instance
  Core.AWSRequest
    DisableImportFindingsForProduct
  where
  type
    AWSResponse DisableImportFindingsForProduct =
      DisableImportFindingsForProductResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableImportFindingsForProductResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableImportFindingsForProduct
  where
  hashWithSalt
    _salt
    DisableImportFindingsForProduct' {..} =
      _salt `Prelude.hashWithSalt` productSubscriptionArn

instance
  Prelude.NFData
    DisableImportFindingsForProduct
  where
  rnf DisableImportFindingsForProduct' {..} =
    Prelude.rnf productSubscriptionArn

instance
  Core.ToHeaders
    DisableImportFindingsForProduct
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DisableImportFindingsForProduct where
  toPath DisableImportFindingsForProduct' {..} =
    Prelude.mconcat
      [ "/productSubscriptions/",
        Core.toBS productSubscriptionArn
      ]

instance Core.ToQuery DisableImportFindingsForProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableImportFindingsForProductResponse' smart constructor.
data DisableImportFindingsForProductResponse = DisableImportFindingsForProductResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableImportFindingsForProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableImportFindingsForProductResponse_httpStatus' - The response's http status code.
newDisableImportFindingsForProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableImportFindingsForProductResponse
newDisableImportFindingsForProductResponse
  pHttpStatus_ =
    DisableImportFindingsForProductResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disableImportFindingsForProductResponse_httpStatus :: Lens.Lens' DisableImportFindingsForProductResponse Prelude.Int
disableImportFindingsForProductResponse_httpStatus = Lens.lens (\DisableImportFindingsForProductResponse' {httpStatus} -> httpStatus) (\s@DisableImportFindingsForProductResponse' {} a -> s {httpStatus = a} :: DisableImportFindingsForProductResponse)

instance
  Prelude.NFData
    DisableImportFindingsForProductResponse
  where
  rnf DisableImportFindingsForProductResponse' {..} =
    Prelude.rnf httpStatus
