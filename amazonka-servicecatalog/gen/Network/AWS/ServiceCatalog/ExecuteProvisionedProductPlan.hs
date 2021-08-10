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
-- Module      : Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions or modifies a product based on the resource changes for the
-- specified plan.
module Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
  ( -- * Creating a Request
    ExecuteProvisionedProductPlan (..),
    newExecuteProvisionedProductPlan,

    -- * Request Lenses
    executeProvisionedProductPlan_acceptLanguage,
    executeProvisionedProductPlan_planId,
    executeProvisionedProductPlan_idempotencyToken,

    -- * Destructuring the Response
    ExecuteProvisionedProductPlanResponse (..),
    newExecuteProvisionedProductPlanResponse,

    -- * Response Lenses
    executeProvisionedProductPlanResponse_recordDetail,
    executeProvisionedProductPlanResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newExecuteProvisionedProductPlan' smart constructor.
data ExecuteProvisionedProductPlan = ExecuteProvisionedProductPlan'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The plan identifier.
    planId :: Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteProvisionedProductPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'executeProvisionedProductPlan_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'planId', 'executeProvisionedProductPlan_planId' - The plan identifier.
--
-- 'idempotencyToken', 'executeProvisionedProductPlan_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
newExecuteProvisionedProductPlan ::
  -- | 'planId'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  ExecuteProvisionedProductPlan
newExecuteProvisionedProductPlan
  pPlanId_
  pIdempotencyToken_ =
    ExecuteProvisionedProductPlan'
      { acceptLanguage =
          Prelude.Nothing,
        planId = pPlanId_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
executeProvisionedProductPlan_acceptLanguage :: Lens.Lens' ExecuteProvisionedProductPlan (Prelude.Maybe Prelude.Text)
executeProvisionedProductPlan_acceptLanguage = Lens.lens (\ExecuteProvisionedProductPlan' {acceptLanguage} -> acceptLanguage) (\s@ExecuteProvisionedProductPlan' {} a -> s {acceptLanguage = a} :: ExecuteProvisionedProductPlan)

-- | The plan identifier.
executeProvisionedProductPlan_planId :: Lens.Lens' ExecuteProvisionedProductPlan Prelude.Text
executeProvisionedProductPlan_planId = Lens.lens (\ExecuteProvisionedProductPlan' {planId} -> planId) (\s@ExecuteProvisionedProductPlan' {} a -> s {planId = a} :: ExecuteProvisionedProductPlan)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
executeProvisionedProductPlan_idempotencyToken :: Lens.Lens' ExecuteProvisionedProductPlan Prelude.Text
executeProvisionedProductPlan_idempotencyToken = Lens.lens (\ExecuteProvisionedProductPlan' {idempotencyToken} -> idempotencyToken) (\s@ExecuteProvisionedProductPlan' {} a -> s {idempotencyToken = a} :: ExecuteProvisionedProductPlan)

instance
  Core.AWSRequest
    ExecuteProvisionedProductPlan
  where
  type
    AWSResponse ExecuteProvisionedProductPlan =
      ExecuteProvisionedProductPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteProvisionedProductPlanResponse'
            Prelude.<$> (x Core..?> "RecordDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExecuteProvisionedProductPlan

instance Prelude.NFData ExecuteProvisionedProductPlan

instance Core.ToHeaders ExecuteProvisionedProductPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ExecuteProvisionedProductPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ExecuteProvisionedProductPlan where
  toJSON ExecuteProvisionedProductPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PlanId" Core..= planId),
            Prelude.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath ExecuteProvisionedProductPlan where
  toPath = Prelude.const "/"

instance Core.ToQuery ExecuteProvisionedProductPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteProvisionedProductPlanResponse' smart constructor.
data ExecuteProvisionedProductPlanResponse = ExecuteProvisionedProductPlanResponse'
  { -- | Information about the result of provisioning the product.
    recordDetail :: Prelude.Maybe RecordDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteProvisionedProductPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDetail', 'executeProvisionedProductPlanResponse_recordDetail' - Information about the result of provisioning the product.
--
-- 'httpStatus', 'executeProvisionedProductPlanResponse_httpStatus' - The response's http status code.
newExecuteProvisionedProductPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteProvisionedProductPlanResponse
newExecuteProvisionedProductPlanResponse pHttpStatus_ =
  ExecuteProvisionedProductPlanResponse'
    { recordDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the result of provisioning the product.
executeProvisionedProductPlanResponse_recordDetail :: Lens.Lens' ExecuteProvisionedProductPlanResponse (Prelude.Maybe RecordDetail)
executeProvisionedProductPlanResponse_recordDetail = Lens.lens (\ExecuteProvisionedProductPlanResponse' {recordDetail} -> recordDetail) (\s@ExecuteProvisionedProductPlanResponse' {} a -> s {recordDetail = a} :: ExecuteProvisionedProductPlanResponse)

-- | The response's http status code.
executeProvisionedProductPlanResponse_httpStatus :: Lens.Lens' ExecuteProvisionedProductPlanResponse Prelude.Int
executeProvisionedProductPlanResponse_httpStatus = Lens.lens (\ExecuteProvisionedProductPlanResponse' {httpStatus} -> httpStatus) (\s@ExecuteProvisionedProductPlanResponse' {} a -> s {httpStatus = a} :: ExecuteProvisionedProductPlanResponse)

instance
  Prelude.NFData
    ExecuteProvisionedProductPlanResponse
