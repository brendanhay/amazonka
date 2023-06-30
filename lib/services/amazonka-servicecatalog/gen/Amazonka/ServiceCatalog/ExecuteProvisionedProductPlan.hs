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
-- Module      : Amazonka.ServiceCatalog.ExecuteProvisionedProductPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions or modifies a product based on the resource changes for the
-- specified plan.
module Amazonka.ServiceCatalog.ExecuteProvisionedProductPlan
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteProvisionedProductPlanResponse'
            Prelude.<$> (x Data..?> "RecordDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExecuteProvisionedProductPlan
  where
  hashWithSalt _salt ExecuteProvisionedProductPlan' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` planId
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData ExecuteProvisionedProductPlan where
  rnf ExecuteProvisionedProductPlan' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf planId
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Data.ToHeaders ExecuteProvisionedProductPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ExecuteProvisionedProductPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExecuteProvisionedProductPlan where
  toJSON ExecuteProvisionedProductPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PlanId" Data..= planId),
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath ExecuteProvisionedProductPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery ExecuteProvisionedProductPlan where
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
  where
  rnf ExecuteProvisionedProductPlanResponse' {..} =
    Prelude.rnf recordDetail
      `Prelude.seq` Prelude.rnf httpStatus
