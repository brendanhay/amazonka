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
-- Module      : Network.AWS.CostExplorer.DeleteCostCategoryDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Cost Category. Expenses from this month going forward will no
-- longer be categorized with this Cost Category.
module Network.AWS.CostExplorer.DeleteCostCategoryDefinition
  ( -- * Creating a Request
    DeleteCostCategoryDefinition (..),
    newDeleteCostCategoryDefinition,

    -- * Request Lenses
    deleteCostCategoryDefinition_costCategoryArn,

    -- * Destructuring the Response
    DeleteCostCategoryDefinitionResponse (..),
    newDeleteCostCategoryDefinitionResponse,

    -- * Response Lenses
    deleteCostCategoryDefinitionResponse_costCategoryArn,
    deleteCostCategoryDefinitionResponse_effectiveEnd,
    deleteCostCategoryDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCostCategoryDefinition' smart constructor.
data DeleteCostCategoryDefinition = DeleteCostCategoryDefinition'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCostCategoryDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategoryArn', 'deleteCostCategoryDefinition_costCategoryArn' - The unique identifier for your Cost Category.
newDeleteCostCategoryDefinition ::
  -- | 'costCategoryArn'
  Core.Text ->
  DeleteCostCategoryDefinition
newDeleteCostCategoryDefinition pCostCategoryArn_ =
  DeleteCostCategoryDefinition'
    { costCategoryArn =
        pCostCategoryArn_
    }

-- | The unique identifier for your Cost Category.
deleteCostCategoryDefinition_costCategoryArn :: Lens.Lens' DeleteCostCategoryDefinition Core.Text
deleteCostCategoryDefinition_costCategoryArn = Lens.lens (\DeleteCostCategoryDefinition' {costCategoryArn} -> costCategoryArn) (\s@DeleteCostCategoryDefinition' {} a -> s {costCategoryArn = a} :: DeleteCostCategoryDefinition)

instance Core.AWSRequest DeleteCostCategoryDefinition where
  type
    AWSResponse DeleteCostCategoryDefinition =
      DeleteCostCategoryDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCostCategoryDefinitionResponse'
            Core.<$> (x Core..?> "CostCategoryArn")
            Core.<*> (x Core..?> "EffectiveEnd")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCostCategoryDefinition

instance Core.NFData DeleteCostCategoryDefinition

instance Core.ToHeaders DeleteCostCategoryDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.DeleteCostCategoryDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteCostCategoryDefinition where
  toJSON DeleteCostCategoryDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CostCategoryArn" Core..= costCategoryArn)
          ]
      )

instance Core.ToPath DeleteCostCategoryDefinition where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCostCategoryDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteCostCategoryDefinitionResponse' smart constructor.
data DeleteCostCategoryDefinitionResponse = DeleteCostCategoryDefinitionResponse'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Core.Maybe Core.Text,
    -- | The effective end date of the Cost Category as a result of deleting it.
    -- No costs after this date will be categorized by the deleted Cost
    -- Category.
    effectiveEnd :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCostCategoryDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategoryArn', 'deleteCostCategoryDefinitionResponse_costCategoryArn' - The unique identifier for your Cost Category.
--
-- 'effectiveEnd', 'deleteCostCategoryDefinitionResponse_effectiveEnd' - The effective end date of the Cost Category as a result of deleting it.
-- No costs after this date will be categorized by the deleted Cost
-- Category.
--
-- 'httpStatus', 'deleteCostCategoryDefinitionResponse_httpStatus' - The response's http status code.
newDeleteCostCategoryDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteCostCategoryDefinitionResponse
newDeleteCostCategoryDefinitionResponse pHttpStatus_ =
  DeleteCostCategoryDefinitionResponse'
    { costCategoryArn =
        Core.Nothing,
      effectiveEnd = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for your Cost Category.
deleteCostCategoryDefinitionResponse_costCategoryArn :: Lens.Lens' DeleteCostCategoryDefinitionResponse (Core.Maybe Core.Text)
deleteCostCategoryDefinitionResponse_costCategoryArn = Lens.lens (\DeleteCostCategoryDefinitionResponse' {costCategoryArn} -> costCategoryArn) (\s@DeleteCostCategoryDefinitionResponse' {} a -> s {costCategoryArn = a} :: DeleteCostCategoryDefinitionResponse)

-- | The effective end date of the Cost Category as a result of deleting it.
-- No costs after this date will be categorized by the deleted Cost
-- Category.
deleteCostCategoryDefinitionResponse_effectiveEnd :: Lens.Lens' DeleteCostCategoryDefinitionResponse (Core.Maybe Core.Text)
deleteCostCategoryDefinitionResponse_effectiveEnd = Lens.lens (\DeleteCostCategoryDefinitionResponse' {effectiveEnd} -> effectiveEnd) (\s@DeleteCostCategoryDefinitionResponse' {} a -> s {effectiveEnd = a} :: DeleteCostCategoryDefinitionResponse)

-- | The response's http status code.
deleteCostCategoryDefinitionResponse_httpStatus :: Lens.Lens' DeleteCostCategoryDefinitionResponse Core.Int
deleteCostCategoryDefinitionResponse_httpStatus = Lens.lens (\DeleteCostCategoryDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteCostCategoryDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteCostCategoryDefinitionResponse)

instance
  Core.NFData
    DeleteCostCategoryDefinitionResponse
