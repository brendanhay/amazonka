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
-- Module      : Network.AWS.CostExplorer.DescribeCostCategoryDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name, ARN, rules, definition, and effective dates of a Cost
-- Category that\'s defined in the account.
--
-- You have the option to use @EffectiveOn@ to return a Cost Category that
-- is active on a specific date. If there is no @EffectiveOn@ specified,
-- you’ll see a Cost Category that is effective on the current date. If
-- Cost Category is still effective, @EffectiveEnd@ is omitted in the
-- response.
module Network.AWS.CostExplorer.DescribeCostCategoryDefinition
  ( -- * Creating a Request
    DescribeCostCategoryDefinition (..),
    newDescribeCostCategoryDefinition,

    -- * Request Lenses
    describeCostCategoryDefinition_effectiveOn,
    describeCostCategoryDefinition_costCategoryArn,

    -- * Destructuring the Response
    DescribeCostCategoryDefinitionResponse (..),
    newDescribeCostCategoryDefinitionResponse,

    -- * Response Lenses
    describeCostCategoryDefinitionResponse_costCategory,
    describeCostCategoryDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCostCategoryDefinition' smart constructor.
data DescribeCostCategoryDefinition = DescribeCostCategoryDefinition'
  { -- | The date when the Cost Category was effective.
    effectiveOn :: Core.Maybe Core.Text,
    -- | The unique identifier for your Cost Category.
    costCategoryArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCostCategoryDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectiveOn', 'describeCostCategoryDefinition_effectiveOn' - The date when the Cost Category was effective.
--
-- 'costCategoryArn', 'describeCostCategoryDefinition_costCategoryArn' - The unique identifier for your Cost Category.
newDescribeCostCategoryDefinition ::
  -- | 'costCategoryArn'
  Core.Text ->
  DescribeCostCategoryDefinition
newDescribeCostCategoryDefinition pCostCategoryArn_ =
  DescribeCostCategoryDefinition'
    { effectiveOn =
        Core.Nothing,
      costCategoryArn = pCostCategoryArn_
    }

-- | The date when the Cost Category was effective.
describeCostCategoryDefinition_effectiveOn :: Lens.Lens' DescribeCostCategoryDefinition (Core.Maybe Core.Text)
describeCostCategoryDefinition_effectiveOn = Lens.lens (\DescribeCostCategoryDefinition' {effectiveOn} -> effectiveOn) (\s@DescribeCostCategoryDefinition' {} a -> s {effectiveOn = a} :: DescribeCostCategoryDefinition)

-- | The unique identifier for your Cost Category.
describeCostCategoryDefinition_costCategoryArn :: Lens.Lens' DescribeCostCategoryDefinition Core.Text
describeCostCategoryDefinition_costCategoryArn = Lens.lens (\DescribeCostCategoryDefinition' {costCategoryArn} -> costCategoryArn) (\s@DescribeCostCategoryDefinition' {} a -> s {costCategoryArn = a} :: DescribeCostCategoryDefinition)

instance
  Core.AWSRequest
    DescribeCostCategoryDefinition
  where
  type
    AWSResponse DescribeCostCategoryDefinition =
      DescribeCostCategoryDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCostCategoryDefinitionResponse'
            Core.<$> (x Core..?> "CostCategory")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCostCategoryDefinition

instance Core.NFData DescribeCostCategoryDefinition

instance
  Core.ToHeaders
    DescribeCostCategoryDefinition
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.DescribeCostCategoryDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCostCategoryDefinition where
  toJSON DescribeCostCategoryDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EffectiveOn" Core..=) Core.<$> effectiveOn,
            Core.Just
              ("CostCategoryArn" Core..= costCategoryArn)
          ]
      )

instance Core.ToPath DescribeCostCategoryDefinition where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCostCategoryDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCostCategoryDefinitionResponse' smart constructor.
data DescribeCostCategoryDefinitionResponse = DescribeCostCategoryDefinitionResponse'
  { costCategory :: Core.Maybe CostCategory,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCostCategoryDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategory', 'describeCostCategoryDefinitionResponse_costCategory' - Undocumented member.
--
-- 'httpStatus', 'describeCostCategoryDefinitionResponse_httpStatus' - The response's http status code.
newDescribeCostCategoryDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCostCategoryDefinitionResponse
newDescribeCostCategoryDefinitionResponse
  pHttpStatus_ =
    DescribeCostCategoryDefinitionResponse'
      { costCategory =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeCostCategoryDefinitionResponse_costCategory :: Lens.Lens' DescribeCostCategoryDefinitionResponse (Core.Maybe CostCategory)
describeCostCategoryDefinitionResponse_costCategory = Lens.lens (\DescribeCostCategoryDefinitionResponse' {costCategory} -> costCategory) (\s@DescribeCostCategoryDefinitionResponse' {} a -> s {costCategory = a} :: DescribeCostCategoryDefinitionResponse)

-- | The response's http status code.
describeCostCategoryDefinitionResponse_httpStatus :: Lens.Lens' DescribeCostCategoryDefinitionResponse Core.Int
describeCostCategoryDefinitionResponse_httpStatus = Lens.lens (\DescribeCostCategoryDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeCostCategoryDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeCostCategoryDefinitionResponse)

instance
  Core.NFData
    DescribeCostCategoryDefinitionResponse
