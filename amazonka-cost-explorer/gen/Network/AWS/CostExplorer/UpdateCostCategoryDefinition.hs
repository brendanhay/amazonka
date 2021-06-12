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
-- Module      : Network.AWS.CostExplorer.UpdateCostCategoryDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Cost Category. Changes made to the Cost Category
-- rules will be used to categorize the current month’s expenses and future
-- expenses. This won’t change categorization for the previous months.
module Network.AWS.CostExplorer.UpdateCostCategoryDefinition
  ( -- * Creating a Request
    UpdateCostCategoryDefinition (..),
    newUpdateCostCategoryDefinition,

    -- * Request Lenses
    updateCostCategoryDefinition_costCategoryArn,
    updateCostCategoryDefinition_ruleVersion,
    updateCostCategoryDefinition_rules,

    -- * Destructuring the Response
    UpdateCostCategoryDefinitionResponse (..),
    newUpdateCostCategoryDefinitionResponse,

    -- * Response Lenses
    updateCostCategoryDefinitionResponse_costCategoryArn,
    updateCostCategoryDefinitionResponse_effectiveStart,
    updateCostCategoryDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCostCategoryDefinition' smart constructor.
data UpdateCostCategoryDefinition = UpdateCostCategoryDefinition'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Core.Text,
    ruleVersion :: CostCategoryRuleVersion,
    -- | The @Expression@ object used to categorize costs. For more information,
    -- see
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule>
    -- .
    rules :: Core.NonEmpty CostCategoryRule
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCostCategoryDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategoryArn', 'updateCostCategoryDefinition_costCategoryArn' - The unique identifier for your Cost Category.
--
-- 'ruleVersion', 'updateCostCategoryDefinition_ruleVersion' - Undocumented member.
--
-- 'rules', 'updateCostCategoryDefinition_rules' - The @Expression@ object used to categorize costs. For more information,
-- see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule>
-- .
newUpdateCostCategoryDefinition ::
  -- | 'costCategoryArn'
  Core.Text ->
  -- | 'ruleVersion'
  CostCategoryRuleVersion ->
  -- | 'rules'
  Core.NonEmpty CostCategoryRule ->
  UpdateCostCategoryDefinition
newUpdateCostCategoryDefinition
  pCostCategoryArn_
  pRuleVersion_
  pRules_ =
    UpdateCostCategoryDefinition'
      { costCategoryArn =
          pCostCategoryArn_,
        ruleVersion = pRuleVersion_,
        rules = Lens._Coerce Lens.# pRules_
      }

-- | The unique identifier for your Cost Category.
updateCostCategoryDefinition_costCategoryArn :: Lens.Lens' UpdateCostCategoryDefinition Core.Text
updateCostCategoryDefinition_costCategoryArn = Lens.lens (\UpdateCostCategoryDefinition' {costCategoryArn} -> costCategoryArn) (\s@UpdateCostCategoryDefinition' {} a -> s {costCategoryArn = a} :: UpdateCostCategoryDefinition)

-- | Undocumented member.
updateCostCategoryDefinition_ruleVersion :: Lens.Lens' UpdateCostCategoryDefinition CostCategoryRuleVersion
updateCostCategoryDefinition_ruleVersion = Lens.lens (\UpdateCostCategoryDefinition' {ruleVersion} -> ruleVersion) (\s@UpdateCostCategoryDefinition' {} a -> s {ruleVersion = a} :: UpdateCostCategoryDefinition)

-- | The @Expression@ object used to categorize costs. For more information,
-- see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule>
-- .
updateCostCategoryDefinition_rules :: Lens.Lens' UpdateCostCategoryDefinition (Core.NonEmpty CostCategoryRule)
updateCostCategoryDefinition_rules = Lens.lens (\UpdateCostCategoryDefinition' {rules} -> rules) (\s@UpdateCostCategoryDefinition' {} a -> s {rules = a} :: UpdateCostCategoryDefinition) Core.. Lens._Coerce

instance Core.AWSRequest UpdateCostCategoryDefinition where
  type
    AWSResponse UpdateCostCategoryDefinition =
      UpdateCostCategoryDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCostCategoryDefinitionResponse'
            Core.<$> (x Core..?> "CostCategoryArn")
            Core.<*> (x Core..?> "EffectiveStart")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateCostCategoryDefinition

instance Core.NFData UpdateCostCategoryDefinition

instance Core.ToHeaders UpdateCostCategoryDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.UpdateCostCategoryDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCostCategoryDefinition where
  toJSON UpdateCostCategoryDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CostCategoryArn" Core..= costCategoryArn),
            Core.Just ("RuleVersion" Core..= ruleVersion),
            Core.Just ("Rules" Core..= rules)
          ]
      )

instance Core.ToPath UpdateCostCategoryDefinition where
  toPath = Core.const "/"

instance Core.ToQuery UpdateCostCategoryDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCostCategoryDefinitionResponse' smart constructor.
data UpdateCostCategoryDefinitionResponse = UpdateCostCategoryDefinitionResponse'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Core.Maybe Core.Text,
    -- | The Cost Category\'s effective start date.
    effectiveStart :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCostCategoryDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategoryArn', 'updateCostCategoryDefinitionResponse_costCategoryArn' - The unique identifier for your Cost Category.
--
-- 'effectiveStart', 'updateCostCategoryDefinitionResponse_effectiveStart' - The Cost Category\'s effective start date.
--
-- 'httpStatus', 'updateCostCategoryDefinitionResponse_httpStatus' - The response's http status code.
newUpdateCostCategoryDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateCostCategoryDefinitionResponse
newUpdateCostCategoryDefinitionResponse pHttpStatus_ =
  UpdateCostCategoryDefinitionResponse'
    { costCategoryArn =
        Core.Nothing,
      effectiveStart = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for your Cost Category.
updateCostCategoryDefinitionResponse_costCategoryArn :: Lens.Lens' UpdateCostCategoryDefinitionResponse (Core.Maybe Core.Text)
updateCostCategoryDefinitionResponse_costCategoryArn = Lens.lens (\UpdateCostCategoryDefinitionResponse' {costCategoryArn} -> costCategoryArn) (\s@UpdateCostCategoryDefinitionResponse' {} a -> s {costCategoryArn = a} :: UpdateCostCategoryDefinitionResponse)

-- | The Cost Category\'s effective start date.
updateCostCategoryDefinitionResponse_effectiveStart :: Lens.Lens' UpdateCostCategoryDefinitionResponse (Core.Maybe Core.Text)
updateCostCategoryDefinitionResponse_effectiveStart = Lens.lens (\UpdateCostCategoryDefinitionResponse' {effectiveStart} -> effectiveStart) (\s@UpdateCostCategoryDefinitionResponse' {} a -> s {effectiveStart = a} :: UpdateCostCategoryDefinitionResponse)

-- | The response's http status code.
updateCostCategoryDefinitionResponse_httpStatus :: Lens.Lens' UpdateCostCategoryDefinitionResponse Core.Int
updateCostCategoryDefinitionResponse_httpStatus = Lens.lens (\UpdateCostCategoryDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateCostCategoryDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateCostCategoryDefinitionResponse)

instance
  Core.NFData
    UpdateCostCategoryDefinitionResponse
