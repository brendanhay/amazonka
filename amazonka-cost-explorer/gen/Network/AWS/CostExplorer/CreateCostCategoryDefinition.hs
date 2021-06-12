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
-- Module      : Network.AWS.CostExplorer.CreateCostCategoryDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Cost Category with the requested name and rules.
module Network.AWS.CostExplorer.CreateCostCategoryDefinition
  ( -- * Creating a Request
    CreateCostCategoryDefinition (..),
    newCreateCostCategoryDefinition,

    -- * Request Lenses
    createCostCategoryDefinition_name,
    createCostCategoryDefinition_ruleVersion,
    createCostCategoryDefinition_rules,

    -- * Destructuring the Response
    CreateCostCategoryDefinitionResponse (..),
    newCreateCostCategoryDefinitionResponse,

    -- * Response Lenses
    createCostCategoryDefinitionResponse_costCategoryArn,
    createCostCategoryDefinitionResponse_effectiveStart,
    createCostCategoryDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCostCategoryDefinition' smart constructor.
data CreateCostCategoryDefinition = CreateCostCategoryDefinition'
  { name :: Core.Text,
    ruleVersion :: CostCategoryRuleVersion,
    -- | The Cost Category rules used to categorize costs. For more information,
    -- see
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule>.
    rules :: Core.NonEmpty CostCategoryRule
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCostCategoryDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createCostCategoryDefinition_name' - Undocumented member.
--
-- 'ruleVersion', 'createCostCategoryDefinition_ruleVersion' - Undocumented member.
--
-- 'rules', 'createCostCategoryDefinition_rules' - The Cost Category rules used to categorize costs. For more information,
-- see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule>.
newCreateCostCategoryDefinition ::
  -- | 'name'
  Core.Text ->
  -- | 'ruleVersion'
  CostCategoryRuleVersion ->
  -- | 'rules'
  Core.NonEmpty CostCategoryRule ->
  CreateCostCategoryDefinition
newCreateCostCategoryDefinition
  pName_
  pRuleVersion_
  pRules_ =
    CreateCostCategoryDefinition'
      { name = pName_,
        ruleVersion = pRuleVersion_,
        rules = Lens._Coerce Lens.# pRules_
      }

-- | Undocumented member.
createCostCategoryDefinition_name :: Lens.Lens' CreateCostCategoryDefinition Core.Text
createCostCategoryDefinition_name = Lens.lens (\CreateCostCategoryDefinition' {name} -> name) (\s@CreateCostCategoryDefinition' {} a -> s {name = a} :: CreateCostCategoryDefinition)

-- | Undocumented member.
createCostCategoryDefinition_ruleVersion :: Lens.Lens' CreateCostCategoryDefinition CostCategoryRuleVersion
createCostCategoryDefinition_ruleVersion = Lens.lens (\CreateCostCategoryDefinition' {ruleVersion} -> ruleVersion) (\s@CreateCostCategoryDefinition' {} a -> s {ruleVersion = a} :: CreateCostCategoryDefinition)

-- | The Cost Category rules used to categorize costs. For more information,
-- see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule>.
createCostCategoryDefinition_rules :: Lens.Lens' CreateCostCategoryDefinition (Core.NonEmpty CostCategoryRule)
createCostCategoryDefinition_rules = Lens.lens (\CreateCostCategoryDefinition' {rules} -> rules) (\s@CreateCostCategoryDefinition' {} a -> s {rules = a} :: CreateCostCategoryDefinition) Core.. Lens._Coerce

instance Core.AWSRequest CreateCostCategoryDefinition where
  type
    AWSResponse CreateCostCategoryDefinition =
      CreateCostCategoryDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCostCategoryDefinitionResponse'
            Core.<$> (x Core..?> "CostCategoryArn")
            Core.<*> (x Core..?> "EffectiveStart")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCostCategoryDefinition

instance Core.NFData CreateCostCategoryDefinition

instance Core.ToHeaders CreateCostCategoryDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.CreateCostCategoryDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCostCategoryDefinition where
  toJSON CreateCostCategoryDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RuleVersion" Core..= ruleVersion),
            Core.Just ("Rules" Core..= rules)
          ]
      )

instance Core.ToPath CreateCostCategoryDefinition where
  toPath = Core.const "/"

instance Core.ToQuery CreateCostCategoryDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCostCategoryDefinitionResponse' smart constructor.
data CreateCostCategoryDefinitionResponse = CreateCostCategoryDefinitionResponse'
  { -- | The unique identifier for your newly created Cost Category.
    costCategoryArn :: Core.Maybe Core.Text,
    -- | The Cost Category\'s effective start date.
    effectiveStart :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCostCategoryDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategoryArn', 'createCostCategoryDefinitionResponse_costCategoryArn' - The unique identifier for your newly created Cost Category.
--
-- 'effectiveStart', 'createCostCategoryDefinitionResponse_effectiveStart' - The Cost Category\'s effective start date.
--
-- 'httpStatus', 'createCostCategoryDefinitionResponse_httpStatus' - The response's http status code.
newCreateCostCategoryDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCostCategoryDefinitionResponse
newCreateCostCategoryDefinitionResponse pHttpStatus_ =
  CreateCostCategoryDefinitionResponse'
    { costCategoryArn =
        Core.Nothing,
      effectiveStart = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for your newly created Cost Category.
createCostCategoryDefinitionResponse_costCategoryArn :: Lens.Lens' CreateCostCategoryDefinitionResponse (Core.Maybe Core.Text)
createCostCategoryDefinitionResponse_costCategoryArn = Lens.lens (\CreateCostCategoryDefinitionResponse' {costCategoryArn} -> costCategoryArn) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {costCategoryArn = a} :: CreateCostCategoryDefinitionResponse)

-- | The Cost Category\'s effective start date.
createCostCategoryDefinitionResponse_effectiveStart :: Lens.Lens' CreateCostCategoryDefinitionResponse (Core.Maybe Core.Text)
createCostCategoryDefinitionResponse_effectiveStart = Lens.lens (\CreateCostCategoryDefinitionResponse' {effectiveStart} -> effectiveStart) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {effectiveStart = a} :: CreateCostCategoryDefinitionResponse)

-- | The response's http status code.
createCostCategoryDefinitionResponse_httpStatus :: Lens.Lens' CreateCostCategoryDefinitionResponse Core.Int
createCostCategoryDefinitionResponse_httpStatus = Lens.lens (\CreateCostCategoryDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {httpStatus = a} :: CreateCostCategoryDefinitionResponse)

instance
  Core.NFData
    CreateCostCategoryDefinitionResponse
