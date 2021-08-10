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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCostCategoryDefinition' smart constructor.
data CreateCostCategoryDefinition = CreateCostCategoryDefinition'
  { name :: Prelude.Text,
    ruleVersion :: CostCategoryRuleVersion,
    -- | The Cost Category rules used to categorize costs. For more information,
    -- see
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule>.
    rules :: Prelude.NonEmpty CostCategoryRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'ruleVersion'
  CostCategoryRuleVersion ->
  -- | 'rules'
  Prelude.NonEmpty CostCategoryRule ->
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
createCostCategoryDefinition_name :: Lens.Lens' CreateCostCategoryDefinition Prelude.Text
createCostCategoryDefinition_name = Lens.lens (\CreateCostCategoryDefinition' {name} -> name) (\s@CreateCostCategoryDefinition' {} a -> s {name = a} :: CreateCostCategoryDefinition)

-- | Undocumented member.
createCostCategoryDefinition_ruleVersion :: Lens.Lens' CreateCostCategoryDefinition CostCategoryRuleVersion
createCostCategoryDefinition_ruleVersion = Lens.lens (\CreateCostCategoryDefinition' {ruleVersion} -> ruleVersion) (\s@CreateCostCategoryDefinition' {} a -> s {ruleVersion = a} :: CreateCostCategoryDefinition)

-- | The Cost Category rules used to categorize costs. For more information,
-- see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule>.
createCostCategoryDefinition_rules :: Lens.Lens' CreateCostCategoryDefinition (Prelude.NonEmpty CostCategoryRule)
createCostCategoryDefinition_rules = Lens.lens (\CreateCostCategoryDefinition' {rules} -> rules) (\s@CreateCostCategoryDefinition' {} a -> s {rules = a} :: CreateCostCategoryDefinition) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateCostCategoryDefinition where
  type
    AWSResponse CreateCostCategoryDefinition =
      CreateCostCategoryDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCostCategoryDefinitionResponse'
            Prelude.<$> (x Core..?> "CostCategoryArn")
            Prelude.<*> (x Core..?> "EffectiveStart")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCostCategoryDefinition

instance Prelude.NFData CreateCostCategoryDefinition

instance Core.ToHeaders CreateCostCategoryDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.CreateCostCategoryDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCostCategoryDefinition where
  toJSON CreateCostCategoryDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("RuleVersion" Core..= ruleVersion),
            Prelude.Just ("Rules" Core..= rules)
          ]
      )

instance Core.ToPath CreateCostCategoryDefinition where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCostCategoryDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCostCategoryDefinitionResponse' smart constructor.
data CreateCostCategoryDefinitionResponse = CreateCostCategoryDefinitionResponse'
  { -- | The unique identifier for your newly created Cost Category.
    costCategoryArn :: Prelude.Maybe Prelude.Text,
    -- | The Cost Category\'s effective start date.
    effectiveStart :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateCostCategoryDefinitionResponse
newCreateCostCategoryDefinitionResponse pHttpStatus_ =
  CreateCostCategoryDefinitionResponse'
    { costCategoryArn =
        Prelude.Nothing,
      effectiveStart = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for your newly created Cost Category.
createCostCategoryDefinitionResponse_costCategoryArn :: Lens.Lens' CreateCostCategoryDefinitionResponse (Prelude.Maybe Prelude.Text)
createCostCategoryDefinitionResponse_costCategoryArn = Lens.lens (\CreateCostCategoryDefinitionResponse' {costCategoryArn} -> costCategoryArn) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {costCategoryArn = a} :: CreateCostCategoryDefinitionResponse)

-- | The Cost Category\'s effective start date.
createCostCategoryDefinitionResponse_effectiveStart :: Lens.Lens' CreateCostCategoryDefinitionResponse (Prelude.Maybe Prelude.Text)
createCostCategoryDefinitionResponse_effectiveStart = Lens.lens (\CreateCostCategoryDefinitionResponse' {effectiveStart} -> effectiveStart) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {effectiveStart = a} :: CreateCostCategoryDefinitionResponse)

-- | The response's http status code.
createCostCategoryDefinitionResponse_httpStatus :: Lens.Lens' CreateCostCategoryDefinitionResponse Prelude.Int
createCostCategoryDefinitionResponse_httpStatus = Lens.lens (\CreateCostCategoryDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {httpStatus = a} :: CreateCostCategoryDefinitionResponse)

instance
  Prelude.NFData
    CreateCostCategoryDefinitionResponse
