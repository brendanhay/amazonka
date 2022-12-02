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
-- Module      : Amazonka.CostExplorer.CreateCostCategoryDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Cost Category with the requested name and rules.
module Amazonka.CostExplorer.CreateCostCategoryDefinition
  ( -- * Creating a Request
    CreateCostCategoryDefinition (..),
    newCreateCostCategoryDefinition,

    -- * Request Lenses
    createCostCategoryDefinition_splitChargeRules,
    createCostCategoryDefinition_defaultValue,
    createCostCategoryDefinition_resourceTags,
    createCostCategoryDefinition_effectiveStart,
    createCostCategoryDefinition_name,
    createCostCategoryDefinition_ruleVersion,
    createCostCategoryDefinition_rules,

    -- * Destructuring the Response
    CreateCostCategoryDefinitionResponse (..),
    newCreateCostCategoryDefinitionResponse,

    -- * Response Lenses
    createCostCategoryDefinitionResponse_effectiveStart,
    createCostCategoryDefinitionResponse_costCategoryArn,
    createCostCategoryDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCostCategoryDefinition' smart constructor.
data CreateCostCategoryDefinition = CreateCostCategoryDefinition'
  { -- | The split charge rules used to allocate your charges between your Cost
    -- Category values.
    splitChargeRules :: Prelude.Maybe (Prelude.NonEmpty CostCategorySplitChargeRule),
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | An optional list of tags to associate with the specified
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategory.html CostCategory>
    -- . You can use resource tags to control access to your @cost category@
    -- using IAM policies.
    --
    -- Each tag consists of a key and a value, and each key must be unique for
    -- the resource. The following restrictions apply to resource tags:
    --
    -- -   Although the maximum number of array members is 200, you can assign
    --     a maximum of 50 user-tags to one resource. The remaining are
    --     reserved for Amazon Web Services use
    --
    -- -   The maximum length of a key is 128 characters
    --
    -- -   The maximum length of a value is 256 characters
    --
    -- -   Keys and values can only contain alphanumeric characters, spaces,
    --     and any of the following: @_.:\/=+\@-@
    --
    -- -   Keys and values are case sensitive
    --
    -- -   Keys and values are trimmed for any leading or trailing whitespaces
    --
    -- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
    --     for Amazon Web Services use
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | The Cost Category\'s effective start date. It can only be a billing
    -- start date (first day of the month). If the date isn\'t provided, it\'s
    -- the first day of the current month. Dates can\'t be before the previous
    -- twelve months, or in the future.
    effectiveStart :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Text,
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
-- 'splitChargeRules', 'createCostCategoryDefinition_splitChargeRules' - The split charge rules used to allocate your charges between your Cost
-- Category values.
--
-- 'defaultValue', 'createCostCategoryDefinition_defaultValue' - Undocumented member.
--
-- 'resourceTags', 'createCostCategoryDefinition_resourceTags' - An optional list of tags to associate with the specified
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategory.html CostCategory>
-- . You can use resource tags to control access to your @cost category@
-- using IAM policies.
--
-- Each tag consists of a key and a value, and each key must be unique for
-- the resource. The following restrictions apply to resource tags:
--
-- -   Although the maximum number of array members is 200, you can assign
--     a maximum of 50 user-tags to one resource. The remaining are
--     reserved for Amazon Web Services use
--
-- -   The maximum length of a key is 128 characters
--
-- -   The maximum length of a value is 256 characters
--
-- -   Keys and values can only contain alphanumeric characters, spaces,
--     and any of the following: @_.:\/=+\@-@
--
-- -   Keys and values are case sensitive
--
-- -   Keys and values are trimmed for any leading or trailing whitespaces
--
-- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
--     for Amazon Web Services use
--
-- 'effectiveStart', 'createCostCategoryDefinition_effectiveStart' - The Cost Category\'s effective start date. It can only be a billing
-- start date (first day of the month). If the date isn\'t provided, it\'s
-- the first day of the current month. Dates can\'t be before the previous
-- twelve months, or in the future.
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
      { splitChargeRules =
          Prelude.Nothing,
        defaultValue = Prelude.Nothing,
        resourceTags = Prelude.Nothing,
        effectiveStart = Prelude.Nothing,
        name = pName_,
        ruleVersion = pRuleVersion_,
        rules = Lens.coerced Lens.# pRules_
      }

-- | The split charge rules used to allocate your charges between your Cost
-- Category values.
createCostCategoryDefinition_splitChargeRules :: Lens.Lens' CreateCostCategoryDefinition (Prelude.Maybe (Prelude.NonEmpty CostCategorySplitChargeRule))
createCostCategoryDefinition_splitChargeRules = Lens.lens (\CreateCostCategoryDefinition' {splitChargeRules} -> splitChargeRules) (\s@CreateCostCategoryDefinition' {} a -> s {splitChargeRules = a} :: CreateCostCategoryDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createCostCategoryDefinition_defaultValue :: Lens.Lens' CreateCostCategoryDefinition (Prelude.Maybe Prelude.Text)
createCostCategoryDefinition_defaultValue = Lens.lens (\CreateCostCategoryDefinition' {defaultValue} -> defaultValue) (\s@CreateCostCategoryDefinition' {} a -> s {defaultValue = a} :: CreateCostCategoryDefinition)

-- | An optional list of tags to associate with the specified
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategory.html CostCategory>
-- . You can use resource tags to control access to your @cost category@
-- using IAM policies.
--
-- Each tag consists of a key and a value, and each key must be unique for
-- the resource. The following restrictions apply to resource tags:
--
-- -   Although the maximum number of array members is 200, you can assign
--     a maximum of 50 user-tags to one resource. The remaining are
--     reserved for Amazon Web Services use
--
-- -   The maximum length of a key is 128 characters
--
-- -   The maximum length of a value is 256 characters
--
-- -   Keys and values can only contain alphanumeric characters, spaces,
--     and any of the following: @_.:\/=+\@-@
--
-- -   Keys and values are case sensitive
--
-- -   Keys and values are trimmed for any leading or trailing whitespaces
--
-- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
--     for Amazon Web Services use
createCostCategoryDefinition_resourceTags :: Lens.Lens' CreateCostCategoryDefinition (Prelude.Maybe [ResourceTag])
createCostCategoryDefinition_resourceTags = Lens.lens (\CreateCostCategoryDefinition' {resourceTags} -> resourceTags) (\s@CreateCostCategoryDefinition' {} a -> s {resourceTags = a} :: CreateCostCategoryDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The Cost Category\'s effective start date. It can only be a billing
-- start date (first day of the month). If the date isn\'t provided, it\'s
-- the first day of the current month. Dates can\'t be before the previous
-- twelve months, or in the future.
createCostCategoryDefinition_effectiveStart :: Lens.Lens' CreateCostCategoryDefinition (Prelude.Maybe Prelude.Text)
createCostCategoryDefinition_effectiveStart = Lens.lens (\CreateCostCategoryDefinition' {effectiveStart} -> effectiveStart) (\s@CreateCostCategoryDefinition' {} a -> s {effectiveStart = a} :: CreateCostCategoryDefinition)

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
createCostCategoryDefinition_rules = Lens.lens (\CreateCostCategoryDefinition' {rules} -> rules) (\s@CreateCostCategoryDefinition' {} a -> s {rules = a} :: CreateCostCategoryDefinition) Prelude.. Lens.coerced

instance Core.AWSRequest CreateCostCategoryDefinition where
  type
    AWSResponse CreateCostCategoryDefinition =
      CreateCostCategoryDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCostCategoryDefinitionResponse'
            Prelude.<$> (x Data..?> "EffectiveStart")
            Prelude.<*> (x Data..?> "CostCategoryArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCostCategoryDefinition
  where
  hashWithSalt _salt CreateCostCategoryDefinition' {..} =
    _salt `Prelude.hashWithSalt` splitChargeRules
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` effectiveStart
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ruleVersion
      `Prelude.hashWithSalt` rules

instance Prelude.NFData CreateCostCategoryDefinition where
  rnf CreateCostCategoryDefinition' {..} =
    Prelude.rnf splitChargeRules
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf effectiveStart
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ruleVersion
      `Prelude.seq` Prelude.rnf rules

instance Data.ToHeaders CreateCostCategoryDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.CreateCostCategoryDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCostCategoryDefinition where
  toJSON CreateCostCategoryDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SplitChargeRules" Data..=)
              Prelude.<$> splitChargeRules,
            ("DefaultValue" Data..=) Prelude.<$> defaultValue,
            ("ResourceTags" Data..=) Prelude.<$> resourceTags,
            ("EffectiveStart" Data..=)
              Prelude.<$> effectiveStart,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RuleVersion" Data..= ruleVersion),
            Prelude.Just ("Rules" Data..= rules)
          ]
      )

instance Data.ToPath CreateCostCategoryDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCostCategoryDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCostCategoryDefinitionResponse' smart constructor.
data CreateCostCategoryDefinitionResponse = CreateCostCategoryDefinitionResponse'
  { -- | The Cost Category\'s effective start date. It can only be a billing
    -- start date (first day of the month).
    effectiveStart :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for your newly created Cost Category.
    costCategoryArn :: Prelude.Maybe Prelude.Text,
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
-- 'effectiveStart', 'createCostCategoryDefinitionResponse_effectiveStart' - The Cost Category\'s effective start date. It can only be a billing
-- start date (first day of the month).
--
-- 'costCategoryArn', 'createCostCategoryDefinitionResponse_costCategoryArn' - The unique identifier for your newly created Cost Category.
--
-- 'httpStatus', 'createCostCategoryDefinitionResponse_httpStatus' - The response's http status code.
newCreateCostCategoryDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCostCategoryDefinitionResponse
newCreateCostCategoryDefinitionResponse pHttpStatus_ =
  CreateCostCategoryDefinitionResponse'
    { effectiveStart =
        Prelude.Nothing,
      costCategoryArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Cost Category\'s effective start date. It can only be a billing
-- start date (first day of the month).
createCostCategoryDefinitionResponse_effectiveStart :: Lens.Lens' CreateCostCategoryDefinitionResponse (Prelude.Maybe Prelude.Text)
createCostCategoryDefinitionResponse_effectiveStart = Lens.lens (\CreateCostCategoryDefinitionResponse' {effectiveStart} -> effectiveStart) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {effectiveStart = a} :: CreateCostCategoryDefinitionResponse)

-- | The unique identifier for your newly created Cost Category.
createCostCategoryDefinitionResponse_costCategoryArn :: Lens.Lens' CreateCostCategoryDefinitionResponse (Prelude.Maybe Prelude.Text)
createCostCategoryDefinitionResponse_costCategoryArn = Lens.lens (\CreateCostCategoryDefinitionResponse' {costCategoryArn} -> costCategoryArn) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {costCategoryArn = a} :: CreateCostCategoryDefinitionResponse)

-- | The response's http status code.
createCostCategoryDefinitionResponse_httpStatus :: Lens.Lens' CreateCostCategoryDefinitionResponse Prelude.Int
createCostCategoryDefinitionResponse_httpStatus = Lens.lens (\CreateCostCategoryDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateCostCategoryDefinitionResponse' {} a -> s {httpStatus = a} :: CreateCostCategoryDefinitionResponse)

instance
  Prelude.NFData
    CreateCostCategoryDefinitionResponse
  where
  rnf CreateCostCategoryDefinitionResponse' {..} =
    Prelude.rnf effectiveStart
      `Prelude.seq` Prelude.rnf costCategoryArn
      `Prelude.seq` Prelude.rnf httpStatus
