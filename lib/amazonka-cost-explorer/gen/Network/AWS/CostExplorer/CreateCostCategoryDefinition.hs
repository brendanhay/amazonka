{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.CreateCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Cost Category with the requested name and rules.
module Network.AWS.CostExplorer.CreateCostCategoryDefinition
  ( -- * Creating a request
    CreateCostCategoryDefinition (..),
    mkCreateCostCategoryDefinition,

    -- ** Request lenses
    cccdName,
    cccdRuleVersion,
    cccdRules,

    -- * Destructuring the response
    CreateCostCategoryDefinitionResponse (..),
    mkCreateCostCategoryDefinitionResponse,

    -- ** Response lenses
    cccdrsEffectiveStart,
    cccdrsCostCategoryARN,
    cccdrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCostCategoryDefinition' smart constructor.
data CreateCostCategoryDefinition = CreateCostCategoryDefinition'
  { name ::
      Lude.Text,
    ruleVersion ::
      CostCategoryRuleVersion,
    rules ::
      Lude.NonEmpty CostCategoryRule
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCostCategoryDefinition' with the minimum fields required to make a request.
--
-- * 'name' - Undocumented field.
-- * 'ruleVersion' - Undocumented field.
-- * 'rules' - The Cost Category rules used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule> .
mkCreateCostCategoryDefinition ::
  -- | 'name'
  Lude.Text ->
  -- | 'ruleVersion'
  CostCategoryRuleVersion ->
  -- | 'rules'
  Lude.NonEmpty CostCategoryRule ->
  CreateCostCategoryDefinition
mkCreateCostCategoryDefinition pName_ pRuleVersion_ pRules_ =
  CreateCostCategoryDefinition'
    { name = pName_,
      ruleVersion = pRuleVersion_,
      rules = pRules_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdName :: Lens.Lens' CreateCostCategoryDefinition Lude.Text
cccdName = Lens.lens (name :: CreateCostCategoryDefinition -> Lude.Text) (\s a -> s {name = a} :: CreateCostCategoryDefinition)
{-# DEPRECATED cccdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ruleVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdRuleVersion :: Lens.Lens' CreateCostCategoryDefinition CostCategoryRuleVersion
cccdRuleVersion = Lens.lens (ruleVersion :: CreateCostCategoryDefinition -> CostCategoryRuleVersion) (\s a -> s {ruleVersion = a} :: CreateCostCategoryDefinition)
{-# DEPRECATED cccdRuleVersion "Use generic-lens or generic-optics with 'ruleVersion' instead." #-}

-- | The Cost Category rules used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule> .
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdRules :: Lens.Lens' CreateCostCategoryDefinition (Lude.NonEmpty CostCategoryRule)
cccdRules = Lens.lens (rules :: CreateCostCategoryDefinition -> Lude.NonEmpty CostCategoryRule) (\s a -> s {rules = a} :: CreateCostCategoryDefinition)
{-# DEPRECATED cccdRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Lude.AWSRequest CreateCostCategoryDefinition where
  type
    Rs CreateCostCategoryDefinition =
      CreateCostCategoryDefinitionResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCostCategoryDefinitionResponse'
            Lude.<$> (x Lude..?> "EffectiveStart")
            Lude.<*> (x Lude..?> "CostCategoryArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCostCategoryDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.CreateCostCategoryDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCostCategoryDefinition where
  toJSON CreateCostCategoryDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("RuleVersion" Lude..= ruleVersion),
            Lude.Just ("Rules" Lude..= rules)
          ]
      )

instance Lude.ToPath CreateCostCategoryDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCostCategoryDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCostCategoryDefinitionResponse' smart constructor.
data CreateCostCategoryDefinitionResponse = CreateCostCategoryDefinitionResponse'
  { effectiveStart ::
      Lude.Maybe
        Lude.Text,
    costCategoryARN ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCostCategoryDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'costCategoryARN' - The unique identifier for your newly created Cost Category.
-- * 'effectiveStart' - The Cost Category's effective start date.
-- * 'responseStatus' - The response status code.
mkCreateCostCategoryDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCostCategoryDefinitionResponse
mkCreateCostCategoryDefinitionResponse pResponseStatus_ =
  CreateCostCategoryDefinitionResponse'
    { effectiveStart =
        Lude.Nothing,
      costCategoryARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Cost Category's effective start date.
--
-- /Note:/ Consider using 'effectiveStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdrsEffectiveStart :: Lens.Lens' CreateCostCategoryDefinitionResponse (Lude.Maybe Lude.Text)
cccdrsEffectiveStart = Lens.lens (effectiveStart :: CreateCostCategoryDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {effectiveStart = a} :: CreateCostCategoryDefinitionResponse)
{-# DEPRECATED cccdrsEffectiveStart "Use generic-lens or generic-optics with 'effectiveStart' instead." #-}

-- | The unique identifier for your newly created Cost Category.
--
-- /Note:/ Consider using 'costCategoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdrsCostCategoryARN :: Lens.Lens' CreateCostCategoryDefinitionResponse (Lude.Maybe Lude.Text)
cccdrsCostCategoryARN = Lens.lens (costCategoryARN :: CreateCostCategoryDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {costCategoryARN = a} :: CreateCostCategoryDefinitionResponse)
{-# DEPRECATED cccdrsCostCategoryARN "Use generic-lens or generic-optics with 'costCategoryARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdrsResponseStatus :: Lens.Lens' CreateCostCategoryDefinitionResponse Lude.Int
cccdrsResponseStatus = Lens.lens (responseStatus :: CreateCostCategoryDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCostCategoryDefinitionResponse)
{-# DEPRECATED cccdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
