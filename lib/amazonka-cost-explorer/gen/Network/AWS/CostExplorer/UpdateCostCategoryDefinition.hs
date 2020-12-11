{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Cost Category. Changes made to the Cost Category rules will be used to categorize the current month’s expenses and future expenses. This won’t change categorization for the previous months.
module Network.AWS.CostExplorer.UpdateCostCategoryDefinition
  ( -- * Creating a request
    UpdateCostCategoryDefinition (..),
    mkUpdateCostCategoryDefinition,

    -- ** Request lenses
    uccdCostCategoryARN,
    uccdRuleVersion,
    uccdRules,

    -- * Destructuring the response
    UpdateCostCategoryDefinitionResponse (..),
    mkUpdateCostCategoryDefinitionResponse,

    -- ** Response lenses
    uccdrsEffectiveStart,
    uccdrsCostCategoryARN,
    uccdrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCostCategoryDefinition' smart constructor.
data UpdateCostCategoryDefinition = UpdateCostCategoryDefinition'
  { costCategoryARN ::
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

-- | Creates a value of 'UpdateCostCategoryDefinition' with the minimum fields required to make a request.
--
-- * 'costCategoryARN' - The unique identifier for your Cost Category.
-- * 'ruleVersion' - Undocumented field.
-- * 'rules' - The @Expression@ object used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule > .
mkUpdateCostCategoryDefinition ::
  -- | 'costCategoryARN'
  Lude.Text ->
  -- | 'ruleVersion'
  CostCategoryRuleVersion ->
  -- | 'rules'
  Lude.NonEmpty CostCategoryRule ->
  UpdateCostCategoryDefinition
mkUpdateCostCategoryDefinition
  pCostCategoryARN_
  pRuleVersion_
  pRules_ =
    UpdateCostCategoryDefinition'
      { costCategoryARN =
          pCostCategoryARN_,
        ruleVersion = pRuleVersion_,
        rules = pRules_
      }

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdCostCategoryARN :: Lens.Lens' UpdateCostCategoryDefinition Lude.Text
uccdCostCategoryARN = Lens.lens (costCategoryARN :: UpdateCostCategoryDefinition -> Lude.Text) (\s a -> s {costCategoryARN = a} :: UpdateCostCategoryDefinition)
{-# DEPRECATED uccdCostCategoryARN "Use generic-lens or generic-optics with 'costCategoryARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ruleVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdRuleVersion :: Lens.Lens' UpdateCostCategoryDefinition CostCategoryRuleVersion
uccdRuleVersion = Lens.lens (ruleVersion :: UpdateCostCategoryDefinition -> CostCategoryRuleVersion) (\s a -> s {ruleVersion = a} :: UpdateCostCategoryDefinition)
{-# DEPRECATED uccdRuleVersion "Use generic-lens or generic-optics with 'ruleVersion' instead." #-}

-- | The @Expression@ object used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule > .
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdRules :: Lens.Lens' UpdateCostCategoryDefinition (Lude.NonEmpty CostCategoryRule)
uccdRules = Lens.lens (rules :: UpdateCostCategoryDefinition -> Lude.NonEmpty CostCategoryRule) (\s a -> s {rules = a} :: UpdateCostCategoryDefinition)
{-# DEPRECATED uccdRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Lude.AWSRequest UpdateCostCategoryDefinition where
  type
    Rs UpdateCostCategoryDefinition =
      UpdateCostCategoryDefinitionResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateCostCategoryDefinitionResponse'
            Lude.<$> (x Lude..?> "EffectiveStart")
            Lude.<*> (x Lude..?> "CostCategoryArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCostCategoryDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.UpdateCostCategoryDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCostCategoryDefinition where
  toJSON UpdateCostCategoryDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CostCategoryArn" Lude..= costCategoryARN),
            Lude.Just ("RuleVersion" Lude..= ruleVersion),
            Lude.Just ("Rules" Lude..= rules)
          ]
      )

instance Lude.ToPath UpdateCostCategoryDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCostCategoryDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCostCategoryDefinitionResponse' smart constructor.
data UpdateCostCategoryDefinitionResponse = UpdateCostCategoryDefinitionResponse'
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

-- | Creates a value of 'UpdateCostCategoryDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'costCategoryARN' - The unique identifier for your Cost Category.
-- * 'effectiveStart' - The Cost Category's effective start date.
-- * 'responseStatus' - The response status code.
mkUpdateCostCategoryDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCostCategoryDefinitionResponse
mkUpdateCostCategoryDefinitionResponse pResponseStatus_ =
  UpdateCostCategoryDefinitionResponse'
    { effectiveStart =
        Lude.Nothing,
      costCategoryARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Cost Category's effective start date.
--
-- /Note:/ Consider using 'effectiveStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdrsEffectiveStart :: Lens.Lens' UpdateCostCategoryDefinitionResponse (Lude.Maybe Lude.Text)
uccdrsEffectiveStart = Lens.lens (effectiveStart :: UpdateCostCategoryDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {effectiveStart = a} :: UpdateCostCategoryDefinitionResponse)
{-# DEPRECATED uccdrsEffectiveStart "Use generic-lens or generic-optics with 'effectiveStart' instead." #-}

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdrsCostCategoryARN :: Lens.Lens' UpdateCostCategoryDefinitionResponse (Lude.Maybe Lude.Text)
uccdrsCostCategoryARN = Lens.lens (costCategoryARN :: UpdateCostCategoryDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {costCategoryARN = a} :: UpdateCostCategoryDefinitionResponse)
{-# DEPRECATED uccdrsCostCategoryARN "Use generic-lens or generic-optics with 'costCategoryARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdrsResponseStatus :: Lens.Lens' UpdateCostCategoryDefinitionResponse Lude.Int
uccdrsResponseStatus = Lens.lens (responseStatus :: UpdateCostCategoryDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCostCategoryDefinitionResponse)
{-# DEPRECATED uccdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
