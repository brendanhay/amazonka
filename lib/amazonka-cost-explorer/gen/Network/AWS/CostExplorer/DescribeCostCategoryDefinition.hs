{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.DescribeCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name, ARN, rules, definition, and effective dates of a Cost Category that's defined in the account.
--
-- You have the option to use @EffectiveOn@ to return a Cost Category that is active on a specific date. If there is no @EffectiveOn@ specified, you’ll see a Cost Category that is effective on the current date. If Cost Category is still effective, @EffectiveEnd@ is omitted in the response.
module Network.AWS.CostExplorer.DescribeCostCategoryDefinition
  ( -- * Creating a request
    DescribeCostCategoryDefinition (..),
    mkDescribeCostCategoryDefinition,

    -- ** Request lenses
    dEffectiveOn,
    dCostCategoryARN,

    -- * Destructuring the response
    DescribeCostCategoryDefinitionResponse (..),
    mkDescribeCostCategoryDefinitionResponse,

    -- ** Response lenses
    drsCostCategory,
    drsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCostCategoryDefinition' smart constructor.
data DescribeCostCategoryDefinition = DescribeCostCategoryDefinition'
  { effectiveOn ::
      Lude.Maybe Lude.Text,
    costCategoryARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCostCategoryDefinition' with the minimum fields required to make a request.
--
-- * 'costCategoryARN' - The unique identifier for your Cost Category.
-- * 'effectiveOn' - The date when the Cost Category was effective.
mkDescribeCostCategoryDefinition ::
  -- | 'costCategoryARN'
  Lude.Text ->
  DescribeCostCategoryDefinition
mkDescribeCostCategoryDefinition pCostCategoryARN_ =
  DescribeCostCategoryDefinition'
    { effectiveOn = Lude.Nothing,
      costCategoryARN = pCostCategoryARN_
    }

-- | The date when the Cost Category was effective.
--
-- /Note:/ Consider using 'effectiveOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEffectiveOn :: Lens.Lens' DescribeCostCategoryDefinition (Lude.Maybe Lude.Text)
dEffectiveOn = Lens.lens (effectiveOn :: DescribeCostCategoryDefinition -> Lude.Maybe Lude.Text) (\s a -> s {effectiveOn = a} :: DescribeCostCategoryDefinition)
{-# DEPRECATED dEffectiveOn "Use generic-lens or generic-optics with 'effectiveOn' instead." #-}

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCostCategoryARN :: Lens.Lens' DescribeCostCategoryDefinition Lude.Text
dCostCategoryARN = Lens.lens (costCategoryARN :: DescribeCostCategoryDefinition -> Lude.Text) (\s a -> s {costCategoryARN = a} :: DescribeCostCategoryDefinition)
{-# DEPRECATED dCostCategoryARN "Use generic-lens or generic-optics with 'costCategoryARN' instead." #-}

instance Lude.AWSRequest DescribeCostCategoryDefinition where
  type
    Rs DescribeCostCategoryDefinition =
      DescribeCostCategoryDefinitionResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCostCategoryDefinitionResponse'
            Lude.<$> (x Lude..?> "CostCategory") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCostCategoryDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.DescribeCostCategoryDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCostCategoryDefinition where
  toJSON DescribeCostCategoryDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EffectiveOn" Lude..=) Lude.<$> effectiveOn,
            Lude.Just ("CostCategoryArn" Lude..= costCategoryARN)
          ]
      )

instance Lude.ToPath DescribeCostCategoryDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCostCategoryDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCostCategoryDefinitionResponse' smart constructor.
data DescribeCostCategoryDefinitionResponse = DescribeCostCategoryDefinitionResponse'
  { costCategory ::
      Lude.Maybe
        CostCategory,
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

-- | Creates a value of 'DescribeCostCategoryDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'costCategory' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeCostCategoryDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCostCategoryDefinitionResponse
mkDescribeCostCategoryDefinitionResponse pResponseStatus_ =
  DescribeCostCategoryDefinitionResponse'
    { costCategory =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'costCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCostCategory :: Lens.Lens' DescribeCostCategoryDefinitionResponse (Lude.Maybe CostCategory)
drsCostCategory = Lens.lens (costCategory :: DescribeCostCategoryDefinitionResponse -> Lude.Maybe CostCategory) (\s a -> s {costCategory = a} :: DescribeCostCategoryDefinitionResponse)
{-# DEPRECATED drsCostCategory "Use generic-lens or generic-optics with 'costCategory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCostCategoryDefinitionResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeCostCategoryDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCostCategoryDefinitionResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
