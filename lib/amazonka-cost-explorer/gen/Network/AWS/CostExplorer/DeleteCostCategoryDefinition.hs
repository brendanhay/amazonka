{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.DeleteCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Cost Category. Expenses from this month going forward will no longer be categorized with this Cost Category.
module Network.AWS.CostExplorer.DeleteCostCategoryDefinition
  ( -- * Creating a request
    DeleteCostCategoryDefinition (..),
    mkDeleteCostCategoryDefinition,

    -- ** Request lenses
    dccdCostCategoryARN,

    -- * Destructuring the response
    DeleteCostCategoryDefinitionResponse (..),
    mkDeleteCostCategoryDefinitionResponse,

    -- ** Response lenses
    dccdrsCostCategoryARN,
    dccdrsEffectiveEnd,
    dccdrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCostCategoryDefinition' smart constructor.
newtype DeleteCostCategoryDefinition = DeleteCostCategoryDefinition'
  { -- | The unique identifier for your Cost Category.
    costCategoryARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCostCategoryDefinition' with the minimum fields required to make a request.
--
-- * 'costCategoryARN' - The unique identifier for your Cost Category.
mkDeleteCostCategoryDefinition ::
  -- | 'costCategoryARN'
  Lude.Text ->
  DeleteCostCategoryDefinition
mkDeleteCostCategoryDefinition pCostCategoryARN_ =
  DeleteCostCategoryDefinition'
    { costCategoryARN =
        pCostCategoryARN_
    }

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccdCostCategoryARN :: Lens.Lens' DeleteCostCategoryDefinition Lude.Text
dccdCostCategoryARN = Lens.lens (costCategoryARN :: DeleteCostCategoryDefinition -> Lude.Text) (\s a -> s {costCategoryARN = a} :: DeleteCostCategoryDefinition)
{-# DEPRECATED dccdCostCategoryARN "Use generic-lens or generic-optics with 'costCategoryARN' instead." #-}

instance Lude.AWSRequest DeleteCostCategoryDefinition where
  type
    Rs DeleteCostCategoryDefinition =
      DeleteCostCategoryDefinitionResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteCostCategoryDefinitionResponse'
            Lude.<$> (x Lude..?> "CostCategoryArn")
            Lude.<*> (x Lude..?> "EffectiveEnd")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCostCategoryDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.DeleteCostCategoryDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCostCategoryDefinition where
  toJSON DeleteCostCategoryDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CostCategoryArn" Lude..= costCategoryARN)]
      )

instance Lude.ToPath DeleteCostCategoryDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCostCategoryDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCostCategoryDefinitionResponse' smart constructor.
data DeleteCostCategoryDefinitionResponse = DeleteCostCategoryDefinitionResponse'
  { -- | The unique identifier for your Cost Category.
    costCategoryARN :: Lude.Maybe Lude.Text,
    -- | The effective end date of the Cost Category as a result of deleting it. No costs after this date will be categorized by the deleted Cost Category.
    effectiveEnd :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCostCategoryDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'costCategoryARN' - The unique identifier for your Cost Category.
-- * 'effectiveEnd' - The effective end date of the Cost Category as a result of deleting it. No costs after this date will be categorized by the deleted Cost Category.
-- * 'responseStatus' - The response status code.
mkDeleteCostCategoryDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCostCategoryDefinitionResponse
mkDeleteCostCategoryDefinitionResponse pResponseStatus_ =
  DeleteCostCategoryDefinitionResponse'
    { costCategoryARN =
        Lude.Nothing,
      effectiveEnd = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccdrsCostCategoryARN :: Lens.Lens' DeleteCostCategoryDefinitionResponse (Lude.Maybe Lude.Text)
dccdrsCostCategoryARN = Lens.lens (costCategoryARN :: DeleteCostCategoryDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {costCategoryARN = a} :: DeleteCostCategoryDefinitionResponse)
{-# DEPRECATED dccdrsCostCategoryARN "Use generic-lens or generic-optics with 'costCategoryARN' instead." #-}

-- | The effective end date of the Cost Category as a result of deleting it. No costs after this date will be categorized by the deleted Cost Category.
--
-- /Note:/ Consider using 'effectiveEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccdrsEffectiveEnd :: Lens.Lens' DeleteCostCategoryDefinitionResponse (Lude.Maybe Lude.Text)
dccdrsEffectiveEnd = Lens.lens (effectiveEnd :: DeleteCostCategoryDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {effectiveEnd = a} :: DeleteCostCategoryDefinitionResponse)
{-# DEPRECATED dccdrsEffectiveEnd "Use generic-lens or generic-optics with 'effectiveEnd' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccdrsResponseStatus :: Lens.Lens' DeleteCostCategoryDefinitionResponse Lude.Int
dccdrsResponseStatus = Lens.lens (responseStatus :: DeleteCostCategoryDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCostCategoryDefinitionResponse)
{-# DEPRECATED dccdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
