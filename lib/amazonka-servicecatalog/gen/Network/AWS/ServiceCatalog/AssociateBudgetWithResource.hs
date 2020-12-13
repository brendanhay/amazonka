{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateBudgetWithResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified budget with the specified resource.
module Network.AWS.ServiceCatalog.AssociateBudgetWithResource
  ( -- * Creating a request
    AssociateBudgetWithResource (..),
    mkAssociateBudgetWithResource,

    -- ** Request lenses
    abwrResourceId,
    abwrBudgetName,

    -- * Destructuring the response
    AssociateBudgetWithResourceResponse (..),
    mkAssociateBudgetWithResourceResponse,

    -- ** Response lenses
    abwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkAssociateBudgetWithResource' smart constructor.
data AssociateBudgetWithResource = AssociateBudgetWithResource'
  { -- | The resource identifier. Either a portfolio-id or a product-id.
    resourceId :: Lude.Text,
    -- | The name of the budget you want to associate.
    budgetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateBudgetWithResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource identifier. Either a portfolio-id or a product-id.
-- * 'budgetName' - The name of the budget you want to associate.
mkAssociateBudgetWithResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  AssociateBudgetWithResource
mkAssociateBudgetWithResource pResourceId_ pBudgetName_ =
  AssociateBudgetWithResource'
    { resourceId = pResourceId_,
      budgetName = pBudgetName_
    }

-- | The resource identifier. Either a portfolio-id or a product-id.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrResourceId :: Lens.Lens' AssociateBudgetWithResource Lude.Text
abwrResourceId = Lens.lens (resourceId :: AssociateBudgetWithResource -> Lude.Text) (\s a -> s {resourceId = a} :: AssociateBudgetWithResource)
{-# DEPRECATED abwrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The name of the budget you want to associate.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrBudgetName :: Lens.Lens' AssociateBudgetWithResource Lude.Text
abwrBudgetName = Lens.lens (budgetName :: AssociateBudgetWithResource -> Lude.Text) (\s a -> s {budgetName = a} :: AssociateBudgetWithResource)
{-# DEPRECATED abwrBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Lude.AWSRequest AssociateBudgetWithResource where
  type
    Rs AssociateBudgetWithResource =
      AssociateBudgetWithResourceResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateBudgetWithResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateBudgetWithResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.AssociateBudgetWithResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateBudgetWithResource where
  toJSON AssociateBudgetWithResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath AssociateBudgetWithResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateBudgetWithResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateBudgetWithResourceResponse' smart constructor.
newtype AssociateBudgetWithResourceResponse = AssociateBudgetWithResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateBudgetWithResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateBudgetWithResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateBudgetWithResourceResponse
mkAssociateBudgetWithResourceResponse pResponseStatus_ =
  AssociateBudgetWithResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrrsResponseStatus :: Lens.Lens' AssociateBudgetWithResourceResponse Lude.Int
abwrrsResponseStatus = Lens.lens (responseStatus :: AssociateBudgetWithResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateBudgetWithResourceResponse)
{-# DEPRECATED abwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
