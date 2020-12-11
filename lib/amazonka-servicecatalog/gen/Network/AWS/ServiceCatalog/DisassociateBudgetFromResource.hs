{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified budget from the specified resource.
module Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
  ( -- * Creating a request
    DisassociateBudgetFromResource (..),
    mkDisassociateBudgetFromResource,

    -- ** Request lenses
    dbfrBudgetName,
    dbfrResourceId,

    -- * Destructuring the response
    DisassociateBudgetFromResourceResponse (..),
    mkDisassociateBudgetFromResourceResponse,

    -- ** Response lenses
    dbfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDisassociateBudgetFromResource' smart constructor.
data DisassociateBudgetFromResource = DisassociateBudgetFromResource'
  { budgetName ::
      Lude.Text,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateBudgetFromResource' with the minimum fields required to make a request.
--
-- * 'budgetName' - The name of the budget you want to disassociate.
-- * 'resourceId' - The resource identifier you want to disassociate from. Either a portfolio-id or a product-id.
mkDisassociateBudgetFromResource ::
  -- | 'budgetName'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  DisassociateBudgetFromResource
mkDisassociateBudgetFromResource pBudgetName_ pResourceId_ =
  DisassociateBudgetFromResource'
    { budgetName = pBudgetName_,
      resourceId = pResourceId_
    }

-- | The name of the budget you want to disassociate.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrBudgetName :: Lens.Lens' DisassociateBudgetFromResource Lude.Text
dbfrBudgetName = Lens.lens (budgetName :: DisassociateBudgetFromResource -> Lude.Text) (\s a -> s {budgetName = a} :: DisassociateBudgetFromResource)
{-# DEPRECATED dbfrBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The resource identifier you want to disassociate from. Either a portfolio-id or a product-id.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrResourceId :: Lens.Lens' DisassociateBudgetFromResource Lude.Text
dbfrResourceId = Lens.lens (resourceId :: DisassociateBudgetFromResource -> Lude.Text) (\s a -> s {resourceId = a} :: DisassociateBudgetFromResource)
{-# DEPRECATED dbfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest DisassociateBudgetFromResource where
  type
    Rs DisassociateBudgetFromResource =
      DisassociateBudgetFromResourceResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateBudgetFromResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateBudgetFromResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DisassociateBudgetFromResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateBudgetFromResource where
  toJSON DisassociateBudgetFromResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath DisassociateBudgetFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateBudgetFromResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateBudgetFromResourceResponse' smart constructor.
newtype DisassociateBudgetFromResourceResponse = DisassociateBudgetFromResourceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateBudgetFromResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateBudgetFromResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateBudgetFromResourceResponse
mkDisassociateBudgetFromResourceResponse pResponseStatus_ =
  DisassociateBudgetFromResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrrsResponseStatus :: Lens.Lens' DisassociateBudgetFromResourceResponse Lude.Int
dbfrrsResponseStatus = Lens.lens (responseStatus :: DisassociateBudgetFromResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateBudgetFromResourceResponse)
{-# DEPRECATED dbfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
