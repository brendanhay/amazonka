{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate the specified TagOption with the specified portfolio or product.
module Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
  ( -- * Creating a request
    AssociateTagOptionWithResource (..),
    mkAssociateTagOptionWithResource,

    -- ** Request lenses
    atowrTagOptionId,
    atowrResourceId,

    -- * Destructuring the response
    AssociateTagOptionWithResourceResponse (..),
    mkAssociateTagOptionWithResourceResponse,

    -- ** Response lenses
    atowrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkAssociateTagOptionWithResource' smart constructor.
data AssociateTagOptionWithResource = AssociateTagOptionWithResource'
  { -- | The TagOption identifier.
    tagOptionId :: Lude.Text,
    -- | The resource identifier.
    resourceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTagOptionWithResource' with the minimum fields required to make a request.
--
-- * 'tagOptionId' - The TagOption identifier.
-- * 'resourceId' - The resource identifier.
mkAssociateTagOptionWithResource ::
  -- | 'tagOptionId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  AssociateTagOptionWithResource
mkAssociateTagOptionWithResource pTagOptionId_ pResourceId_ =
  AssociateTagOptionWithResource'
    { tagOptionId = pTagOptionId_,
      resourceId = pResourceId_
    }

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'tagOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrTagOptionId :: Lens.Lens' AssociateTagOptionWithResource Lude.Text
atowrTagOptionId = Lens.lens (tagOptionId :: AssociateTagOptionWithResource -> Lude.Text) (\s a -> s {tagOptionId = a} :: AssociateTagOptionWithResource)
{-# DEPRECATED atowrTagOptionId "Use generic-lens or generic-optics with 'tagOptionId' instead." #-}

-- | The resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrResourceId :: Lens.Lens' AssociateTagOptionWithResource Lude.Text
atowrResourceId = Lens.lens (resourceId :: AssociateTagOptionWithResource -> Lude.Text) (\s a -> s {resourceId = a} :: AssociateTagOptionWithResource)
{-# DEPRECATED atowrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest AssociateTagOptionWithResource where
  type
    Rs AssociateTagOptionWithResource =
      AssociateTagOptionWithResourceResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateTagOptionWithResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateTagOptionWithResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.AssociateTagOptionWithResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateTagOptionWithResource where
  toJSON AssociateTagOptionWithResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TagOptionId" Lude..= tagOptionId),
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath AssociateTagOptionWithResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateTagOptionWithResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateTagOptionWithResourceResponse' smart constructor.
newtype AssociateTagOptionWithResourceResponse = AssociateTagOptionWithResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTagOptionWithResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateTagOptionWithResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateTagOptionWithResourceResponse
mkAssociateTagOptionWithResourceResponse pResponseStatus_ =
  AssociateTagOptionWithResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrrsResponseStatus :: Lens.Lens' AssociateTagOptionWithResourceResponse Lude.Int
atowrrsResponseStatus = Lens.lens (responseStatus :: AssociateTagOptionWithResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateTagOptionWithResourceResponse)
{-# DEPRECATED atowrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
