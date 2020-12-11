{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified TagOption from the specified resource.
module Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
  ( -- * Creating a request
    DisassociateTagOptionFromResource (..),
    mkDisassociateTagOptionFromResource,

    -- ** Request lenses
    dtofrResourceId,
    dtofrTagOptionId,

    -- * Destructuring the response
    DisassociateTagOptionFromResourceResponse (..),
    mkDisassociateTagOptionFromResourceResponse,

    -- ** Response lenses
    dtofrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDisassociateTagOptionFromResource' smart constructor.
data DisassociateTagOptionFromResource = DisassociateTagOptionFromResource'
  { resourceId ::
      Lude.Text,
    tagOptionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateTagOptionFromResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource identifier.
-- * 'tagOptionId' - The TagOption identifier.
mkDisassociateTagOptionFromResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'tagOptionId'
  Lude.Text ->
  DisassociateTagOptionFromResource
mkDisassociateTagOptionFromResource pResourceId_ pTagOptionId_ =
  DisassociateTagOptionFromResource'
    { resourceId = pResourceId_,
      tagOptionId = pTagOptionId_
    }

-- | The resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrResourceId :: Lens.Lens' DisassociateTagOptionFromResource Lude.Text
dtofrResourceId = Lens.lens (resourceId :: DisassociateTagOptionFromResource -> Lude.Text) (\s a -> s {resourceId = a} :: DisassociateTagOptionFromResource)
{-# DEPRECATED dtofrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'tagOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrTagOptionId :: Lens.Lens' DisassociateTagOptionFromResource Lude.Text
dtofrTagOptionId = Lens.lens (tagOptionId :: DisassociateTagOptionFromResource -> Lude.Text) (\s a -> s {tagOptionId = a} :: DisassociateTagOptionFromResource)
{-# DEPRECATED dtofrTagOptionId "Use generic-lens or generic-optics with 'tagOptionId' instead." #-}

instance Lude.AWSRequest DisassociateTagOptionFromResource where
  type
    Rs DisassociateTagOptionFromResource =
      DisassociateTagOptionFromResourceResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateTagOptionFromResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateTagOptionFromResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DisassociateTagOptionFromResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateTagOptionFromResource where
  toJSON DisassociateTagOptionFromResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("TagOptionId" Lude..= tagOptionId)
          ]
      )

instance Lude.ToPath DisassociateTagOptionFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateTagOptionFromResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateTagOptionFromResourceResponse' smart constructor.
newtype DisassociateTagOptionFromResourceResponse = DisassociateTagOptionFromResourceResponse'
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

-- | Creates a value of 'DisassociateTagOptionFromResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateTagOptionFromResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateTagOptionFromResourceResponse
mkDisassociateTagOptionFromResourceResponse pResponseStatus_ =
  DisassociateTagOptionFromResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrrsResponseStatus :: Lens.Lens' DisassociateTagOptionFromResourceResponse Lude.Int
dtofrrsResponseStatus = Lens.lens (responseStatus :: DisassociateTagOptionFromResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateTagOptionFromResourceResponse)
{-# DEPRECATED dtofrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
