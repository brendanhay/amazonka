{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'Resource' resource.
module Network.AWS.APIGateway.DeleteResource
  ( -- * Creating a request
    DeleteResource (..),
    mkDeleteResource,

    -- ** Request lenses
    drRestAPIId,
    drResourceId,

    -- * Destructuring the response
    DeleteResourceResponse (..),
    mkDeleteResourceResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete a 'Resource' .
--
-- /See:/ 'mkDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { restAPIId :: Lude.Text,
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

-- | Creates a value of 'DeleteResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - [Required] The identifier of the 'Resource' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteResource ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  DeleteResource
mkDeleteResource pRestAPIId_ pResourceId_ =
  DeleteResource'
    { restAPIId = pRestAPIId_,
      resourceId = pResourceId_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRestAPIId :: Lens.Lens' DeleteResource Lude.Text
drRestAPIId = Lens.lens (restAPIId :: DeleteResource -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteResource)
{-# DEPRECATED drRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The identifier of the 'Resource' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drResourceId :: Lens.Lens' DeleteResource Lude.Text
drResourceId = Lens.lens (resourceId :: DeleteResource -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteResource)
{-# DEPRECATED drResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest DeleteResource where
  type Rs DeleteResource = DeleteResourceResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteResourceResponse'

instance Lude.ToHeaders DeleteResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteResource where
  toPath DeleteResource' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId
      ]

instance Lude.ToQuery DeleteResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourceResponse' smart constructor.
data DeleteResourceResponse = DeleteResourceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceResponse' with the minimum fields required to make a request.
mkDeleteResourceResponse ::
  DeleteResourceResponse
mkDeleteResourceResponse = DeleteResourceResponse'
