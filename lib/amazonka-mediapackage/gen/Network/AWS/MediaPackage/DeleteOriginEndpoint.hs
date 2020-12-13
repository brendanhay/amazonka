{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DeleteOriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing OriginEndpoint.
module Network.AWS.MediaPackage.DeleteOriginEndpoint
  ( -- * Creating a request
    DeleteOriginEndpoint (..),
    mkDeleteOriginEndpoint,

    -- ** Request lenses
    doeId,

    -- * Destructuring the response
    DeleteOriginEndpointResponse (..),
    mkDeleteOriginEndpointResponse,

    -- ** Response lenses
    doersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteOriginEndpoint' smart constructor.
newtype DeleteOriginEndpoint = DeleteOriginEndpoint'
  { -- | The ID of the OriginEndpoint to delete.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOriginEndpoint' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the OriginEndpoint to delete.
mkDeleteOriginEndpoint ::
  -- | 'id'
  Lude.Text ->
  DeleteOriginEndpoint
mkDeleteOriginEndpoint pId_ = DeleteOriginEndpoint' {id = pId_}

-- | The ID of the OriginEndpoint to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doeId :: Lens.Lens' DeleteOriginEndpoint Lude.Text
doeId = Lens.lens (id :: DeleteOriginEndpoint -> Lude.Text) (\s a -> s {id = a} :: DeleteOriginEndpoint)
{-# DEPRECATED doeId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteOriginEndpoint where
  type Rs DeleteOriginEndpoint = DeleteOriginEndpointResponse
  request = Req.delete mediaPackageService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteOriginEndpointResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteOriginEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteOriginEndpoint where
  toPath DeleteOriginEndpoint' {..} =
    Lude.mconcat ["/origin_endpoints/", Lude.toBS id]

instance Lude.ToQuery DeleteOriginEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteOriginEndpointResponse' smart constructor.
newtype DeleteOriginEndpointResponse = DeleteOriginEndpointResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOriginEndpointResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteOriginEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteOriginEndpointResponse
mkDeleteOriginEndpointResponse pResponseStatus_ =
  DeleteOriginEndpointResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doersResponseStatus :: Lens.Lens' DeleteOriginEndpointResponse Lude.Int
doersResponseStatus = Lens.lens (responseStatus :: DeleteOriginEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteOriginEndpointResponse)
{-# DEPRECATED doersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
