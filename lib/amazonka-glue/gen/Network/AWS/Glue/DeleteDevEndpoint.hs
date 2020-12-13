{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteDevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified development endpoint.
module Network.AWS.Glue.DeleteDevEndpoint
  ( -- * Creating a request
    DeleteDevEndpoint (..),
    mkDeleteDevEndpoint,

    -- ** Request lenses
    ddeEndpointName,

    -- * Destructuring the response
    DeleteDevEndpointResponse (..),
    mkDeleteDevEndpointResponse,

    -- ** Response lenses
    ddersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDevEndpoint' smart constructor.
newtype DeleteDevEndpoint = DeleteDevEndpoint'
  { -- | The name of the @DevEndpoint@ .
    endpointName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDevEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointName' - The name of the @DevEndpoint@ .
mkDeleteDevEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  DeleteDevEndpoint
mkDeleteDevEndpoint pEndpointName_ =
  DeleteDevEndpoint' {endpointName = pEndpointName_}

-- | The name of the @DevEndpoint@ .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeEndpointName :: Lens.Lens' DeleteDevEndpoint Lude.Text
ddeEndpointName = Lens.lens (endpointName :: DeleteDevEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: DeleteDevEndpoint)
{-# DEPRECATED ddeEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

instance Lude.AWSRequest DeleteDevEndpoint where
  type Rs DeleteDevEndpoint = DeleteDevEndpointResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDevEndpointResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDevEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteDevEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDevEndpoint where
  toJSON DeleteDevEndpoint' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EndpointName" Lude..= endpointName)])

instance Lude.ToPath DeleteDevEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDevEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDevEndpointResponse' smart constructor.
newtype DeleteDevEndpointResponse = DeleteDevEndpointResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDevEndpointResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDevEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDevEndpointResponse
mkDeleteDevEndpointResponse pResponseStatus_ =
  DeleteDevEndpointResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddersResponseStatus :: Lens.Lens' DeleteDevEndpointResponse Lude.Int
ddersResponseStatus = Lens.lens (responseStatus :: DeleteDevEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDevEndpointResponse)
{-# DEPRECATED ddersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
