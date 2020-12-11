{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteUserEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all the endpoints that are associated with a specific user ID.
module Network.AWS.Pinpoint.DeleteUserEndpoints
  ( -- * Creating a request
    DeleteUserEndpoints (..),
    mkDeleteUserEndpoints,

    -- ** Request lenses
    dueApplicationId,
    dueUserId,

    -- * Destructuring the response
    DeleteUserEndpointsResponse (..),
    mkDeleteUserEndpointsResponse,

    -- ** Response lenses
    duersResponseStatus,
    duersEndpointsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserEndpoints' smart constructor.
data DeleteUserEndpoints = DeleteUserEndpoints'
  { applicationId ::
      Lude.Text,
    userId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserEndpoints' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'userId' - The unique identifier for the user.
mkDeleteUserEndpoints ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  DeleteUserEndpoints
mkDeleteUserEndpoints pApplicationId_ pUserId_ =
  DeleteUserEndpoints'
    { applicationId = pApplicationId_,
      userId = pUserId_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dueApplicationId :: Lens.Lens' DeleteUserEndpoints Lude.Text
dueApplicationId = Lens.lens (applicationId :: DeleteUserEndpoints -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteUserEndpoints)
{-# DEPRECATED dueApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dueUserId :: Lens.Lens' DeleteUserEndpoints Lude.Text
dueUserId = Lens.lens (userId :: DeleteUserEndpoints -> Lude.Text) (\s a -> s {userId = a} :: DeleteUserEndpoints)
{-# DEPRECATED dueUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest DeleteUserEndpoints where
  type Rs DeleteUserEndpoints = DeleteUserEndpointsResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteUserEndpointsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteUserEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteUserEndpoints where
  toPath DeleteUserEndpoints' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/users/", Lude.toBS userId]

instance Lude.ToQuery DeleteUserEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserEndpointsResponse' smart constructor.
data DeleteUserEndpointsResponse = DeleteUserEndpointsResponse'
  { responseStatus ::
      Lude.Int,
    endpointsResponse ::
      EndpointsResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'endpointsResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteUserEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'endpointsResponse'
  EndpointsResponse ->
  DeleteUserEndpointsResponse
mkDeleteUserEndpointsResponse pResponseStatus_ pEndpointsResponse_ =
  DeleteUserEndpointsResponse'
    { responseStatus = pResponseStatus_,
      endpointsResponse = pEndpointsResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duersResponseStatus :: Lens.Lens' DeleteUserEndpointsResponse Lude.Int
duersResponseStatus = Lens.lens (responseStatus :: DeleteUserEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUserEndpointsResponse)
{-# DEPRECATED duersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duersEndpointsResponse :: Lens.Lens' DeleteUserEndpointsResponse EndpointsResponse
duersEndpointsResponse = Lens.lens (endpointsResponse :: DeleteUserEndpointsResponse -> EndpointsResponse) (\s a -> s {endpointsResponse = a} :: DeleteUserEndpointsResponse)
{-# DEPRECATED duersEndpointsResponse "Use generic-lens or generic-optics with 'endpointsResponse' instead." #-}
