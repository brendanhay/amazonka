{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetUserEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the endpoints that are associated with a specific user ID.
module Network.AWS.Pinpoint.GetUserEndpoints
  ( -- * Creating a request
    GetUserEndpoints (..),
    mkGetUserEndpoints,

    -- ** Request lenses
    gueUserId,
    gueApplicationId,

    -- * Destructuring the response
    GetUserEndpointsResponse (..),
    mkGetUserEndpointsResponse,

    -- ** Response lenses
    guersEndpointsResponse,
    guersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUserEndpoints' smart constructor.
data GetUserEndpoints = GetUserEndpoints'
  { -- | The unique identifier for the user.
    userId :: Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserEndpoints' with the minimum fields required to make a request.
--
-- * 'userId' - The unique identifier for the user.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetUserEndpoints ::
  -- | 'userId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetUserEndpoints
mkGetUserEndpoints pUserId_ pApplicationId_ =
  GetUserEndpoints'
    { userId = pUserId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gueUserId :: Lens.Lens' GetUserEndpoints Lude.Text
gueUserId = Lens.lens (userId :: GetUserEndpoints -> Lude.Text) (\s a -> s {userId = a} :: GetUserEndpoints)
{-# DEPRECATED gueUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gueApplicationId :: Lens.Lens' GetUserEndpoints Lude.Text
gueApplicationId = Lens.lens (applicationId :: GetUserEndpoints -> Lude.Text) (\s a -> s {applicationId = a} :: GetUserEndpoints)
{-# DEPRECATED gueApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetUserEndpoints where
  type Rs GetUserEndpoints = GetUserEndpointsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUserEndpointsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUserEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetUserEndpoints where
  toPath GetUserEndpoints' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/users/", Lude.toBS userId]

instance Lude.ToQuery GetUserEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetUserEndpointsResponse' smart constructor.
data GetUserEndpointsResponse = GetUserEndpointsResponse'
  { endpointsResponse :: EndpointsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'endpointsResponse' -
-- * 'responseStatus' - The response status code.
mkGetUserEndpointsResponse ::
  -- | 'endpointsResponse'
  EndpointsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetUserEndpointsResponse
mkGetUserEndpointsResponse pEndpointsResponse_ pResponseStatus_ =
  GetUserEndpointsResponse'
    { endpointsResponse =
        pEndpointsResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guersEndpointsResponse :: Lens.Lens' GetUserEndpointsResponse EndpointsResponse
guersEndpointsResponse = Lens.lens (endpointsResponse :: GetUserEndpointsResponse -> EndpointsResponse) (\s a -> s {endpointsResponse = a} :: GetUserEndpointsResponse)
{-# DEPRECATED guersEndpointsResponse "Use generic-lens or generic-optics with 'endpointsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guersResponseStatus :: Lens.Lens' GetUserEndpointsResponse Lude.Int
guersResponseStatus = Lens.lens (responseStatus :: GetUserEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserEndpointsResponse)
{-# DEPRECATED guersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
