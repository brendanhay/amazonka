{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a specific endpoint. Use this operation to get the status of an endpoint.
module Network.AWS.Comprehend.DescribeEndpoint
  ( -- * Creating a request
    DescribeEndpoint (..),
    mkDescribeEndpoint,

    -- ** Request lenses
    desEndpointARN,

    -- * Destructuring the response
    DescribeEndpointResponse (..),
    mkDescribeEndpointResponse,

    -- ** Response lenses
    dersEndpointProperties,
    dersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { endpointARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Number (ARN) of the endpoint being described.
mkDescribeEndpoint ::
  -- | 'endpointARN'
  Lude.Text ->
  DescribeEndpoint
mkDescribeEndpoint pEndpointARN_ =
  DescribeEndpoint' {endpointARN = pEndpointARN_}

-- | The Amazon Resource Number (ARN) of the endpoint being described.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEndpointARN :: Lens.Lens' DescribeEndpoint Lude.Text
desEndpointARN = Lens.lens (endpointARN :: DescribeEndpoint -> Lude.Text) (\s a -> s {endpointARN = a} :: DescribeEndpoint)
{-# DEPRECATED desEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest DescribeEndpoint where
  type Rs DescribeEndpoint = DescribeEndpointResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Lude.<$> (x Lude..?> "EndpointProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DescribeEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEndpoint where
  toJSON DescribeEndpoint' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EndpointArn" Lude..= endpointARN)])

instance Lude.ToPath DescribeEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { endpointProperties ::
      Lude.Maybe EndpointProperties,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointProperties' - Describes information associated with the specific endpoint.
-- * 'responseStatus' - The response status code.
mkDescribeEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEndpointResponse
mkDescribeEndpointResponse pResponseStatus_ =
  DescribeEndpointResponse'
    { endpointProperties = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Describes information associated with the specific endpoint.
--
-- /Note:/ Consider using 'endpointProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpointProperties :: Lens.Lens' DescribeEndpointResponse (Lude.Maybe EndpointProperties)
dersEndpointProperties = Lens.lens (endpointProperties :: DescribeEndpointResponse -> Lude.Maybe EndpointProperties) (\s a -> s {endpointProperties = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersEndpointProperties "Use generic-lens or generic-optics with 'endpointProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEndpointResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
