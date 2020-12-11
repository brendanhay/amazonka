{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the regional endpoint information.
module Network.AWS.DynamoDB.DescribeEndpoints
  ( -- * Creating a request
    DescribeEndpoints (..),
    mkDescribeEndpoints,

    -- * Destructuring the response
    DescribeEndpointsResponse (..),
    mkDescribeEndpointsResponse,

    -- ** Response lenses
    dersResponseStatus,
    dersEndpoints,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpoints' with the minimum fields required to make a request.
mkDescribeEndpoints ::
  DescribeEndpoints
mkDescribeEndpoints = DescribeEndpoints'

instance Lude.AWSRequest DescribeEndpoints where
  type Rs DescribeEndpoints = DescribeEndpointsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Endpoints" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribeEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DescribeEndpoints" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEndpoints where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { responseStatus ::
      Lude.Int,
    endpoints :: [Endpoint]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'endpoints' - List of endpoints.
-- * 'responseStatus' - The response status code.
mkDescribeEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEndpointsResponse
mkDescribeEndpointsResponse pResponseStatus_ =
  DescribeEndpointsResponse'
    { responseStatus = pResponseStatus_,
      endpoints = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEndpointsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | List of endpoints.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpoints :: Lens.Lens' DescribeEndpointsResponse [Endpoint]
dersEndpoints = Lens.lens (endpoints :: DescribeEndpointsResponse -> [Endpoint]) (\s a -> s {endpoints = a} :: DescribeEndpointsResponse)
{-# DEPRECATED dersEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}
