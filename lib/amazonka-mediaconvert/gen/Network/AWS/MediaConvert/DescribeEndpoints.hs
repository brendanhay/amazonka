{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DescribeEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send an request with an empty body to the regional API endpoint to get your account API endpoint.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.DescribeEndpoints
  ( -- * Creating a request
    DescribeEndpoints (..),
    mkDescribeEndpoints,

    -- ** Request lenses
    deMode,
    deNextToken,
    deMaxResults,

    -- * Destructuring the response
    DescribeEndpointsResponse (..),
    mkDescribeEndpointsResponse,

    -- ** Response lenses
    dersNextToken,
    dersEndpoints,
    dersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | DescribeEndpointsRequest
--
-- /See:/ 'mkDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { -- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
    mode :: Lude.Maybe DescribeEndpointsMode,
    -- | Use this string, provided with the response to a previous request, to request the next batch of endpoints.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Optional. Max number of endpoints, up to twenty, that will be returned at one time.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpoints' with the minimum fields required to make a request.
--
-- * 'mode' - Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
-- * 'nextToken' - Use this string, provided with the response to a previous request, to request the next batch of endpoints.
-- * 'maxResults' - Optional. Max number of endpoints, up to twenty, that will be returned at one time.
mkDescribeEndpoints ::
  DescribeEndpoints
mkDescribeEndpoints =
  DescribeEndpoints'
    { mode = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMode :: Lens.Lens' DescribeEndpoints (Lude.Maybe DescribeEndpointsMode)
deMode = Lens.lens (mode :: DescribeEndpoints -> Lude.Maybe DescribeEndpointsMode) (\s a -> s {mode = a} :: DescribeEndpoints)
{-# DEPRECATED deMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | Use this string, provided with the response to a previous request, to request the next batch of endpoints.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEndpoints (Lude.Maybe Lude.Text)
deNextToken = Lens.lens (nextToken :: DescribeEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEndpoints)
{-# DEPRECATED deNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. Max number of endpoints, up to twenty, that will be returned at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxResults :: Lens.Lens' DescribeEndpoints (Lude.Maybe Lude.Int)
deMaxResults = Lens.lens (maxResults :: DescribeEndpoints -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeEndpoints)
{-# DEPRECATED deMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. dersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dersEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deNextToken Lens..~ rs Lens.^. dersNextToken

instance Lude.AWSRequest DescribeEndpoints where
  type Rs DescribeEndpoints = DescribeEndpointsResponse
  request = Req.postJSON mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "endpoints" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEndpoints where
  toJSON DescribeEndpoints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("mode" Lude..=) Lude.<$> mode,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEndpoints where
  toPath = Lude.const "/2017-08-29/endpoints"

instance Lude.ToQuery DescribeEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | Use this string to request the next batch of endpoints.
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of endpoints
    endpoints :: Lude.Maybe [Endpoint],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Use this string to request the next batch of endpoints.
-- * 'endpoints' - List of endpoints
-- * 'responseStatus' - The response status code.
mkDescribeEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEndpointsResponse
mkDescribeEndpointsResponse pResponseStatus_ =
  DescribeEndpointsResponse'
    { nextToken = Lude.Nothing,
      endpoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Use this string to request the next batch of endpoints.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersNextToken :: Lens.Lens' DescribeEndpointsResponse (Lude.Maybe Lude.Text)
dersNextToken = Lens.lens (nextToken :: DescribeEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEndpointsResponse)
{-# DEPRECATED dersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of endpoints
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpoints :: Lens.Lens' DescribeEndpointsResponse (Lude.Maybe [Endpoint])
dersEndpoints = Lens.lens (endpoints :: DescribeEndpointsResponse -> Lude.Maybe [Endpoint]) (\s a -> s {endpoints = a} :: DescribeEndpointsResponse)
{-# DEPRECATED dersEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEndpointsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
