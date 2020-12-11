{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDevEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all the development endpoints in this AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetDevEndpoints
  ( -- * Creating a request
    GetDevEndpoints (..),
    mkGetDevEndpoints,

    -- ** Request lenses
    gdeNextToken,
    gdeMaxResults,

    -- * Destructuring the response
    GetDevEndpointsResponse (..),
    mkGetDevEndpointsResponse,

    -- ** Response lenses
    gdersNextToken,
    gdersDevEndpoints,
    gdersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDevEndpoints' smart constructor.
data GetDevEndpoints = GetDevEndpoints'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevEndpoints' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum size of information to return.
-- * 'nextToken' - A continuation token, if this is a continuation call.
mkGetDevEndpoints ::
  GetDevEndpoints
mkGetDevEndpoints =
  GetDevEndpoints'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeNextToken :: Lens.Lens' GetDevEndpoints (Lude.Maybe Lude.Text)
gdeNextToken = Lens.lens (nextToken :: GetDevEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDevEndpoints)
{-# DEPRECATED gdeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of information to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeMaxResults :: Lens.Lens' GetDevEndpoints (Lude.Maybe Lude.Natural)
gdeMaxResults = Lens.lens (maxResults :: GetDevEndpoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetDevEndpoints)
{-# DEPRECATED gdeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetDevEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. gdersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gdersDevEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdeNextToken Lens..~ rs Lens.^. gdersNextToken

instance Lude.AWSRequest GetDevEndpoints where
  type Rs GetDevEndpoints = GetDevEndpointsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDevEndpointsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "DevEndpoints" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDevEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetDevEndpoints" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDevEndpoints where
  toJSON GetDevEndpoints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetDevEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDevEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDevEndpointsResponse' smart constructor.
data GetDevEndpointsResponse = GetDevEndpointsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    devEndpoints :: Lude.Maybe [DevEndpoint],
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

-- | Creates a value of 'GetDevEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'devEndpoints' - A list of @DevEndpoint@ definitions.
-- * 'nextToken' - A continuation token, if not all @DevEndpoint@ definitions have yet been returned.
-- * 'responseStatus' - The response status code.
mkGetDevEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDevEndpointsResponse
mkGetDevEndpointsResponse pResponseStatus_ =
  GetDevEndpointsResponse'
    { nextToken = Lude.Nothing,
      devEndpoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if not all @DevEndpoint@ definitions have yet been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdersNextToken :: Lens.Lens' GetDevEndpointsResponse (Lude.Maybe Lude.Text)
gdersNextToken = Lens.lens (nextToken :: GetDevEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetDevEndpointsResponse)
{-# DEPRECATED gdersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @DevEndpoint@ definitions.
--
-- /Note:/ Consider using 'devEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdersDevEndpoints :: Lens.Lens' GetDevEndpointsResponse (Lude.Maybe [DevEndpoint])
gdersDevEndpoints = Lens.lens (devEndpoints :: GetDevEndpointsResponse -> Lude.Maybe [DevEndpoint]) (\s a -> s {devEndpoints = a} :: GetDevEndpointsResponse)
{-# DEPRECATED gdersDevEndpoints "Use generic-lens or generic-optics with 'devEndpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdersResponseStatus :: Lens.Lens' GetDevEndpointsResponse Lude.Int
gdersResponseStatus = Lens.lens (responseStatus :: GetDevEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDevEndpointsResponse)
{-# DEPRECATED gdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
