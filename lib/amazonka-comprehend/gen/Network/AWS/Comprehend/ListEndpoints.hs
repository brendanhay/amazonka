{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all existing endpoints that you've created.
module Network.AWS.Comprehend.ListEndpoints
  ( -- * Creating a request
    ListEndpoints (..),
    mkListEndpoints,

    -- ** Request lenses
    leNextToken,
    leFilter,
    leMaxResults,

    -- * Destructuring the response
    ListEndpointsResponse (..),
    mkListEndpointsResponse,

    -- ** Response lenses
    lersEndpointPropertiesList,
    lersNextToken,
    lersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter :: Lude.Maybe EndpointFilter,
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

-- | Creates a value of 'ListEndpoints' with the minimum fields required to make a request.
--
-- * 'filter' - Filters the endpoints that are returned. You can filter endpoints on their name, model, status, or the date and time that they were created. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default is 100.
-- * 'nextToken' - Identifies the next page of results to return.
mkListEndpoints ::
  ListEndpoints
mkListEndpoints =
  ListEndpoints'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Text)
leNextToken = Lens.lens (nextToken :: ListEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpoints)
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the endpoints that are returned. You can filter endpoints on their name, model, status, or the date and time that they were created. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leFilter :: Lens.Lens' ListEndpoints (Lude.Maybe EndpointFilter)
leFilter = Lens.lens (filter :: ListEndpoints -> Lude.Maybe EndpointFilter) (\s a -> s {filter = a} :: ListEndpoints)
{-# DEPRECATED leFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Natural)
leMaxResults = Lens.lens (maxResults :: ListEndpoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListEndpoints)
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListEndpoints where
  type Rs ListEndpoints = ListEndpointsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Lude.<$> (x Lude..?> "EndpointPropertiesList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.ListEndpoints" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { endpointPropertiesList ::
      Lude.Maybe [EndpointProperties],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'endpointPropertiesList' - Displays a list of endpoint properties being retrieved by the service in response to the request.
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'responseStatus' - The response status code.
mkListEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEndpointsResponse
mkListEndpointsResponse pResponseStatus_ =
  ListEndpointsResponse'
    { endpointPropertiesList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Displays a list of endpoint properties being retrieved by the service in response to the request.
--
-- /Note:/ Consider using 'endpointPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersEndpointPropertiesList :: Lens.Lens' ListEndpointsResponse (Lude.Maybe [EndpointProperties])
lersEndpointPropertiesList = Lens.lens (endpointPropertiesList :: ListEndpointsResponse -> Lude.Maybe [EndpointProperties]) (\s a -> s {endpointPropertiesList = a} :: ListEndpointsResponse)
{-# DEPRECATED lersEndpointPropertiesList "Use generic-lens or generic-optics with 'endpointPropertiesList' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersNextToken :: Lens.Lens' ListEndpointsResponse (Lude.Maybe Lude.Text)
lersNextToken = Lens.lens (nextToken :: ListEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpointsResponse)
{-# DEPRECATED lersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersResponseStatus :: Lens.Lens' ListEndpointsResponse Lude.Int
lersResponseStatus = Lens.lens (responseStatus :: ListEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEndpointsResponse)
{-# DEPRECATED lersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
