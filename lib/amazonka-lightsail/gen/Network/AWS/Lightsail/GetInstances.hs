{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all Amazon Lightsail virtual private servers, or /instances/ .
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetInstances
  ( -- * Creating a request
    GetInstances (..),
    mkGetInstances,

    -- ** Request lenses
    giPageToken,

    -- * Destructuring the response
    GetInstancesResponse (..),
    mkGetInstancesResponse,

    -- ** Response lenses
    girsNextPageToken,
    girsInstances,
    girsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstances' smart constructor.
newtype GetInstances = GetInstances'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstances@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstances' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstances@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetInstances ::
  GetInstances
mkGetInstances = GetInstances' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstances@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giPageToken :: Lens.Lens' GetInstances (Lude.Maybe Lude.Text)
giPageToken = Lens.lens (pageToken :: GetInstances -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetInstances)
{-# DEPRECATED giPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetInstances where
  page rq rs
    | Page.stop (rs Lens.^. girsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. girsInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& giPageToken Lens..~ rs Lens.^. girsNextPageToken

instance Lude.AWSRequest GetInstances where
  type Rs GetInstances = GetInstancesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstancesResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "instances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstances where
  toJSON GetInstances' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstancesResponse' smart constructor.
data GetInstancesResponse = GetInstancesResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetInstances@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of key-value pairs containing information about your instances.
    instances :: Lude.Maybe [Instance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetInstances@ request and specify the next page token using the @pageToken@ parameter.
-- * 'instances' - An array of key-value pairs containing information about your instances.
-- * 'responseStatus' - The response status code.
mkGetInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstancesResponse
mkGetInstancesResponse pResponseStatus_ =
  GetInstancesResponse'
    { nextPageToken = Lude.Nothing,
      instances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetInstances@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsNextPageToken :: Lens.Lens' GetInstancesResponse (Lude.Maybe Lude.Text)
girsNextPageToken = Lens.lens (nextPageToken :: GetInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetInstancesResponse)
{-# DEPRECATED girsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of key-value pairs containing information about your instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsInstances :: Lens.Lens' GetInstancesResponse (Lude.Maybe [Instance])
girsInstances = Lens.lens (instances :: GetInstancesResponse -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: GetInstancesResponse)
{-# DEPRECATED girsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetInstancesResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstancesResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
