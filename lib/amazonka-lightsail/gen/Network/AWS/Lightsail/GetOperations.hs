{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all operations.
--
-- Results are returned from oldest to newest, up to a maximum of 200. Results can be paged by making each subsequent call to @GetOperations@ use the maximum (last) @statusChangedAt@ value from the previous request.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetOperations
  ( -- * Creating a request
    GetOperations (..),
    mkGetOperations,

    -- ** Request lenses
    goPageToken,

    -- * Destructuring the response
    GetOperationsResponse (..),
    mkGetOperationsResponse,

    -- ** Response lenses
    gosrsNextPageToken,
    gosrsOperations,
    gosrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOperations' smart constructor.
newtype GetOperations = GetOperations'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetOperations@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperations' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperations@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetOperations ::
  GetOperations
mkGetOperations = GetOperations' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperations@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goPageToken :: Lens.Lens' GetOperations (Lude.Maybe Lude.Text)
goPageToken = Lens.lens (pageToken :: GetOperations -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetOperations)
{-# DEPRECATED goPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetOperations where
  page rq rs
    | Page.stop (rs Lens.^. gosrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gosrsOperations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& goPageToken Lens..~ rs Lens.^. gosrsNextPageToken

instance Lude.AWSRequest GetOperations where
  type Rs GetOperations = GetOperationsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOperationsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOperations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetOperations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOperations where
  toJSON GetOperations' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetOperations where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOperations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOperationsResponse' smart constructor.
data GetOperationsResponse = GetOperationsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetOperations@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperationsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetOperations@ request and specify the next page token using the @pageToken@ parameter.
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkGetOperationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOperationsResponse
mkGetOperationsResponse pResponseStatus_ =
  GetOperationsResponse'
    { nextPageToken = Lude.Nothing,
      operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetOperations@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsNextPageToken :: Lens.Lens' GetOperationsResponse (Lude.Maybe Lude.Text)
gosrsNextPageToken = Lens.lens (nextPageToken :: GetOperationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetOperationsResponse)
{-# DEPRECATED gosrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsOperations :: Lens.Lens' GetOperationsResponse (Lude.Maybe [Operation])
gosrsOperations = Lens.lens (operations :: GetOperationsResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: GetOperationsResponse)
{-# DEPRECATED gosrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsResponseStatus :: Lens.Lens' GetOperationsResponse Lude.Int
gosrsResponseStatus = Lens.lens (responseStatus :: GetOperationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOperationsResponse)
{-# DEPRECATED gosrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
