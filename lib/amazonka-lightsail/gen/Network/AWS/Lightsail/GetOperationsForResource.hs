{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetOperationsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets operations for a specific resource (e.g., an instance or a static IP).
module Network.AWS.Lightsail.GetOperationsForResource
  ( -- * Creating a request
    GetOperationsForResource (..),
    mkGetOperationsForResource,

    -- ** Request lenses
    gofrResourceName,
    gofrPageToken,

    -- * Destructuring the response
    GetOperationsForResourceResponse (..),
    mkGetOperationsForResourceResponse,

    -- ** Response lenses
    gofrrsNextPageCount,
    gofrrsNextPageToken,
    gofrrsOperations,
    gofrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOperationsForResource' smart constructor.
data GetOperationsForResource = GetOperationsForResource'
  { -- | The name of the resource for which you are requesting information.
    resourceName :: Lude.Text,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetOperationsForResource@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperationsForResource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The name of the resource for which you are requesting information.
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperationsForResource@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetOperationsForResource ::
  -- | 'resourceName'
  Lude.Text ->
  GetOperationsForResource
mkGetOperationsForResource pResourceName_ =
  GetOperationsForResource'
    { resourceName = pResourceName_,
      pageToken = Lude.Nothing
    }

-- | The name of the resource for which you are requesting information.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrResourceName :: Lens.Lens' GetOperationsForResource Lude.Text
gofrResourceName = Lens.lens (resourceName :: GetOperationsForResource -> Lude.Text) (\s a -> s {resourceName = a} :: GetOperationsForResource)
{-# DEPRECATED gofrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperationsForResource@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrPageToken :: Lens.Lens' GetOperationsForResource (Lude.Maybe Lude.Text)
gofrPageToken = Lens.lens (pageToken :: GetOperationsForResource -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetOperationsForResource)
{-# DEPRECATED gofrPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Lude.AWSRequest GetOperationsForResource where
  type Rs GetOperationsForResource = GetOperationsForResourceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOperationsForResourceResponse'
            Lude.<$> (x Lude..?> "nextPageCount")
            Lude.<*> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOperationsForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetOperationsForResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOperationsForResource where
  toJSON GetOperationsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resourceName" Lude..= resourceName),
            ("pageToken" Lude..=) Lude.<$> pageToken
          ]
      )

instance Lude.ToPath GetOperationsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOperationsForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOperationsForResourceResponse' smart constructor.
data GetOperationsForResourceResponse = GetOperationsForResourceResponse'
  { -- | (Deprecated) Returns the number of pages of results that remain.
    nextPageCount :: Lude.Maybe Lude.Text,
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetOperationsForResource@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperationsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'nextPageCount' - (Deprecated) Returns the number of pages of results that remain.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetOperationsForResource@ request and specify the next page token using the @pageToken@ parameter.
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkGetOperationsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOperationsForResourceResponse
mkGetOperationsForResourceResponse pResponseStatus_ =
  GetOperationsForResourceResponse'
    { nextPageCount = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | (Deprecated) Returns the number of pages of results that remain.
--
-- /Note:/ Consider using 'nextPageCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrrsNextPageCount :: Lens.Lens' GetOperationsForResourceResponse (Lude.Maybe Lude.Text)
gofrrsNextPageCount = Lens.lens (nextPageCount :: GetOperationsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageCount = a} :: GetOperationsForResourceResponse)
{-# DEPRECATED gofrrsNextPageCount "Use generic-lens or generic-optics with 'nextPageCount' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetOperationsForResource@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrrsNextPageToken :: Lens.Lens' GetOperationsForResourceResponse (Lude.Maybe Lude.Text)
gofrrsNextPageToken = Lens.lens (nextPageToken :: GetOperationsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetOperationsForResourceResponse)
{-# DEPRECATED gofrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrrsOperations :: Lens.Lens' GetOperationsForResourceResponse (Lude.Maybe [Operation])
gofrrsOperations = Lens.lens (operations :: GetOperationsForResourceResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: GetOperationsForResourceResponse)
{-# DEPRECATED gofrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrrsResponseStatus :: Lens.Lens' GetOperationsForResourceResponse Lude.Int
gofrrsResponseStatus = Lens.lens (responseStatus :: GetOperationsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOperationsForResourceResponse)
{-# DEPRECATED gofrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
