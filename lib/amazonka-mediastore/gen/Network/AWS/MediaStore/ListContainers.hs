{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.ListContainers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the properties of all containers in AWS Elemental MediaStore.
--
-- You can query to receive all the containers in one response. Or you can include the @MaxResults@ parameter to receive a limited number of containers in each response. In this case, the response includes a token. To get the next set of containers, send the command again, this time with the @NextToken@ parameter (with the returned token as its value). The next set of responses appears, with a token if there are still more containers to receive.
-- See also 'DescribeContainer' , which gets the properties of one container.
--
-- This operation returns paginated results.
module Network.AWS.MediaStore.ListContainers
  ( -- * Creating a request
    ListContainers (..),
    mkListContainers,

    -- ** Request lenses
    lcNextToken,
    lcMaxResults,

    -- * Destructuring the response
    ListContainersResponse (..),
    mkListContainersResponse,

    -- ** Response lenses
    lcrsNextToken,
    lcrsResponseStatus,
    lcrsContainers,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListContainers' smart constructor.
data ListContainers = ListContainers'
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

-- | Creates a value of 'ListContainers' with the minimum fields required to make a request.
--
-- * 'maxResults' - Enter the maximum number of containers in the response. Use from 1 to 255 characters.
-- * 'nextToken' - Only if you used @MaxResults@ in the first command, enter the token (which was included in the previous response) to obtain the next set of containers. This token is included in a response only if there actually are more containers to list.
mkListContainers ::
  ListContainers
mkListContainers =
  ListContainers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Only if you used @MaxResults@ in the first command, enter the token (which was included in the previous response) to obtain the next set of containers. This token is included in a response only if there actually are more containers to list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListContainers (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListContainers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListContainers)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Enter the maximum number of containers in the response. Use from 1 to 255 characters.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListContainers (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListContainers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListContainers)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListContainers where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsContainers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListContainers where
  type Rs ListContainers = ListContainersResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListContainersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Containers" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListContainers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.ListContainers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListContainers where
  toJSON ListContainers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListContainers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListContainers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListContainersResponse' smart constructor.
data ListContainersResponse = ListContainersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    containers :: [Container]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListContainersResponse' with the minimum fields required to make a request.
--
-- * 'containers' - The names of the containers.
-- * 'nextToken' - @NextToken@ is the token to use in the next call to @ListContainers@ . This token is returned only if you included the @MaxResults@ tag in the original command, and only if there are still containers to return.
-- * 'responseStatus' - The response status code.
mkListContainersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListContainersResponse
mkListContainersResponse pResponseStatus_ =
  ListContainersResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      containers = Lude.mempty
    }

-- | @NextToken@ is the token to use in the next call to @ListContainers@ . This token is returned only if you included the @MaxResults@ tag in the original command, and only if there are still containers to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListContainersResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListContainersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListContainersResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListContainersResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListContainersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListContainersResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The names of the containers.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsContainers :: Lens.Lens' ListContainersResponse [Container]
lcrsContainers = Lens.lens (containers :: ListContainersResponse -> [Container]) (\s a -> s {containers = a} :: ListContainersResponse)
{-# DEPRECATED lcrsContainers "Use generic-lens or generic-optics with 'containers' instead." #-}
