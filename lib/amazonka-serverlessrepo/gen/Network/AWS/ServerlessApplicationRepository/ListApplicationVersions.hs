{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists versions for the specified application.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
  ( -- * Creating a request
    ListApplicationVersions (..),
    mkListApplicationVersions,

    -- ** Request lenses
    lavNextToken,
    lavMaxItems,
    lavApplicationId,

    -- * Destructuring the response
    ListApplicationVersionsResponse (..),
    mkListApplicationVersionsResponse,

    -- ** Response lenses
    lavrsVersions,
    lavrsNextToken,
    lavrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkListApplicationVersions' smart constructor.
data ListApplicationVersions = ListApplicationVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplicationVersions' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'maxItems' - The total number of items to return.
-- * 'nextToken' - A token to specify where to start paginating.
mkListApplicationVersions ::
  -- | 'applicationId'
  Lude.Text ->
  ListApplicationVersions
mkListApplicationVersions pApplicationId_ =
  ListApplicationVersions'
    { nextToken = Lude.Nothing,
      maxItems = Lude.Nothing,
      applicationId = pApplicationId_
    }

-- | A token to specify where to start paginating.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavNextToken :: Lens.Lens' ListApplicationVersions (Lude.Maybe Lude.Text)
lavNextToken = Lens.lens (nextToken :: ListApplicationVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationVersions)
{-# DEPRECATED lavNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavMaxItems :: Lens.Lens' ListApplicationVersions (Lude.Maybe Lude.Natural)
lavMaxItems = Lens.lens (maxItems :: ListApplicationVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListApplicationVersions)
{-# DEPRECATED lavMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavApplicationId :: Lens.Lens' ListApplicationVersions Lude.Text
lavApplicationId = Lens.lens (applicationId :: ListApplicationVersions -> Lude.Text) (\s a -> s {applicationId = a} :: ListApplicationVersions)
{-# DEPRECATED lavApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Page.AWSPager ListApplicationVersions where
  page rq rs
    | Page.stop (rs Lens.^. lavrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lavrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lavNextToken Lens..~ rs Lens.^. lavrsNextToken

instance Lude.AWSRequest ListApplicationVersions where
  type Rs ListApplicationVersions = ListApplicationVersionsResponse
  request = Req.get serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApplicationVersionsResponse'
            Lude.<$> (x Lude..?> "versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApplicationVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListApplicationVersions where
  toPath ListApplicationVersions' {..} =
    Lude.mconcat
      ["/applications/", Lude.toBS applicationId, "/versions"]

instance Lude.ToQuery ListApplicationVersions where
  toQuery ListApplicationVersions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxItems" Lude.=: maxItems]

-- | /See:/ 'mkListApplicationVersionsResponse' smart constructor.
data ListApplicationVersionsResponse = ListApplicationVersionsResponse'
  { versions ::
      Lude.Maybe [VersionSummary],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListApplicationVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to request the next page of results.
-- * 'responseStatus' - The response status code.
-- * 'versions' - An array of version summaries for the application.
mkListApplicationVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListApplicationVersionsResponse
mkListApplicationVersionsResponse pResponseStatus_ =
  ListApplicationVersionsResponse'
    { versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of version summaries for the application.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsVersions :: Lens.Lens' ListApplicationVersionsResponse (Lude.Maybe [VersionSummary])
lavrsVersions = Lens.lens (versions :: ListApplicationVersionsResponse -> Lude.Maybe [VersionSummary]) (\s a -> s {versions = a} :: ListApplicationVersionsResponse)
{-# DEPRECATED lavrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsNextToken :: Lens.Lens' ListApplicationVersionsResponse (Lude.Maybe Lude.Text)
lavrsNextToken = Lens.lens (nextToken :: ListApplicationVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationVersionsResponse)
{-# DEPRECATED lavrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsResponseStatus :: Lens.Lens' ListApplicationVersionsResponse Lude.Int
lavrsResponseStatus = Lens.lens (responseStatus :: ListApplicationVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApplicationVersionsResponse)
{-# DEPRECATED lavrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
