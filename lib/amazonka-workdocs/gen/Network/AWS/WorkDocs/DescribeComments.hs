{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeComments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all the comments for the specified document version.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeComments
  ( -- * Creating a request
    DescribeComments (..),
    mkDescribeComments,

    -- ** Request lenses
    dcVersionId,
    dcDocumentId,
    dcAuthenticationToken,
    dcMarker,
    dcLimit,

    -- * Destructuring the response
    DescribeCommentsResponse (..),
    mkDescribeCommentsResponse,

    -- ** Response lenses
    dcrsMarker,
    dcrsComments,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeComments' smart constructor.
data DescribeComments = DescribeComments'
  { -- | The ID of the document version.
    versionId :: Lude.Text,
    -- | The ID of the document.
    documentId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The marker for the next set of results. This marker was received from a previous call.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeComments' with the minimum fields required to make a request.
--
-- * 'versionId' - The ID of the document version.
-- * 'documentId' - The ID of the document.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'marker' - The marker for the next set of results. This marker was received from a previous call.
-- * 'limit' - The maximum number of items to return.
mkDescribeComments ::
  -- | 'versionId'
  Lude.Text ->
  -- | 'documentId'
  Lude.Text ->
  DescribeComments
mkDescribeComments pVersionId_ pDocumentId_ =
  DescribeComments'
    { versionId = pVersionId_,
      documentId = pDocumentId_,
      authenticationToken = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The ID of the document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcVersionId :: Lens.Lens' DescribeComments Lude.Text
dcVersionId = Lens.lens (versionId :: DescribeComments -> Lude.Text) (\s a -> s {versionId = a} :: DescribeComments)
{-# DEPRECATED dcVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDocumentId :: Lens.Lens' DescribeComments Lude.Text
dcDocumentId = Lens.lens (documentId :: DescribeComments -> Lude.Text) (\s a -> s {documentId = a} :: DescribeComments)
{-# DEPRECATED dcDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAuthenticationToken :: Lens.Lens' DescribeComments (Lude.Maybe (Lude.Sensitive Lude.Text))
dcAuthenticationToken = Lens.lens (authenticationToken :: DescribeComments -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DescribeComments)
{-# DEPRECATED dcAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The marker for the next set of results. This marker was received from a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMarker :: Lens.Lens' DescribeComments (Lude.Maybe Lude.Text)
dcMarker = Lens.lens (marker :: DescribeComments -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeComments)
{-# DEPRECATED dcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLimit :: Lens.Lens' DescribeComments (Lude.Maybe Lude.Natural)
dcLimit = Lens.lens (limit :: DescribeComments -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeComments)
{-# DEPRECATED dcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeComments where
  page rq rs
    | Page.stop (rs Lens.^. dcrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcrsComments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dcMarker Lens..~ rs Lens.^. dcrsMarker

instance Lude.AWSRequest DescribeComments where
  type Rs DescribeComments = DescribeCommentsResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCommentsResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "Comments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeComments where
  toHeaders DescribeComments' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeComments where
  toPath DescribeComments' {..} =
    Lude.mconcat
      [ "/api/v1/documents/",
        Lude.toBS documentId,
        "/versions/",
        Lude.toBS versionId,
        "/comments"
      ]

instance Lude.ToQuery DescribeComments where
  toQuery DescribeComments' {..} =
    Lude.mconcat ["marker" Lude.=: marker, "limit" Lude.=: limit]

-- | /See:/ 'mkDescribeCommentsResponse' smart constructor.
data DescribeCommentsResponse = DescribeCommentsResponse'
  { -- | The marker for the next set of results. This marker was received from a previous call.
    marker :: Lude.Maybe Lude.Text,
    -- | The list of comments for the specified document version.
    comments :: Lude.Maybe [Comment],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCommentsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - The marker for the next set of results. This marker was received from a previous call.
-- * 'comments' - The list of comments for the specified document version.
-- * 'responseStatus' - The response status code.
mkDescribeCommentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCommentsResponse
mkDescribeCommentsResponse pResponseStatus_ =
  DescribeCommentsResponse'
    { marker = Lude.Nothing,
      comments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The marker for the next set of results. This marker was received from a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsMarker :: Lens.Lens' DescribeCommentsResponse (Lude.Maybe Lude.Text)
dcrsMarker = Lens.lens (marker :: DescribeCommentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCommentsResponse)
{-# DEPRECATED dcrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of comments for the specified document version.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsComments :: Lens.Lens' DescribeCommentsResponse (Lude.Maybe [Comment])
dcrsComments = Lens.lens (comments :: DescribeCommentsResponse -> Lude.Maybe [Comment]) (\s a -> s {comments = a} :: DescribeCommentsResponse)
{-# DEPRECATED dcrsComments "Use generic-lens or generic-optics with 'comments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeCommentsResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeCommentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCommentsResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
