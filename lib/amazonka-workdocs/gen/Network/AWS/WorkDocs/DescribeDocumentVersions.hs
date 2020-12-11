{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeDocumentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the document versions for the specified document.
--
-- By default, only active versions are returned.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeDocumentVersions
  ( -- * Creating a request
    DescribeDocumentVersions (..),
    mkDescribeDocumentVersions,

    -- ** Request lenses
    ddvInclude,
    ddvAuthenticationToken,
    ddvMarker,
    ddvLimit,
    ddvFields,
    ddvDocumentId,

    -- * Destructuring the response
    DescribeDocumentVersionsResponse (..),
    mkDescribeDocumentVersionsResponse,

    -- ** Response lenses
    ddvrsDocumentVersions,
    ddvrsMarker,
    ddvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeDocumentVersions' smart constructor.
data DescribeDocumentVersions = DescribeDocumentVersions'
  { include ::
      Lude.Maybe Lude.Text,
    authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    fields :: Lude.Maybe Lude.Text,
    documentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocumentVersions' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'documentId' - The ID of the document.
-- * 'fields' - Specify "SOURCE" to include initialized versions and a URL for the source document.
-- * 'include' - A comma-separated list of values. Specify "INITIALIZED" to include incomplete versions.
-- * 'limit' - The maximum number of versions to return with this call.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
mkDescribeDocumentVersions ::
  -- | 'documentId'
  Lude.Text ->
  DescribeDocumentVersions
mkDescribeDocumentVersions pDocumentId_ =
  DescribeDocumentVersions'
    { include = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      fields = Lude.Nothing,
      documentId = pDocumentId_
    }

-- | A comma-separated list of values. Specify "INITIALIZED" to include incomplete versions.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvInclude :: Lens.Lens' DescribeDocumentVersions (Lude.Maybe Lude.Text)
ddvInclude = Lens.lens (include :: DescribeDocumentVersions -> Lude.Maybe Lude.Text) (\s a -> s {include = a} :: DescribeDocumentVersions)
{-# DEPRECATED ddvInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvAuthenticationToken :: Lens.Lens' DescribeDocumentVersions (Lude.Maybe (Lude.Sensitive Lude.Text))
ddvAuthenticationToken = Lens.lens (authenticationToken :: DescribeDocumentVersions -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DescribeDocumentVersions)
{-# DEPRECATED ddvAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvMarker :: Lens.Lens' DescribeDocumentVersions (Lude.Maybe Lude.Text)
ddvMarker = Lens.lens (marker :: DescribeDocumentVersions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDocumentVersions)
{-# DEPRECATED ddvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of versions to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvLimit :: Lens.Lens' DescribeDocumentVersions (Lude.Maybe Lude.Natural)
ddvLimit = Lens.lens (limit :: DescribeDocumentVersions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeDocumentVersions)
{-# DEPRECATED ddvLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Specify "SOURCE" to include initialized versions and a URL for the source document.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvFields :: Lens.Lens' DescribeDocumentVersions (Lude.Maybe Lude.Text)
ddvFields = Lens.lens (fields :: DescribeDocumentVersions -> Lude.Maybe Lude.Text) (\s a -> s {fields = a} :: DescribeDocumentVersions)
{-# DEPRECATED ddvFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvDocumentId :: Lens.Lens' DescribeDocumentVersions Lude.Text
ddvDocumentId = Lens.lens (documentId :: DescribeDocumentVersions -> Lude.Text) (\s a -> s {documentId = a} :: DescribeDocumentVersions)
{-# DEPRECATED ddvDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

instance Page.AWSPager DescribeDocumentVersions where
  page rq rs
    | Page.stop (rs Lens.^. ddvrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddvrsDocumentVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& ddvMarker Lens..~ rs Lens.^. ddvrsMarker

instance Lude.AWSRequest DescribeDocumentVersions where
  type Rs DescribeDocumentVersions = DescribeDocumentVersionsResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDocumentVersionsResponse'
            Lude.<$> (x Lude..?> "DocumentVersions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDocumentVersions where
  toHeaders DescribeDocumentVersions' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeDocumentVersions where
  toPath DescribeDocumentVersions' {..} =
    Lude.mconcat
      ["/api/v1/documents/", Lude.toBS documentId, "/versions"]

instance Lude.ToQuery DescribeDocumentVersions where
  toQuery DescribeDocumentVersions' {..} =
    Lude.mconcat
      [ "include" Lude.=: include,
        "marker" Lude.=: marker,
        "limit" Lude.=: limit,
        "fields" Lude.=: fields
      ]

-- | /See:/ 'mkDescribeDocumentVersionsResponse' smart constructor.
data DescribeDocumentVersionsResponse = DescribeDocumentVersionsResponse'
  { documentVersions ::
      Lude.Maybe
        [DocumentVersionMetadata],
    marker ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocumentVersionsResponse' with the minimum fields required to make a request.
--
-- * 'documentVersions' - The document versions.
-- * 'marker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeDocumentVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDocumentVersionsResponse
mkDescribeDocumentVersionsResponse pResponseStatus_ =
  DescribeDocumentVersionsResponse'
    { documentVersions =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The document versions.
--
-- /Note:/ Consider using 'documentVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvrsDocumentVersions :: Lens.Lens' DescribeDocumentVersionsResponse (Lude.Maybe [DocumentVersionMetadata])
ddvrsDocumentVersions = Lens.lens (documentVersions :: DescribeDocumentVersionsResponse -> Lude.Maybe [DocumentVersionMetadata]) (\s a -> s {documentVersions = a} :: DescribeDocumentVersionsResponse)
{-# DEPRECATED ddvrsDocumentVersions "Use generic-lens or generic-optics with 'documentVersions' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvrsMarker :: Lens.Lens' DescribeDocumentVersionsResponse (Lude.Maybe Lude.Text)
ddvrsMarker = Lens.lens (marker :: DescribeDocumentVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDocumentVersionsResponse)
{-# DEPRECATED ddvrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvrsResponseStatus :: Lens.Lens' DescribeDocumentVersionsResponse Lude.Int
ddvrsResponseStatus = Lens.lens (responseStatus :: DescribeDocumentVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDocumentVersionsResponse)
{-# DEPRECATED ddvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
