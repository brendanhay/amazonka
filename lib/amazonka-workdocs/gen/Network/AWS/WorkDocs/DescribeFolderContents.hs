{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeFolderContents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the contents of the specified folder, including its documents and subfolders.
--
-- By default, Amazon WorkDocs returns the first 100 active document and folder metadata items. If there are more results, the response includes a marker that you can use to request the next set of results. You can also request initialized documents.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeFolderContents
  ( -- * Creating a request
    DescribeFolderContents (..),
    mkDescribeFolderContents,

    -- ** Request lenses
    dfcsInclude,
    dfcsAuthenticationToken,
    dfcsSort,
    dfcsMarker,
    dfcsLimit,
    dfcsType,
    dfcsOrder,
    dfcsFolderId,

    -- * Destructuring the response
    DescribeFolderContentsResponse (..),
    mkDescribeFolderContentsResponse,

    -- ** Response lenses
    dfcrsFolders,
    dfcrsDocuments,
    dfcrsMarker,
    dfcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeFolderContents' smart constructor.
data DescribeFolderContents = DescribeFolderContents'
  { include ::
      Lude.Maybe Lude.Text,
    authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    sort :: Lude.Maybe ResourceSortType,
    marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    type' :: Lude.Maybe FolderContentType,
    order :: Lude.Maybe OrderType,
    folderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFolderContents' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'folderId' - The ID of the folder.
-- * 'include' - The contents to include. Specify "INITIALIZED" to include initialized documents.
-- * 'limit' - The maximum number of items to return with this call.
-- * 'marker' - The marker for the next set of results. This marker was received from a previous call.
-- * 'order' - The order for the contents of the folder.
-- * 'sort' - The sorting criteria.
-- * 'type'' - The type of items.
mkDescribeFolderContents ::
  -- | 'folderId'
  Lude.Text ->
  DescribeFolderContents
mkDescribeFolderContents pFolderId_ =
  DescribeFolderContents'
    { include = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      sort = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      type' = Lude.Nothing,
      order = Lude.Nothing,
      folderId = pFolderId_
    }

-- | The contents to include. Specify "INITIALIZED" to include initialized documents.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcsInclude :: Lens.Lens' DescribeFolderContents (Lude.Maybe Lude.Text)
dfcsInclude = Lens.lens (include :: DescribeFolderContents -> Lude.Maybe Lude.Text) (\s a -> s {include = a} :: DescribeFolderContents)
{-# DEPRECATED dfcsInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcsAuthenticationToken :: Lens.Lens' DescribeFolderContents (Lude.Maybe (Lude.Sensitive Lude.Text))
dfcsAuthenticationToken = Lens.lens (authenticationToken :: DescribeFolderContents -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DescribeFolderContents)
{-# DEPRECATED dfcsAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The sorting criteria.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcsSort :: Lens.Lens' DescribeFolderContents (Lude.Maybe ResourceSortType)
dfcsSort = Lens.lens (sort :: DescribeFolderContents -> Lude.Maybe ResourceSortType) (\s a -> s {sort = a} :: DescribeFolderContents)
{-# DEPRECATED dfcsSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | The marker for the next set of results. This marker was received from a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcsMarker :: Lens.Lens' DescribeFolderContents (Lude.Maybe Lude.Text)
dfcsMarker = Lens.lens (marker :: DescribeFolderContents -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeFolderContents)
{-# DEPRECATED dfcsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcsLimit :: Lens.Lens' DescribeFolderContents (Lude.Maybe Lude.Natural)
dfcsLimit = Lens.lens (limit :: DescribeFolderContents -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeFolderContents)
{-# DEPRECATED dfcsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The type of items.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcsType :: Lens.Lens' DescribeFolderContents (Lude.Maybe FolderContentType)
dfcsType = Lens.lens (type' :: DescribeFolderContents -> Lude.Maybe FolderContentType) (\s a -> s {type' = a} :: DescribeFolderContents)
{-# DEPRECATED dfcsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The order for the contents of the folder.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcsOrder :: Lens.Lens' DescribeFolderContents (Lude.Maybe OrderType)
dfcsOrder = Lens.lens (order :: DescribeFolderContents -> Lude.Maybe OrderType) (\s a -> s {order = a} :: DescribeFolderContents)
{-# DEPRECATED dfcsOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcsFolderId :: Lens.Lens' DescribeFolderContents Lude.Text
dfcsFolderId = Lens.lens (folderId :: DescribeFolderContents -> Lude.Text) (\s a -> s {folderId = a} :: DescribeFolderContents)
{-# DEPRECATED dfcsFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

instance Page.AWSPager DescribeFolderContents where
  page rq rs
    | Page.stop (rs Lens.^. dfcrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dfcrsFolders) = Lude.Nothing
    | Page.stop (rs Lens.^. dfcrsDocuments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfcsMarker Lens..~ rs Lens.^. dfcrsMarker

instance Lude.AWSRequest DescribeFolderContents where
  type Rs DescribeFolderContents = DescribeFolderContentsResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFolderContentsResponse'
            Lude.<$> (x Lude..?> "Folders" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Documents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFolderContents where
  toHeaders DescribeFolderContents' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeFolderContents where
  toPath DescribeFolderContents' {..} =
    Lude.mconcat
      ["/api/v1/folders/", Lude.toBS folderId, "/contents"]

instance Lude.ToQuery DescribeFolderContents where
  toQuery DescribeFolderContents' {..} =
    Lude.mconcat
      [ "include" Lude.=: include,
        "sort" Lude.=: sort,
        "marker" Lude.=: marker,
        "limit" Lude.=: limit,
        "type" Lude.=: type',
        "order" Lude.=: order
      ]

-- | /See:/ 'mkDescribeFolderContentsResponse' smart constructor.
data DescribeFolderContentsResponse = DescribeFolderContentsResponse'
  { folders ::
      Lude.Maybe [FolderMetadata],
    documents ::
      Lude.Maybe [DocumentMetadata],
    marker ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFolderContentsResponse' with the minimum fields required to make a request.
--
-- * 'documents' - The documents in the specified folder.
-- * 'folders' - The subfolders in the specified folder.
-- * 'marker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeFolderContentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFolderContentsResponse
mkDescribeFolderContentsResponse pResponseStatus_ =
  DescribeFolderContentsResponse'
    { folders = Lude.Nothing,
      documents = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The subfolders in the specified folder.
--
-- /Note:/ Consider using 'folders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrsFolders :: Lens.Lens' DescribeFolderContentsResponse (Lude.Maybe [FolderMetadata])
dfcrsFolders = Lens.lens (folders :: DescribeFolderContentsResponse -> Lude.Maybe [FolderMetadata]) (\s a -> s {folders = a} :: DescribeFolderContentsResponse)
{-# DEPRECATED dfcrsFolders "Use generic-lens or generic-optics with 'folders' instead." #-}

-- | The documents in the specified folder.
--
-- /Note:/ Consider using 'documents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrsDocuments :: Lens.Lens' DescribeFolderContentsResponse (Lude.Maybe [DocumentMetadata])
dfcrsDocuments = Lens.lens (documents :: DescribeFolderContentsResponse -> Lude.Maybe [DocumentMetadata]) (\s a -> s {documents = a} :: DescribeFolderContentsResponse)
{-# DEPRECATED dfcrsDocuments "Use generic-lens or generic-optics with 'documents' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrsMarker :: Lens.Lens' DescribeFolderContentsResponse (Lude.Maybe Lude.Text)
dfcrsMarker = Lens.lens (marker :: DescribeFolderContentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeFolderContentsResponse)
{-# DEPRECATED dfcrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrsResponseStatus :: Lens.Lens' DescribeFolderContentsResponse Lude.Int
dfcrsResponseStatus = Lens.lens (responseStatus :: DescribeFolderContentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFolderContentsResponse)
{-# DEPRECATED dfcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
