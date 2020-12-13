{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of resources, including folders and documents. The only @CollectionType@ supported is @SHARED_WITH_ME@ .
module Network.AWS.WorkDocs.GetResources
  ( -- * Creating a request
    GetResources (..),
    mkGetResources,

    -- ** Request lenses
    grAuthenticationToken,
    grUserId,
    grMarker,
    grLimit,
    grCollectionType,

    -- * Destructuring the response
    GetResourcesResponse (..),
    mkGetResourcesResponse,

    -- ** Response lenses
    grrsFolders,
    grrsDocuments,
    grrsMarker,
    grrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkGetResources' smart constructor.
data GetResources = GetResources'
  { -- | The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
    userId :: Lude.Maybe Lude.Text,
    -- | The marker for the next set of results. This marker was received from a previous call.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of resources to return.
    limit :: Lude.Maybe Lude.Natural,
    -- | The collection type.
    collectionType :: Lude.Maybe ResourceCollectionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResources' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'userId' - The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
-- * 'marker' - The marker for the next set of results. This marker was received from a previous call.
-- * 'limit' - The maximum number of resources to return.
-- * 'collectionType' - The collection type.
mkGetResources ::
  GetResources
mkGetResources =
  GetResources'
    { authenticationToken = Lude.Nothing,
      userId = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      collectionType = Lude.Nothing
    }

-- | The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grAuthenticationToken :: Lens.Lens' GetResources (Lude.Maybe (Lude.Sensitive Lude.Text))
grAuthenticationToken = Lens.lens (authenticationToken :: GetResources -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: GetResources)
{-# DEPRECATED grAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grUserId :: Lens.Lens' GetResources (Lude.Maybe Lude.Text)
grUserId = Lens.lens (userId :: GetResources -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: GetResources)
{-# DEPRECATED grUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The marker for the next set of results. This marker was received from a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grMarker :: Lens.Lens' GetResources (Lude.Maybe Lude.Text)
grMarker = Lens.lens (marker :: GetResources -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetResources)
{-# DEPRECATED grMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of resources to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grLimit :: Lens.Lens' GetResources (Lude.Maybe Lude.Natural)
grLimit = Lens.lens (limit :: GetResources -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetResources)
{-# DEPRECATED grLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The collection type.
--
-- /Note:/ Consider using 'collectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grCollectionType :: Lens.Lens' GetResources (Lude.Maybe ResourceCollectionType)
grCollectionType = Lens.lens (collectionType :: GetResources -> Lude.Maybe ResourceCollectionType) (\s a -> s {collectionType = a} :: GetResources)
{-# DEPRECATED grCollectionType "Use generic-lens or generic-optics with 'collectionType' instead." #-}

instance Lude.AWSRequest GetResources where
  type Rs GetResources = GetResourcesResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            Lude.<$> (x Lude..?> "Folders" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Documents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResources where
  toHeaders GetResources' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath GetResources where
  toPath = Lude.const "/api/v1/resources"

instance Lude.ToQuery GetResources where
  toQuery GetResources' {..} =
    Lude.mconcat
      [ "userId" Lude.=: userId,
        "marker" Lude.=: marker,
        "limit" Lude.=: limit,
        "collectionType" Lude.=: collectionType
      ]

-- | /See:/ 'mkGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { -- | The folders in the specified folder.
    folders :: Lude.Maybe [FolderMetadata],
    -- | The documents in the specified collection.
    documents :: Lude.Maybe [DocumentMetadata],
    -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcesResponse' with the minimum fields required to make a request.
--
-- * 'folders' - The folders in the specified folder.
-- * 'documents' - The documents in the specified collection.
-- * 'marker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
-- * 'responseStatus' - The response status code.
mkGetResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourcesResponse
mkGetResourcesResponse pResponseStatus_ =
  GetResourcesResponse'
    { folders = Lude.Nothing,
      documents = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The folders in the specified folder.
--
-- /Note:/ Consider using 'folders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsFolders :: Lens.Lens' GetResourcesResponse (Lude.Maybe [FolderMetadata])
grrsFolders = Lens.lens (folders :: GetResourcesResponse -> Lude.Maybe [FolderMetadata]) (\s a -> s {folders = a} :: GetResourcesResponse)
{-# DEPRECATED grrsFolders "Use generic-lens or generic-optics with 'folders' instead." #-}

-- | The documents in the specified collection.
--
-- /Note:/ Consider using 'documents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsDocuments :: Lens.Lens' GetResourcesResponse (Lude.Maybe [DocumentMetadata])
grrsDocuments = Lens.lens (documents :: GetResourcesResponse -> Lude.Maybe [DocumentMetadata]) (\s a -> s {documents = a} :: GetResourcesResponse)
{-# DEPRECATED grrsDocuments "Use generic-lens or generic-optics with 'documents' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsMarker :: Lens.Lens' GetResourcesResponse (Lude.Maybe Lude.Text)
grrsMarker = Lens.lens (marker :: GetResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetResourcesResponse)
{-# DEPRECATED grrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetResourcesResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourcesResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
