{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListInstanceStorageConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of storage configs for the identified instance and resource type.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstanceStorageConfigs
  ( -- * Creating a request
    ListInstanceStorageConfigs (..),
    mkListInstanceStorageConfigs,

    -- ** Request lenses
    liscNextToken,
    liscMaxResults,
    liscInstanceId,
    liscResourceType,

    -- * Destructuring the response
    ListInstanceStorageConfigsResponse (..),
    mkListInstanceStorageConfigsResponse,

    -- ** Response lenses
    liscrsStorageConfigs,
    liscrsNextToken,
    liscrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListInstanceStorageConfigs' smart constructor.
data ListInstanceStorageConfigs = ListInstanceStorageConfigs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text,
    resourceType ::
      InstanceStorageResourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceStorageConfigs' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'resourceType' - A valid resource type.
mkListInstanceStorageConfigs ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  ListInstanceStorageConfigs
mkListInstanceStorageConfigs pInstanceId_ pResourceType_ =
  ListInstanceStorageConfigs'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_,
      resourceType = pResourceType_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscNextToken :: Lens.Lens' ListInstanceStorageConfigs (Lude.Maybe Lude.Text)
liscNextToken = Lens.lens (nextToken :: ListInstanceStorageConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstanceStorageConfigs)
{-# DEPRECATED liscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscMaxResults :: Lens.Lens' ListInstanceStorageConfigs (Lude.Maybe Lude.Natural)
liscMaxResults = Lens.lens (maxResults :: ListInstanceStorageConfigs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInstanceStorageConfigs)
{-# DEPRECATED liscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscInstanceId :: Lens.Lens' ListInstanceStorageConfigs Lude.Text
liscInstanceId = Lens.lens (instanceId :: ListInstanceStorageConfigs -> Lude.Text) (\s a -> s {instanceId = a} :: ListInstanceStorageConfigs)
{-# DEPRECATED liscInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscResourceType :: Lens.Lens' ListInstanceStorageConfigs InstanceStorageResourceType
liscResourceType = Lens.lens (resourceType :: ListInstanceStorageConfigs -> InstanceStorageResourceType) (\s a -> s {resourceType = a} :: ListInstanceStorageConfigs)
{-# DEPRECATED liscResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Page.AWSPager ListInstanceStorageConfigs where
  page rq rs
    | Page.stop (rs Lens.^. liscrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. liscrsStorageConfigs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liscNextToken Lens..~ rs Lens.^. liscrsNextToken

instance Lude.AWSRequest ListInstanceStorageConfigs where
  type
    Rs ListInstanceStorageConfigs =
      ListInstanceStorageConfigsResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInstanceStorageConfigsResponse'
            Lude.<$> (x Lude..?> "StorageConfigs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstanceStorageConfigs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListInstanceStorageConfigs where
  toPath ListInstanceStorageConfigs' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/storage-configs"]

instance Lude.ToQuery ListInstanceStorageConfigs where
  toQuery ListInstanceStorageConfigs' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults,
        "resourceType" Lude.=: resourceType
      ]

-- | /See:/ 'mkListInstanceStorageConfigsResponse' smart constructor.
data ListInstanceStorageConfigsResponse = ListInstanceStorageConfigsResponse'
  { storageConfigs ::
      Lude.Maybe
        [InstanceStorageConfig],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceStorageConfigsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'storageConfigs' - A valid storage type.
mkListInstanceStorageConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstanceStorageConfigsResponse
mkListInstanceStorageConfigsResponse pResponseStatus_ =
  ListInstanceStorageConfigsResponse'
    { storageConfigs =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscrsStorageConfigs :: Lens.Lens' ListInstanceStorageConfigsResponse (Lude.Maybe [InstanceStorageConfig])
liscrsStorageConfigs = Lens.lens (storageConfigs :: ListInstanceStorageConfigsResponse -> Lude.Maybe [InstanceStorageConfig]) (\s a -> s {storageConfigs = a} :: ListInstanceStorageConfigsResponse)
{-# DEPRECATED liscrsStorageConfigs "Use generic-lens or generic-optics with 'storageConfigs' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscrsNextToken :: Lens.Lens' ListInstanceStorageConfigsResponse (Lude.Maybe Lude.Text)
liscrsNextToken = Lens.lens (nextToken :: ListInstanceStorageConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstanceStorageConfigsResponse)
{-# DEPRECATED liscrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscrsResponseStatus :: Lens.Lens' ListInstanceStorageConfigsResponse Lude.Int
liscrsResponseStatus = Lens.lens (responseStatus :: ListInstanceStorageConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstanceStorageConfigsResponse)
{-# DEPRECATED liscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
