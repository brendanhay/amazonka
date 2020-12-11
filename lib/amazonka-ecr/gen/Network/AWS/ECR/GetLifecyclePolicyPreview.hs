{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetLifecyclePolicyPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the results of the lifecycle policy preview request for the specified repository.
--
-- This operation returns paginated results.
module Network.AWS.ECR.GetLifecyclePolicyPreview
  ( -- * Creating a request
    GetLifecyclePolicyPreview (..),
    mkGetLifecyclePolicyPreview,

    -- ** Request lenses
    glppRegistryId,
    glppImageIds,
    glppNextToken,
    glppFilter,
    glppMaxResults,
    glppRepositoryName,

    -- * Destructuring the response
    GetLifecyclePolicyPreviewResponse (..),
    mkGetLifecyclePolicyPreviewResponse,

    -- ** Response lenses
    glpprsSummary,
    glpprsStatus,
    glpprsRegistryId,
    glpprsLifecyclePolicyText,
    glpprsNextToken,
    glpprsRepositoryName,
    glpprsPreviewResults,
    glpprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLifecyclePolicyPreview' smart constructor.
data GetLifecyclePolicyPreview = GetLifecyclePolicyPreview'
  { registryId ::
      Lude.Maybe Lude.Text,
    imageIds ::
      Lude.Maybe [ImageIdentifier],
    nextToken :: Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe LifecyclePolicyPreviewFilter,
    maxResults :: Lude.Maybe Lude.Natural,
    repositoryName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLifecyclePolicyPreview' with the minimum fields required to make a request.
--
-- * 'filter' - An optional parameter that filters results based on image tag status and all tags, if tagged.
-- * 'imageIds' - The list of imageIDs to be included.
-- * 'maxResults' - The maximum number of repository results returned by @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns  @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending  another @GetLifecyclePolicyPreviewRequest@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@ returns up to  100 results and a @nextToken@ value, if  applicable. This option cannot be used when you specify images with @imageIds@ .
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated  @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used and the  results exceeded the value of that parameter. Pagination continues from the end of the  previous results that returned the @nextToken@ value. This value is  @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository.
mkGetLifecyclePolicyPreview ::
  -- | 'repositoryName'
  Lude.Text ->
  GetLifecyclePolicyPreview
mkGetLifecyclePolicyPreview pRepositoryName_ =
  GetLifecyclePolicyPreview'
    { registryId = Lude.Nothing,
      imageIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppRegistryId :: Lens.Lens' GetLifecyclePolicyPreview (Lude.Maybe Lude.Text)
glppRegistryId = Lens.lens (registryId :: GetLifecyclePolicyPreview -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: GetLifecyclePolicyPreview)
{-# DEPRECATED glppRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The list of imageIDs to be included.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppImageIds :: Lens.Lens' GetLifecyclePolicyPreview (Lude.Maybe [ImageIdentifier])
glppImageIds = Lens.lens (imageIds :: GetLifecyclePolicyPreview -> Lude.Maybe [ImageIdentifier]) (\s a -> s {imageIds = a} :: GetLifecyclePolicyPreview)
{-# DEPRECATED glppImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The @nextToken@ value returned from a previous paginated  @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used and the  results exceeded the value of that parameter. Pagination continues from the end of the  previous results that returned the @nextToken@ value. This value is  @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppNextToken :: Lens.Lens' GetLifecyclePolicyPreview (Lude.Maybe Lude.Text)
glppNextToken = Lens.lens (nextToken :: GetLifecyclePolicyPreview -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetLifecyclePolicyPreview)
{-# DEPRECATED glppNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An optional parameter that filters results based on image tag status and all tags, if tagged.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppFilter :: Lens.Lens' GetLifecyclePolicyPreview (Lude.Maybe LifecyclePolicyPreviewFilter)
glppFilter = Lens.lens (filter :: GetLifecyclePolicyPreview -> Lude.Maybe LifecyclePolicyPreviewFilter) (\s a -> s {filter = a} :: GetLifecyclePolicyPreview)
{-# DEPRECATED glppFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of repository results returned by @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns  @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending  another @GetLifecyclePolicyPreviewRequest@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@ returns up to  100 results and a @nextToken@ value, if  applicable. This option cannot be used when you specify images with @imageIds@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppMaxResults :: Lens.Lens' GetLifecyclePolicyPreview (Lude.Maybe Lude.Natural)
glppMaxResults = Lens.lens (maxResults :: GetLifecyclePolicyPreview -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetLifecyclePolicyPreview)
{-# DEPRECATED glppMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glppRepositoryName :: Lens.Lens' GetLifecyclePolicyPreview Lude.Text
glppRepositoryName = Lens.lens (repositoryName :: GetLifecyclePolicyPreview -> Lude.Text) (\s a -> s {repositoryName = a} :: GetLifecyclePolicyPreview)
{-# DEPRECATED glppRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Page.AWSPager GetLifecyclePolicyPreview where
  page rq rs
    | Page.stop (rs Lens.^. glpprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. glpprsPreviewResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& glppNextToken Lens..~ rs Lens.^. glpprsNextToken

instance Lude.AWSRequest GetLifecyclePolicyPreview where
  type
    Rs GetLifecyclePolicyPreview =
      GetLifecyclePolicyPreviewResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyPreviewResponse'
            Lude.<$> (x Lude..?> "summary")
            Lude.<*> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "lifecyclePolicyText")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "previewResults" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLifecyclePolicyPreview where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicyPreview" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLifecyclePolicyPreview where
  toJSON GetLifecyclePolicyPreview' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            ("imageIds" Lude..=) Lude.<$> imageIds,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath GetLifecyclePolicyPreview where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLifecyclePolicyPreview where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLifecyclePolicyPreviewResponse' smart constructor.
data GetLifecyclePolicyPreviewResponse = GetLifecyclePolicyPreviewResponse'
  { summary ::
      Lude.Maybe
        LifecyclePolicyPreviewSummary,
    status ::
      Lude.Maybe
        LifecyclePolicyPreviewStatus,
    registryId ::
      Lude.Maybe Lude.Text,
    lifecyclePolicyText ::
      Lude.Maybe Lude.Text,
    nextToken ::
      Lude.Maybe Lude.Text,
    repositoryName ::
      Lude.Maybe Lude.Text,
    previewResults ::
      Lude.Maybe
        [LifecyclePolicyPreviewResult],
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

-- | Creates a value of 'GetLifecyclePolicyPreviewResponse' with the minimum fields required to make a request.
--
-- * 'lifecyclePolicyText' - The JSON lifecycle policy text.
-- * 'nextToken' - The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@ request. When the results of a @GetLifecyclePolicyPreview@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'previewResults' - The results of the lifecycle policy preview request.
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the lifecycle policy preview request.
-- * 'summary' - The list of images that is returned as a result of the action.
mkGetLifecyclePolicyPreviewResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLifecyclePolicyPreviewResponse
mkGetLifecyclePolicyPreviewResponse pResponseStatus_ =
  GetLifecyclePolicyPreviewResponse'
    { summary = Lude.Nothing,
      status = Lude.Nothing,
      registryId = Lude.Nothing,
      lifecyclePolicyText = Lude.Nothing,
      nextToken = Lude.Nothing,
      repositoryName = Lude.Nothing,
      previewResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of images that is returned as a result of the action.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprsSummary :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Lude.Maybe LifecyclePolicyPreviewSummary)
glpprsSummary = Lens.lens (summary :: GetLifecyclePolicyPreviewResponse -> Lude.Maybe LifecyclePolicyPreviewSummary) (\s a -> s {summary = a} :: GetLifecyclePolicyPreviewResponse)
{-# DEPRECATED glpprsSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The status of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprsStatus :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Lude.Maybe LifecyclePolicyPreviewStatus)
glpprsStatus = Lens.lens (status :: GetLifecyclePolicyPreviewResponse -> Lude.Maybe LifecyclePolicyPreviewStatus) (\s a -> s {status = a} :: GetLifecyclePolicyPreviewResponse)
{-# DEPRECATED glpprsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprsRegistryId :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Lude.Maybe Lude.Text)
glpprsRegistryId = Lens.lens (registryId :: GetLifecyclePolicyPreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: GetLifecyclePolicyPreviewResponse)
{-# DEPRECATED glpprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The JSON lifecycle policy text.
--
-- /Note:/ Consider using 'lifecyclePolicyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprsLifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Lude.Maybe Lude.Text)
glpprsLifecyclePolicyText = Lens.lens (lifecyclePolicyText :: GetLifecyclePolicyPreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {lifecyclePolicyText = a} :: GetLifecyclePolicyPreviewResponse)
{-# DEPRECATED glpprsLifecyclePolicyText "Use generic-lens or generic-optics with 'lifecyclePolicyText' instead." #-}

-- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@ request. When the results of a @GetLifecyclePolicyPreview@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprsNextToken :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Lude.Maybe Lude.Text)
glpprsNextToken = Lens.lens (nextToken :: GetLifecyclePolicyPreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetLifecyclePolicyPreviewResponse)
{-# DEPRECATED glpprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprsRepositoryName :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Lude.Maybe Lude.Text)
glpprsRepositoryName = Lens.lens (repositoryName :: GetLifecyclePolicyPreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: GetLifecyclePolicyPreviewResponse)
{-# DEPRECATED glpprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The results of the lifecycle policy preview request.
--
-- /Note:/ Consider using 'previewResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprsPreviewResults :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Lude.Maybe [LifecyclePolicyPreviewResult])
glpprsPreviewResults = Lens.lens (previewResults :: GetLifecyclePolicyPreviewResponse -> Lude.Maybe [LifecyclePolicyPreviewResult]) (\s a -> s {previewResults = a} :: GetLifecyclePolicyPreviewResponse)
{-# DEPRECATED glpprsPreviewResults "Use generic-lens or generic-optics with 'previewResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpprsResponseStatus :: Lens.Lens' GetLifecyclePolicyPreviewResponse Lude.Int
glpprsResponseStatus = Lens.lens (responseStatus :: GetLifecyclePolicyPreviewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLifecyclePolicyPreviewResponse)
{-# DEPRECATED glpprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
