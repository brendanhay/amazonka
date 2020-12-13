{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DescribeImageScanFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the scan findings for the specified image.
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeImageScanFindings
  ( -- * Creating a request
    DescribeImageScanFindings (..),
    mkDescribeImageScanFindings,

    -- ** Request lenses
    disfRegistryId,
    disfNextToken,
    disfImageId,
    disfRepositoryName,
    disfMaxResults,

    -- * Destructuring the response
    DescribeImageScanFindingsResponse (..),
    mkDescribeImageScanFindingsResponse,

    -- ** Response lenses
    disfrsRegistryId,
    disfrsImageScanFindings,
    disfrsImageScanStatus,
    disfrsNextToken,
    disfrsImageId,
    disfrsRepositoryName,
    disfrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImageScanFindings' smart constructor.
data DescribeImageScanFindings = DescribeImageScanFindings'
  { -- | The AWS account ID associated with the registry that contains the repository in which to describe the image scan findings for. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The @nextToken@ value returned from a previous paginated @DescribeImageScanFindings@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    imageId :: ImageIdentifier,
    -- | The repository for the image for which to describe the scan findings.
    repositoryName :: Lude.Text,
    -- | The maximum number of image scan results returned by @DescribeImageScanFindings@ in paginated output. When this parameter is used, @DescribeImageScanFindings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImageScanFindings@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImageScanFindings@ returns up to 100 results and a @nextToken@ value, if applicable.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageScanFindings' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository in which to describe the image scan findings for. If you do not specify a registry, the default registry is assumed.
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @DescribeImageScanFindings@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
-- * 'imageId' -
-- * 'repositoryName' - The repository for the image for which to describe the scan findings.
-- * 'maxResults' - The maximum number of image scan results returned by @DescribeImageScanFindings@ in paginated output. When this parameter is used, @DescribeImageScanFindings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImageScanFindings@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImageScanFindings@ returns up to 100 results and a @nextToken@ value, if applicable.
mkDescribeImageScanFindings ::
  -- | 'imageId'
  ImageIdentifier ->
  -- | 'repositoryName'
  Lude.Text ->
  DescribeImageScanFindings
mkDescribeImageScanFindings pImageId_ pRepositoryName_ =
  DescribeImageScanFindings'
    { registryId = Lude.Nothing,
      nextToken = Lude.Nothing,
      imageId = pImageId_,
      repositoryName = pRepositoryName_,
      maxResults = Lude.Nothing
    }

-- | The AWS account ID associated with the registry that contains the repository in which to describe the image scan findings for. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfRegistryId :: Lens.Lens' DescribeImageScanFindings (Lude.Maybe Lude.Text)
disfRegistryId = Lens.lens (registryId :: DescribeImageScanFindings -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DescribeImageScanFindings)
{-# DEPRECATED disfRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeImageScanFindings@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfNextToken :: Lens.Lens' DescribeImageScanFindings (Lude.Maybe Lude.Text)
disfNextToken = Lens.lens (nextToken :: DescribeImageScanFindings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImageScanFindings)
{-# DEPRECATED disfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfImageId :: Lens.Lens' DescribeImageScanFindings ImageIdentifier
disfImageId = Lens.lens (imageId :: DescribeImageScanFindings -> ImageIdentifier) (\s a -> s {imageId = a} :: DescribeImageScanFindings)
{-# DEPRECATED disfImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The repository for the image for which to describe the scan findings.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfRepositoryName :: Lens.Lens' DescribeImageScanFindings Lude.Text
disfRepositoryName = Lens.lens (repositoryName :: DescribeImageScanFindings -> Lude.Text) (\s a -> s {repositoryName = a} :: DescribeImageScanFindings)
{-# DEPRECATED disfRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The maximum number of image scan results returned by @DescribeImageScanFindings@ in paginated output. When this parameter is used, @DescribeImageScanFindings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImageScanFindings@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeImageScanFindings@ returns up to 100 results and a @nextToken@ value, if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfMaxResults :: Lens.Lens' DescribeImageScanFindings (Lude.Maybe Lude.Natural)
disfMaxResults = Lens.lens (maxResults :: DescribeImageScanFindings -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeImageScanFindings)
{-# DEPRECATED disfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeImageScanFindings where
  page rq rs
    | Page.stop (rs Lens.^. disfrsNextToken) = Lude.Nothing
    | Page.stop
        ( rs
            Lens.^? disfrsImageScanFindings Lude.. Lens._Just
              Lude.. isfFindings
              Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& disfNextToken Lens..~ rs Lens.^. disfrsNextToken

instance Lude.AWSRequest DescribeImageScanFindings where
  type
    Rs DescribeImageScanFindings =
      DescribeImageScanFindingsResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeImageScanFindingsResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "imageScanFindings")
            Lude.<*> (x Lude..?> "imageScanStatus")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "imageId")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImageScanFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImageScanFindings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeImageScanFindings where
  toJSON DescribeImageScanFindings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("imageId" Lude..= imageId),
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeImageScanFindings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImageScanFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeImageScanFindingsResponse' smart constructor.
data DescribeImageScanFindingsResponse = DescribeImageScanFindingsResponse'
  { -- | The registry ID associated with the request.
    registryId :: Lude.Maybe Lude.Text,
    -- | The information contained in the image scan findings.
    imageScanFindings :: Lude.Maybe ImageScanFindings,
    -- | The current state of the scan.
    imageScanStatus :: Lude.Maybe ImageScanStatus,
    -- | The @nextToken@ value to include in a future @DescribeImageScanFindings@ request. When the results of a @DescribeImageScanFindings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    imageId :: Lude.Maybe ImageIdentifier,
    -- | The repository name associated with the request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageScanFindingsResponse' with the minimum fields required to make a request.
--
-- * 'registryId' - The registry ID associated with the request.
-- * 'imageScanFindings' - The information contained in the image scan findings.
-- * 'imageScanStatus' - The current state of the scan.
-- * 'nextToken' - The @nextToken@ value to include in a future @DescribeImageScanFindings@ request. When the results of a @DescribeImageScanFindings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
-- * 'imageId' -
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
mkDescribeImageScanFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImageScanFindingsResponse
mkDescribeImageScanFindingsResponse pResponseStatus_ =
  DescribeImageScanFindingsResponse'
    { registryId = Lude.Nothing,
      imageScanFindings = Lude.Nothing,
      imageScanStatus = Lude.Nothing,
      nextToken = Lude.Nothing,
      imageId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrsRegistryId :: Lens.Lens' DescribeImageScanFindingsResponse (Lude.Maybe Lude.Text)
disfrsRegistryId = Lens.lens (registryId :: DescribeImageScanFindingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DescribeImageScanFindingsResponse)
{-# DEPRECATED disfrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The information contained in the image scan findings.
--
-- /Note:/ Consider using 'imageScanFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrsImageScanFindings :: Lens.Lens' DescribeImageScanFindingsResponse (Lude.Maybe ImageScanFindings)
disfrsImageScanFindings = Lens.lens (imageScanFindings :: DescribeImageScanFindingsResponse -> Lude.Maybe ImageScanFindings) (\s a -> s {imageScanFindings = a} :: DescribeImageScanFindingsResponse)
{-# DEPRECATED disfrsImageScanFindings "Use generic-lens or generic-optics with 'imageScanFindings' instead." #-}

-- | The current state of the scan.
--
-- /Note:/ Consider using 'imageScanStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrsImageScanStatus :: Lens.Lens' DescribeImageScanFindingsResponse (Lude.Maybe ImageScanStatus)
disfrsImageScanStatus = Lens.lens (imageScanStatus :: DescribeImageScanFindingsResponse -> Lude.Maybe ImageScanStatus) (\s a -> s {imageScanStatus = a} :: DescribeImageScanFindingsResponse)
{-# DEPRECATED disfrsImageScanStatus "Use generic-lens or generic-optics with 'imageScanStatus' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeImageScanFindings@ request. When the results of a @DescribeImageScanFindings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrsNextToken :: Lens.Lens' DescribeImageScanFindingsResponse (Lude.Maybe Lude.Text)
disfrsNextToken = Lens.lens (nextToken :: DescribeImageScanFindingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImageScanFindingsResponse)
{-# DEPRECATED disfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrsImageId :: Lens.Lens' DescribeImageScanFindingsResponse (Lude.Maybe ImageIdentifier)
disfrsImageId = Lens.lens (imageId :: DescribeImageScanFindingsResponse -> Lude.Maybe ImageIdentifier) (\s a -> s {imageId = a} :: DescribeImageScanFindingsResponse)
{-# DEPRECATED disfrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrsRepositoryName :: Lens.Lens' DescribeImageScanFindingsResponse (Lude.Maybe Lude.Text)
disfrsRepositoryName = Lens.lens (repositoryName :: DescribeImageScanFindingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: DescribeImageScanFindingsResponse)
{-# DEPRECATED disfrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disfrsResponseStatus :: Lens.Lens' DescribeImageScanFindingsResponse Lude.Int
disfrsResponseStatus = Lens.lens (responseStatus :: DescribeImageScanFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImageScanFindingsResponse)
{-# DEPRECATED disfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
