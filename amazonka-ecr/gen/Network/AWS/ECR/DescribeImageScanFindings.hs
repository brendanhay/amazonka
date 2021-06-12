{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DescribeImageScanFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the scan findings for the specified image.
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeImageScanFindings
  ( -- * Creating a Request
    DescribeImageScanFindings (..),
    newDescribeImageScanFindings,

    -- * Request Lenses
    describeImageScanFindings_nextToken,
    describeImageScanFindings_maxResults,
    describeImageScanFindings_registryId,
    describeImageScanFindings_repositoryName,
    describeImageScanFindings_imageId,

    -- * Destructuring the Response
    DescribeImageScanFindingsResponse (..),
    newDescribeImageScanFindingsResponse,

    -- * Response Lenses
    describeImageScanFindingsResponse_nextToken,
    describeImageScanFindingsResponse_imageScanStatus,
    describeImageScanFindingsResponse_imageScanFindings,
    describeImageScanFindingsResponse_registryId,
    describeImageScanFindingsResponse_repositoryName,
    describeImageScanFindingsResponse_imageId,
    describeImageScanFindingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImageScanFindings' smart constructor.
data DescribeImageScanFindings = DescribeImageScanFindings'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeImageScanFindings@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    -- This value is null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of image scan results returned by
    -- @DescribeImageScanFindings@ in paginated output. When this parameter is
    -- used, @DescribeImageScanFindings@ only returns @maxResults@ results in a
    -- single page along with a @nextToken@ response element. The remaining
    -- results of the initial request can be seen by sending another
    -- @DescribeImageScanFindings@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 1000. If this parameter is not used,
    -- then @DescribeImageScanFindings@ returns up to 100 results and a
    -- @nextToken@ value, if applicable.
    maxResults :: Core.Maybe Core.Natural,
    -- | The AWS account ID associated with the registry that contains the
    -- repository in which to describe the image scan findings for. If you do
    -- not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The repository for the image for which to describe the scan findings.
    repositoryName :: Core.Text,
    imageId :: ImageIdentifier
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImageScanFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImageScanFindings_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeImageScanFindings@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is null when there are no more results to return.
--
-- 'maxResults', 'describeImageScanFindings_maxResults' - The maximum number of image scan results returned by
-- @DescribeImageScanFindings@ in paginated output. When this parameter is
-- used, @DescribeImageScanFindings@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @DescribeImageScanFindings@ request with the returned @nextToken@ value.
-- This value can be between 1 and 1000. If this parameter is not used,
-- then @DescribeImageScanFindings@ returns up to 100 results and a
-- @nextToken@ value, if applicable.
--
-- 'registryId', 'describeImageScanFindings_registryId' - The AWS account ID associated with the registry that contains the
-- repository in which to describe the image scan findings for. If you do
-- not specify a registry, the default registry is assumed.
--
-- 'repositoryName', 'describeImageScanFindings_repositoryName' - The repository for the image for which to describe the scan findings.
--
-- 'imageId', 'describeImageScanFindings_imageId' - Undocumented member.
newDescribeImageScanFindings ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'imageId'
  ImageIdentifier ->
  DescribeImageScanFindings
newDescribeImageScanFindings
  pRepositoryName_
  pImageId_ =
    DescribeImageScanFindings'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        registryId = Core.Nothing,
        repositoryName = pRepositoryName_,
        imageId = pImageId_
      }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeImageScanFindings@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is null when there are no more results to return.
describeImageScanFindings_nextToken :: Lens.Lens' DescribeImageScanFindings (Core.Maybe Core.Text)
describeImageScanFindings_nextToken = Lens.lens (\DescribeImageScanFindings' {nextToken} -> nextToken) (\s@DescribeImageScanFindings' {} a -> s {nextToken = a} :: DescribeImageScanFindings)

-- | The maximum number of image scan results returned by
-- @DescribeImageScanFindings@ in paginated output. When this parameter is
-- used, @DescribeImageScanFindings@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @DescribeImageScanFindings@ request with the returned @nextToken@ value.
-- This value can be between 1 and 1000. If this parameter is not used,
-- then @DescribeImageScanFindings@ returns up to 100 results and a
-- @nextToken@ value, if applicable.
describeImageScanFindings_maxResults :: Lens.Lens' DescribeImageScanFindings (Core.Maybe Core.Natural)
describeImageScanFindings_maxResults = Lens.lens (\DescribeImageScanFindings' {maxResults} -> maxResults) (\s@DescribeImageScanFindings' {} a -> s {maxResults = a} :: DescribeImageScanFindings)

-- | The AWS account ID associated with the registry that contains the
-- repository in which to describe the image scan findings for. If you do
-- not specify a registry, the default registry is assumed.
describeImageScanFindings_registryId :: Lens.Lens' DescribeImageScanFindings (Core.Maybe Core.Text)
describeImageScanFindings_registryId = Lens.lens (\DescribeImageScanFindings' {registryId} -> registryId) (\s@DescribeImageScanFindings' {} a -> s {registryId = a} :: DescribeImageScanFindings)

-- | The repository for the image for which to describe the scan findings.
describeImageScanFindings_repositoryName :: Lens.Lens' DescribeImageScanFindings Core.Text
describeImageScanFindings_repositoryName = Lens.lens (\DescribeImageScanFindings' {repositoryName} -> repositoryName) (\s@DescribeImageScanFindings' {} a -> s {repositoryName = a} :: DescribeImageScanFindings)

-- | Undocumented member.
describeImageScanFindings_imageId :: Lens.Lens' DescribeImageScanFindings ImageIdentifier
describeImageScanFindings_imageId = Lens.lens (\DescribeImageScanFindings' {imageId} -> imageId) (\s@DescribeImageScanFindings' {} a -> s {imageId = a} :: DescribeImageScanFindings)

instance Core.AWSPager DescribeImageScanFindings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImageScanFindingsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImageScanFindingsResponse_imageScanFindings
              Core.. Lens._Just
              Core.. imageScanFindings_findings
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeImageScanFindings_nextToken
          Lens..~ rs
          Lens.^? describeImageScanFindingsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeImageScanFindings where
  type
    AWSResponse DescribeImageScanFindings =
      DescribeImageScanFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageScanFindingsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "imageScanStatus")
            Core.<*> (x Core..?> "imageScanFindings")
            Core.<*> (x Core..?> "registryId")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "imageId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImageScanFindings

instance Core.NFData DescribeImageScanFindings

instance Core.ToHeaders DescribeImageScanFindings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImageScanFindings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeImageScanFindings where
  toJSON DescribeImageScanFindings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("imageId" Core..= imageId)
          ]
      )

instance Core.ToPath DescribeImageScanFindings where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImageScanFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeImageScanFindingsResponse' smart constructor.
data DescribeImageScanFindingsResponse = DescribeImageScanFindingsResponse'
  { -- | The @nextToken@ value to include in a future @DescribeImageScanFindings@
    -- request. When the results of a @DescribeImageScanFindings@ request
    -- exceed @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The current state of the scan.
    imageScanStatus :: Core.Maybe ImageScanStatus,
    -- | The information contained in the image scan findings.
    imageScanFindings :: Core.Maybe ImageScanFindings,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    imageId :: Core.Maybe ImageIdentifier,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImageScanFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImageScanFindingsResponse_nextToken' - The @nextToken@ value to include in a future @DescribeImageScanFindings@
-- request. When the results of a @DescribeImageScanFindings@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is null when there are no more results to return.
--
-- 'imageScanStatus', 'describeImageScanFindingsResponse_imageScanStatus' - The current state of the scan.
--
-- 'imageScanFindings', 'describeImageScanFindingsResponse_imageScanFindings' - The information contained in the image scan findings.
--
-- 'registryId', 'describeImageScanFindingsResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'describeImageScanFindingsResponse_repositoryName' - The repository name associated with the request.
--
-- 'imageId', 'describeImageScanFindingsResponse_imageId' - Undocumented member.
--
-- 'httpStatus', 'describeImageScanFindingsResponse_httpStatus' - The response's http status code.
newDescribeImageScanFindingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImageScanFindingsResponse
newDescribeImageScanFindingsResponse pHttpStatus_ =
  DescribeImageScanFindingsResponse'
    { nextToken =
        Core.Nothing,
      imageScanStatus = Core.Nothing,
      imageScanFindings = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      imageId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeImageScanFindings@
-- request. When the results of a @DescribeImageScanFindings@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is null when there are no more results to return.
describeImageScanFindingsResponse_nextToken :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Core.Text)
describeImageScanFindingsResponse_nextToken = Lens.lens (\DescribeImageScanFindingsResponse' {nextToken} -> nextToken) (\s@DescribeImageScanFindingsResponse' {} a -> s {nextToken = a} :: DescribeImageScanFindingsResponse)

-- | The current state of the scan.
describeImageScanFindingsResponse_imageScanStatus :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe ImageScanStatus)
describeImageScanFindingsResponse_imageScanStatus = Lens.lens (\DescribeImageScanFindingsResponse' {imageScanStatus} -> imageScanStatus) (\s@DescribeImageScanFindingsResponse' {} a -> s {imageScanStatus = a} :: DescribeImageScanFindingsResponse)

-- | The information contained in the image scan findings.
describeImageScanFindingsResponse_imageScanFindings :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe ImageScanFindings)
describeImageScanFindingsResponse_imageScanFindings = Lens.lens (\DescribeImageScanFindingsResponse' {imageScanFindings} -> imageScanFindings) (\s@DescribeImageScanFindingsResponse' {} a -> s {imageScanFindings = a} :: DescribeImageScanFindingsResponse)

-- | The registry ID associated with the request.
describeImageScanFindingsResponse_registryId :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Core.Text)
describeImageScanFindingsResponse_registryId = Lens.lens (\DescribeImageScanFindingsResponse' {registryId} -> registryId) (\s@DescribeImageScanFindingsResponse' {} a -> s {registryId = a} :: DescribeImageScanFindingsResponse)

-- | The repository name associated with the request.
describeImageScanFindingsResponse_repositoryName :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe Core.Text)
describeImageScanFindingsResponse_repositoryName = Lens.lens (\DescribeImageScanFindingsResponse' {repositoryName} -> repositoryName) (\s@DescribeImageScanFindingsResponse' {} a -> s {repositoryName = a} :: DescribeImageScanFindingsResponse)

-- | Undocumented member.
describeImageScanFindingsResponse_imageId :: Lens.Lens' DescribeImageScanFindingsResponse (Core.Maybe ImageIdentifier)
describeImageScanFindingsResponse_imageId = Lens.lens (\DescribeImageScanFindingsResponse' {imageId} -> imageId) (\s@DescribeImageScanFindingsResponse' {} a -> s {imageId = a} :: DescribeImageScanFindingsResponse)

-- | The response's http status code.
describeImageScanFindingsResponse_httpStatus :: Lens.Lens' DescribeImageScanFindingsResponse Core.Int
describeImageScanFindingsResponse_httpStatus = Lens.lens (\DescribeImageScanFindingsResponse' {httpStatus} -> httpStatus) (\s@DescribeImageScanFindingsResponse' {} a -> s {httpStatus = a} :: DescribeImageScanFindingsResponse)

instance
  Core.NFData
    DescribeImageScanFindingsResponse
