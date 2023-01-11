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
-- Module      : Amazonka.ECR.DescribeImageScanFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the scan findings for the specified image.
--
-- This operation returns paginated results.
module Amazonka.ECR.DescribeImageScanFindings
  ( -- * Creating a Request
    DescribeImageScanFindings (..),
    newDescribeImageScanFindings,

    -- * Request Lenses
    describeImageScanFindings_maxResults,
    describeImageScanFindings_nextToken,
    describeImageScanFindings_registryId,
    describeImageScanFindings_repositoryName,
    describeImageScanFindings_imageId,

    -- * Destructuring the Response
    DescribeImageScanFindingsResponse (..),
    newDescribeImageScanFindingsResponse,

    -- * Response Lenses
    describeImageScanFindingsResponse_imageId,
    describeImageScanFindingsResponse_imageScanFindings,
    describeImageScanFindingsResponse_imageScanStatus,
    describeImageScanFindingsResponse_nextToken,
    describeImageScanFindingsResponse_registryId,
    describeImageScanFindingsResponse_repositoryName,
    describeImageScanFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImageScanFindings' smart constructor.
data DescribeImageScanFindings = DescribeImageScanFindings'
  { -- | The maximum number of image scan results returned by
    -- @DescribeImageScanFindings@ in paginated output. When this parameter is
    -- used, @DescribeImageScanFindings@ only returns @maxResults@ results in a
    -- single page along with a @nextToken@ response element. The remaining
    -- results of the initial request can be seen by sending another
    -- @DescribeImageScanFindings@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 1000. If this parameter is not used,
    -- then @DescribeImageScanFindings@ returns up to 100 results and a
    -- @nextToken@ value, if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeImageScanFindings@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    -- This value is null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository in which to describe the image scan findings
    -- for. If you do not specify a registry, the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository for the image for which to describe the scan findings.
    repositoryName :: Prelude.Text,
    imageId :: ImageIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageScanFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'nextToken', 'describeImageScanFindings_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeImageScanFindings@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is null when there are no more results to return.
--
-- 'registryId', 'describeImageScanFindings_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository in which to describe the image scan findings
-- for. If you do not specify a registry, the default registry is assumed.
--
-- 'repositoryName', 'describeImageScanFindings_repositoryName' - The repository for the image for which to describe the scan findings.
--
-- 'imageId', 'describeImageScanFindings_imageId' - Undocumented member.
newDescribeImageScanFindings ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'imageId'
  ImageIdentifier ->
  DescribeImageScanFindings
newDescribeImageScanFindings
  pRepositoryName_
  pImageId_ =
    DescribeImageScanFindings'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        registryId = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        imageId = pImageId_
      }

-- | The maximum number of image scan results returned by
-- @DescribeImageScanFindings@ in paginated output. When this parameter is
-- used, @DescribeImageScanFindings@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @DescribeImageScanFindings@ request with the returned @nextToken@ value.
-- This value can be between 1 and 1000. If this parameter is not used,
-- then @DescribeImageScanFindings@ returns up to 100 results and a
-- @nextToken@ value, if applicable.
describeImageScanFindings_maxResults :: Lens.Lens' DescribeImageScanFindings (Prelude.Maybe Prelude.Natural)
describeImageScanFindings_maxResults = Lens.lens (\DescribeImageScanFindings' {maxResults} -> maxResults) (\s@DescribeImageScanFindings' {} a -> s {maxResults = a} :: DescribeImageScanFindings)

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeImageScanFindings@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is null when there are no more results to return.
describeImageScanFindings_nextToken :: Lens.Lens' DescribeImageScanFindings (Prelude.Maybe Prelude.Text)
describeImageScanFindings_nextToken = Lens.lens (\DescribeImageScanFindings' {nextToken} -> nextToken) (\s@DescribeImageScanFindings' {} a -> s {nextToken = a} :: DescribeImageScanFindings)

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository in which to describe the image scan findings
-- for. If you do not specify a registry, the default registry is assumed.
describeImageScanFindings_registryId :: Lens.Lens' DescribeImageScanFindings (Prelude.Maybe Prelude.Text)
describeImageScanFindings_registryId = Lens.lens (\DescribeImageScanFindings' {registryId} -> registryId) (\s@DescribeImageScanFindings' {} a -> s {registryId = a} :: DescribeImageScanFindings)

-- | The repository for the image for which to describe the scan findings.
describeImageScanFindings_repositoryName :: Lens.Lens' DescribeImageScanFindings Prelude.Text
describeImageScanFindings_repositoryName = Lens.lens (\DescribeImageScanFindings' {repositoryName} -> repositoryName) (\s@DescribeImageScanFindings' {} a -> s {repositoryName = a} :: DescribeImageScanFindings)

-- | Undocumented member.
describeImageScanFindings_imageId :: Lens.Lens' DescribeImageScanFindings ImageIdentifier
describeImageScanFindings_imageId = Lens.lens (\DescribeImageScanFindings' {imageId} -> imageId) (\s@DescribeImageScanFindings' {} a -> s {imageId = a} :: DescribeImageScanFindings)

instance Core.AWSPager DescribeImageScanFindings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImageScanFindingsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImageScanFindingsResponse_imageScanFindings
              Prelude.. Lens._Just
              Prelude.. imageScanFindings_findings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImageScanFindingsResponse_imageScanFindings
              Prelude.. Lens._Just
              Prelude.. imageScanFindings_enhancedFindings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeImageScanFindings_nextToken
          Lens..~ rs
          Lens.^? describeImageScanFindingsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeImageScanFindings where
  type
    AWSResponse DescribeImageScanFindings =
      DescribeImageScanFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageScanFindingsResponse'
            Prelude.<$> (x Data..?> "imageId")
            Prelude.<*> (x Data..?> "imageScanFindings")
            Prelude.<*> (x Data..?> "imageScanStatus")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImageScanFindings where
  hashWithSalt _salt DescribeImageScanFindings' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData DescribeImageScanFindings where
  rnf DescribeImageScanFindings' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders DescribeImageScanFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImageScanFindings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeImageScanFindings where
  toJSON DescribeImageScanFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("imageId" Data..= imageId)
          ]
      )

instance Data.ToPath DescribeImageScanFindings where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImageScanFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageScanFindingsResponse' smart constructor.
data DescribeImageScanFindingsResponse = DescribeImageScanFindingsResponse'
  { imageId :: Prelude.Maybe ImageIdentifier,
    -- | The information contained in the image scan findings.
    imageScanFindings :: Prelude.Maybe ImageScanFindings,
    -- | The current state of the scan.
    imageScanStatus :: Prelude.Maybe ImageScanStatus,
    -- | The @nextToken@ value to include in a future @DescribeImageScanFindings@
    -- request. When the results of a @DescribeImageScanFindings@ request
    -- exceed @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageScanFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'describeImageScanFindingsResponse_imageId' - Undocumented member.
--
-- 'imageScanFindings', 'describeImageScanFindingsResponse_imageScanFindings' - The information contained in the image scan findings.
--
-- 'imageScanStatus', 'describeImageScanFindingsResponse_imageScanStatus' - The current state of the scan.
--
-- 'nextToken', 'describeImageScanFindingsResponse_nextToken' - The @nextToken@ value to include in a future @DescribeImageScanFindings@
-- request. When the results of a @DescribeImageScanFindings@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is null when there are no more results to return.
--
-- 'registryId', 'describeImageScanFindingsResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'describeImageScanFindingsResponse_repositoryName' - The repository name associated with the request.
--
-- 'httpStatus', 'describeImageScanFindingsResponse_httpStatus' - The response's http status code.
newDescribeImageScanFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageScanFindingsResponse
newDescribeImageScanFindingsResponse pHttpStatus_ =
  DescribeImageScanFindingsResponse'
    { imageId =
        Prelude.Nothing,
      imageScanFindings = Prelude.Nothing,
      imageScanStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeImageScanFindingsResponse_imageId :: Lens.Lens' DescribeImageScanFindingsResponse (Prelude.Maybe ImageIdentifier)
describeImageScanFindingsResponse_imageId = Lens.lens (\DescribeImageScanFindingsResponse' {imageId} -> imageId) (\s@DescribeImageScanFindingsResponse' {} a -> s {imageId = a} :: DescribeImageScanFindingsResponse)

-- | The information contained in the image scan findings.
describeImageScanFindingsResponse_imageScanFindings :: Lens.Lens' DescribeImageScanFindingsResponse (Prelude.Maybe ImageScanFindings)
describeImageScanFindingsResponse_imageScanFindings = Lens.lens (\DescribeImageScanFindingsResponse' {imageScanFindings} -> imageScanFindings) (\s@DescribeImageScanFindingsResponse' {} a -> s {imageScanFindings = a} :: DescribeImageScanFindingsResponse)

-- | The current state of the scan.
describeImageScanFindingsResponse_imageScanStatus :: Lens.Lens' DescribeImageScanFindingsResponse (Prelude.Maybe ImageScanStatus)
describeImageScanFindingsResponse_imageScanStatus = Lens.lens (\DescribeImageScanFindingsResponse' {imageScanStatus} -> imageScanStatus) (\s@DescribeImageScanFindingsResponse' {} a -> s {imageScanStatus = a} :: DescribeImageScanFindingsResponse)

-- | The @nextToken@ value to include in a future @DescribeImageScanFindings@
-- request. When the results of a @DescribeImageScanFindings@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is null when there are no more results to return.
describeImageScanFindingsResponse_nextToken :: Lens.Lens' DescribeImageScanFindingsResponse (Prelude.Maybe Prelude.Text)
describeImageScanFindingsResponse_nextToken = Lens.lens (\DescribeImageScanFindingsResponse' {nextToken} -> nextToken) (\s@DescribeImageScanFindingsResponse' {} a -> s {nextToken = a} :: DescribeImageScanFindingsResponse)

-- | The registry ID associated with the request.
describeImageScanFindingsResponse_registryId :: Lens.Lens' DescribeImageScanFindingsResponse (Prelude.Maybe Prelude.Text)
describeImageScanFindingsResponse_registryId = Lens.lens (\DescribeImageScanFindingsResponse' {registryId} -> registryId) (\s@DescribeImageScanFindingsResponse' {} a -> s {registryId = a} :: DescribeImageScanFindingsResponse)

-- | The repository name associated with the request.
describeImageScanFindingsResponse_repositoryName :: Lens.Lens' DescribeImageScanFindingsResponse (Prelude.Maybe Prelude.Text)
describeImageScanFindingsResponse_repositoryName = Lens.lens (\DescribeImageScanFindingsResponse' {repositoryName} -> repositoryName) (\s@DescribeImageScanFindingsResponse' {} a -> s {repositoryName = a} :: DescribeImageScanFindingsResponse)

-- | The response's http status code.
describeImageScanFindingsResponse_httpStatus :: Lens.Lens' DescribeImageScanFindingsResponse Prelude.Int
describeImageScanFindingsResponse_httpStatus = Lens.lens (\DescribeImageScanFindingsResponse' {httpStatus} -> httpStatus) (\s@DescribeImageScanFindingsResponse' {} a -> s {httpStatus = a} :: DescribeImageScanFindingsResponse)

instance
  Prelude.NFData
    DescribeImageScanFindingsResponse
  where
  rnf DescribeImageScanFindingsResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf imageScanFindings
      `Prelude.seq` Prelude.rnf imageScanStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf httpStatus
