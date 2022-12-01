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
-- Module      : Amazonka.EC2.ListImagesInRecycleBin
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists one or more AMIs that are currently in the Recycle Bin. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/recycle-bin.html Recycle Bin>
-- in the Amazon Elastic Compute Cloud User Guide.
--
-- This operation returns paginated results.
module Amazonka.EC2.ListImagesInRecycleBin
  ( -- * Creating a Request
    ListImagesInRecycleBin (..),
    newListImagesInRecycleBin,

    -- * Request Lenses
    listImagesInRecycleBin_nextToken,
    listImagesInRecycleBin_imageIds,
    listImagesInRecycleBin_dryRun,
    listImagesInRecycleBin_maxResults,

    -- * Destructuring the Response
    ListImagesInRecycleBinResponse (..),
    newListImagesInRecycleBinResponse,

    -- * Response Lenses
    listImagesInRecycleBinResponse_nextToken,
    listImagesInRecycleBinResponse_images,
    listImagesInRecycleBinResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImagesInRecycleBin' smart constructor.
data ListImagesInRecycleBin = ListImagesInRecycleBin'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the AMIs to list. Omit this parameter to list all of the AMIs
    -- that are in the Recycle Bin. You can specify up to 20 IDs in a single
    -- request.
    imageIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If you do not specify a value for /MaxResults/, the request returns
    -- 1,000 items per page by default. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagesInRecycleBin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImagesInRecycleBin_nextToken' - The token for the next page of results.
--
-- 'imageIds', 'listImagesInRecycleBin_imageIds' - The IDs of the AMIs to list. Omit this parameter to list all of the AMIs
-- that are in the Recycle Bin. You can specify up to 20 IDs in a single
-- request.
--
-- 'dryRun', 'listImagesInRecycleBin_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'listImagesInRecycleBin_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If you do not specify a value for /MaxResults/, the request returns
-- 1,000 items per page by default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
newListImagesInRecycleBin ::
  ListImagesInRecycleBin
newListImagesInRecycleBin =
  ListImagesInRecycleBin'
    { nextToken =
        Prelude.Nothing,
      imageIds = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
listImagesInRecycleBin_nextToken :: Lens.Lens' ListImagesInRecycleBin (Prelude.Maybe Prelude.Text)
listImagesInRecycleBin_nextToken = Lens.lens (\ListImagesInRecycleBin' {nextToken} -> nextToken) (\s@ListImagesInRecycleBin' {} a -> s {nextToken = a} :: ListImagesInRecycleBin)

-- | The IDs of the AMIs to list. Omit this parameter to list all of the AMIs
-- that are in the Recycle Bin. You can specify up to 20 IDs in a single
-- request.
listImagesInRecycleBin_imageIds :: Lens.Lens' ListImagesInRecycleBin (Prelude.Maybe [Prelude.Text])
listImagesInRecycleBin_imageIds = Lens.lens (\ListImagesInRecycleBin' {imageIds} -> imageIds) (\s@ListImagesInRecycleBin' {} a -> s {imageIds = a} :: ListImagesInRecycleBin) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
listImagesInRecycleBin_dryRun :: Lens.Lens' ListImagesInRecycleBin (Prelude.Maybe Prelude.Bool)
listImagesInRecycleBin_dryRun = Lens.lens (\ListImagesInRecycleBin' {dryRun} -> dryRun) (\s@ListImagesInRecycleBin' {} a -> s {dryRun = a} :: ListImagesInRecycleBin)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If you do not specify a value for /MaxResults/, the request returns
-- 1,000 items per page by default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
listImagesInRecycleBin_maxResults :: Lens.Lens' ListImagesInRecycleBin (Prelude.Maybe Prelude.Natural)
listImagesInRecycleBin_maxResults = Lens.lens (\ListImagesInRecycleBin' {maxResults} -> maxResults) (\s@ListImagesInRecycleBin' {} a -> s {maxResults = a} :: ListImagesInRecycleBin)

instance Core.AWSPager ListImagesInRecycleBin where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImagesInRecycleBinResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listImagesInRecycleBinResponse_images
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listImagesInRecycleBin_nextToken
          Lens..~ rs
          Lens.^? listImagesInRecycleBinResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListImagesInRecycleBin where
  type
    AWSResponse ListImagesInRecycleBin =
      ListImagesInRecycleBinResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListImagesInRecycleBinResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "imageSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImagesInRecycleBin where
  hashWithSalt _salt ListImagesInRecycleBin' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` imageIds
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListImagesInRecycleBin where
  rnf ListImagesInRecycleBin' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imageIds
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListImagesInRecycleBin where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListImagesInRecycleBin where
  toPath = Prelude.const "/"

instance Core.ToQuery ListImagesInRecycleBin where
  toQuery ListImagesInRecycleBin' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListImagesInRecycleBin" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "ImageId" Prelude.<$> imageIds),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListImagesInRecycleBinResponse' smart constructor.
data ListImagesInRecycleBinResponse = ListImagesInRecycleBinResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the AMIs.
    images :: Prelude.Maybe [ImageRecycleBinInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagesInRecycleBinResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImagesInRecycleBinResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'images', 'listImagesInRecycleBinResponse_images' - Information about the AMIs.
--
-- 'httpStatus', 'listImagesInRecycleBinResponse_httpStatus' - The response's http status code.
newListImagesInRecycleBinResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImagesInRecycleBinResponse
newListImagesInRecycleBinResponse pHttpStatus_ =
  ListImagesInRecycleBinResponse'
    { nextToken =
        Prelude.Nothing,
      images = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listImagesInRecycleBinResponse_nextToken :: Lens.Lens' ListImagesInRecycleBinResponse (Prelude.Maybe Prelude.Text)
listImagesInRecycleBinResponse_nextToken = Lens.lens (\ListImagesInRecycleBinResponse' {nextToken} -> nextToken) (\s@ListImagesInRecycleBinResponse' {} a -> s {nextToken = a} :: ListImagesInRecycleBinResponse)

-- | Information about the AMIs.
listImagesInRecycleBinResponse_images :: Lens.Lens' ListImagesInRecycleBinResponse (Prelude.Maybe [ImageRecycleBinInfo])
listImagesInRecycleBinResponse_images = Lens.lens (\ListImagesInRecycleBinResponse' {images} -> images) (\s@ListImagesInRecycleBinResponse' {} a -> s {images = a} :: ListImagesInRecycleBinResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImagesInRecycleBinResponse_httpStatus :: Lens.Lens' ListImagesInRecycleBinResponse Prelude.Int
listImagesInRecycleBinResponse_httpStatus = Lens.lens (\ListImagesInRecycleBinResponse' {httpStatus} -> httpStatus) (\s@ListImagesInRecycleBinResponse' {} a -> s {httpStatus = a} :: ListImagesInRecycleBinResponse)

instance
  Prelude.NFData
    ListImagesInRecycleBinResponse
  where
  rnf ListImagesInRecycleBinResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf images
      `Prelude.seq` Prelude.rnf httpStatus
