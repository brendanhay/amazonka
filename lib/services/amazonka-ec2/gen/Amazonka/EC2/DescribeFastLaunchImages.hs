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
-- Module      : Amazonka.EC2.DescribeFastLaunchImages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe details for Windows AMIs that are configured for faster
-- launching.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeFastLaunchImages
  ( -- * Creating a Request
    DescribeFastLaunchImages (..),
    newDescribeFastLaunchImages,

    -- * Request Lenses
    describeFastLaunchImages_dryRun,
    describeFastLaunchImages_filters,
    describeFastLaunchImages_imageIds,
    describeFastLaunchImages_maxResults,
    describeFastLaunchImages_nextToken,

    -- * Destructuring the Response
    DescribeFastLaunchImagesResponse (..),
    newDescribeFastLaunchImagesResponse,

    -- * Response Lenses
    describeFastLaunchImagesResponse_fastLaunchImages,
    describeFastLaunchImagesResponse_nextToken,
    describeFastLaunchImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFastLaunchImages' smart constructor.
data DescribeFastLaunchImages = DescribeFastLaunchImages'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Use the following filters to streamline results.
    --
    -- -   @resource-type@ - The resource type for pre-provisioning.
    --
    -- -   @launch-template@ - The launch template that is associated with the
    --     pre-provisioned Windows AMI.
    --
    -- -   @owner-id@ - The owner ID for the pre-provisioning resource.
    --
    -- -   @state@ - The current state of fast launching for the Windows AMI.
    filters :: Prelude.Maybe [Filter],
    -- | Details for one or more Windows AMI image IDs.
    imageIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned from a previous paginated request. Pagination
    -- continues from the end of the items returned by the previous request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFastLaunchImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeFastLaunchImages_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeFastLaunchImages_filters' - Use the following filters to streamline results.
--
-- -   @resource-type@ - The resource type for pre-provisioning.
--
-- -   @launch-template@ - The launch template that is associated with the
--     pre-provisioned Windows AMI.
--
-- -   @owner-id@ - The owner ID for the pre-provisioning resource.
--
-- -   @state@ - The current state of fast launching for the Windows AMI.
--
-- 'imageIds', 'describeFastLaunchImages_imageIds' - Details for one or more Windows AMI image IDs.
--
-- 'maxResults', 'describeFastLaunchImages_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeFastLaunchImages_nextToken' - The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
newDescribeFastLaunchImages ::
  DescribeFastLaunchImages
newDescribeFastLaunchImages =
  DescribeFastLaunchImages'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      imageIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFastLaunchImages_dryRun :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe Prelude.Bool)
describeFastLaunchImages_dryRun = Lens.lens (\DescribeFastLaunchImages' {dryRun} -> dryRun) (\s@DescribeFastLaunchImages' {} a -> s {dryRun = a} :: DescribeFastLaunchImages)

-- | Use the following filters to streamline results.
--
-- -   @resource-type@ - The resource type for pre-provisioning.
--
-- -   @launch-template@ - The launch template that is associated with the
--     pre-provisioned Windows AMI.
--
-- -   @owner-id@ - The owner ID for the pre-provisioning resource.
--
-- -   @state@ - The current state of fast launching for the Windows AMI.
describeFastLaunchImages_filters :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe [Filter])
describeFastLaunchImages_filters = Lens.lens (\DescribeFastLaunchImages' {filters} -> filters) (\s@DescribeFastLaunchImages' {} a -> s {filters = a} :: DescribeFastLaunchImages) Prelude.. Lens.mapping Lens.coerced

-- | Details for one or more Windows AMI image IDs.
describeFastLaunchImages_imageIds :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe [Prelude.Text])
describeFastLaunchImages_imageIds = Lens.lens (\DescribeFastLaunchImages' {imageIds} -> imageIds) (\s@DescribeFastLaunchImages' {} a -> s {imageIds = a} :: DescribeFastLaunchImages) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeFastLaunchImages_maxResults :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe Prelude.Natural)
describeFastLaunchImages_maxResults = Lens.lens (\DescribeFastLaunchImages' {maxResults} -> maxResults) (\s@DescribeFastLaunchImages' {} a -> s {maxResults = a} :: DescribeFastLaunchImages)

-- | The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
describeFastLaunchImages_nextToken :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe Prelude.Text)
describeFastLaunchImages_nextToken = Lens.lens (\DescribeFastLaunchImages' {nextToken} -> nextToken) (\s@DescribeFastLaunchImages' {} a -> s {nextToken = a} :: DescribeFastLaunchImages)

instance Core.AWSPager DescribeFastLaunchImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFastLaunchImagesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFastLaunchImagesResponse_fastLaunchImages
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeFastLaunchImages_nextToken
          Lens..~ rs
          Lens.^? describeFastLaunchImagesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeFastLaunchImages where
  type
    AWSResponse DescribeFastLaunchImages =
      DescribeFastLaunchImagesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFastLaunchImagesResponse'
            Prelude.<$> ( x
                            Data..@? "fastLaunchImageSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFastLaunchImages where
  hashWithSalt _salt DescribeFastLaunchImages' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` imageIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeFastLaunchImages where
  rnf DescribeFastLaunchImages' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf imageIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeFastLaunchImages where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeFastLaunchImages where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFastLaunchImages where
  toQuery DescribeFastLaunchImages' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeFastLaunchImages" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          (Data.toQueryList "ImageId" Prelude.<$> imageIds),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeFastLaunchImagesResponse' smart constructor.
data DescribeFastLaunchImagesResponse = DescribeFastLaunchImagesResponse'
  { -- | A collection of details about the fast-launch enabled Windows images
    -- that meet the requested criteria.
    fastLaunchImages :: Prelude.Maybe [DescribeFastLaunchImagesSuccessItem],
    -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFastLaunchImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fastLaunchImages', 'describeFastLaunchImagesResponse_fastLaunchImages' - A collection of details about the fast-launch enabled Windows images
-- that meet the requested criteria.
--
-- 'nextToken', 'describeFastLaunchImagesResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'httpStatus', 'describeFastLaunchImagesResponse_httpStatus' - The response's http status code.
newDescribeFastLaunchImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFastLaunchImagesResponse
newDescribeFastLaunchImagesResponse pHttpStatus_ =
  DescribeFastLaunchImagesResponse'
    { fastLaunchImages =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of details about the fast-launch enabled Windows images
-- that meet the requested criteria.
describeFastLaunchImagesResponse_fastLaunchImages :: Lens.Lens' DescribeFastLaunchImagesResponse (Prelude.Maybe [DescribeFastLaunchImagesSuccessItem])
describeFastLaunchImagesResponse_fastLaunchImages = Lens.lens (\DescribeFastLaunchImagesResponse' {fastLaunchImages} -> fastLaunchImages) (\s@DescribeFastLaunchImagesResponse' {} a -> s {fastLaunchImages = a} :: DescribeFastLaunchImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeFastLaunchImagesResponse_nextToken :: Lens.Lens' DescribeFastLaunchImagesResponse (Prelude.Maybe Prelude.Text)
describeFastLaunchImagesResponse_nextToken = Lens.lens (\DescribeFastLaunchImagesResponse' {nextToken} -> nextToken) (\s@DescribeFastLaunchImagesResponse' {} a -> s {nextToken = a} :: DescribeFastLaunchImagesResponse)

-- | The response's http status code.
describeFastLaunchImagesResponse_httpStatus :: Lens.Lens' DescribeFastLaunchImagesResponse Prelude.Int
describeFastLaunchImagesResponse_httpStatus = Lens.lens (\DescribeFastLaunchImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeFastLaunchImagesResponse' {} a -> s {httpStatus = a} :: DescribeFastLaunchImagesResponse)

instance
  Prelude.NFData
    DescribeFastLaunchImagesResponse
  where
  rnf DescribeFastLaunchImagesResponse' {..} =
    Prelude.rnf fastLaunchImages
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
