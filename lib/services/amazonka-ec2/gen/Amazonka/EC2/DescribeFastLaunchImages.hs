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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeFastLaunchImages_nextToken,
    describeFastLaunchImages_imageIds,
    describeFastLaunchImages_filters,
    describeFastLaunchImages_dryRun,
    describeFastLaunchImages_maxResults,

    -- * Destructuring the Response
    DescribeFastLaunchImagesResponse (..),
    newDescribeFastLaunchImagesResponse,

    -- * Response Lenses
    describeFastLaunchImagesResponse_nextToken,
    describeFastLaunchImagesResponse_fastLaunchImages,
    describeFastLaunchImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFastLaunchImages' smart constructor.
data DescribeFastLaunchImages = DescribeFastLaunchImages'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Details for one or more Windows AMI image IDs.
    imageIds :: Prelude.Maybe [Prelude.Text],
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
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another request with the returned NextToken
    -- value. If this parameter is not specified, then all results are
    -- returned.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeFastLaunchImages_nextToken' - The token for the next set of results.
--
-- 'imageIds', 'describeFastLaunchImages_imageIds' - Details for one or more Windows AMI image IDs.
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
-- 'dryRun', 'describeFastLaunchImages_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeFastLaunchImages_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another request with the returned NextToken
-- value. If this parameter is not specified, then all results are
-- returned.
newDescribeFastLaunchImages ::
  DescribeFastLaunchImages
newDescribeFastLaunchImages =
  DescribeFastLaunchImages'
    { nextToken =
        Prelude.Nothing,
      imageIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results.
describeFastLaunchImages_nextToken :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe Prelude.Text)
describeFastLaunchImages_nextToken = Lens.lens (\DescribeFastLaunchImages' {nextToken} -> nextToken) (\s@DescribeFastLaunchImages' {} a -> s {nextToken = a} :: DescribeFastLaunchImages)

-- | Details for one or more Windows AMI image IDs.
describeFastLaunchImages_imageIds :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe [Prelude.Text])
describeFastLaunchImages_imageIds = Lens.lens (\DescribeFastLaunchImages' {imageIds} -> imageIds) (\s@DescribeFastLaunchImages' {} a -> s {imageIds = a} :: DescribeFastLaunchImages) Prelude.. Lens.mapping Lens.coerced

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

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFastLaunchImages_dryRun :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe Prelude.Bool)
describeFastLaunchImages_dryRun = Lens.lens (\DescribeFastLaunchImages' {dryRun} -> dryRun) (\s@DescribeFastLaunchImages' {} a -> s {dryRun = a} :: DescribeFastLaunchImages)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another request with the returned NextToken
-- value. If this parameter is not specified, then all results are
-- returned.
describeFastLaunchImages_maxResults :: Lens.Lens' DescribeFastLaunchImages (Prelude.Maybe Prelude.Natural)
describeFastLaunchImages_maxResults = Lens.lens (\DescribeFastLaunchImages' {maxResults} -> maxResults) (\s@DescribeFastLaunchImages' {} a -> s {maxResults = a} :: DescribeFastLaunchImages)

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
      Prelude.Just Prelude.$
        rq
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
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "fastLaunchImageSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFastLaunchImages where
  hashWithSalt _salt DescribeFastLaunchImages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` imageIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeFastLaunchImages where
  rnf DescribeFastLaunchImages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imageIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeFastLaunchImages where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeFastLaunchImages where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFastLaunchImages where
  toQuery DescribeFastLaunchImages' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeFastLaunchImages" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "ImageId" Prelude.<$> imageIds),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeFastLaunchImagesResponse' smart constructor.
data DescribeFastLaunchImagesResponse = DescribeFastLaunchImagesResponse'
  { -- | The token to use for the next set of results. This value is null when
    -- there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of details about the fast-launch enabled Windows images
    -- that meet the requested criteria.
    fastLaunchImages :: Prelude.Maybe [DescribeFastLaunchImagesSuccessItem],
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
-- 'nextToken', 'describeFastLaunchImagesResponse_nextToken' - The token to use for the next set of results. This value is null when
-- there are no more results to return.
--
-- 'fastLaunchImages', 'describeFastLaunchImagesResponse_fastLaunchImages' - A collection of details about the fast-launch enabled Windows images
-- that meet the requested criteria.
--
-- 'httpStatus', 'describeFastLaunchImagesResponse_httpStatus' - The response's http status code.
newDescribeFastLaunchImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFastLaunchImagesResponse
newDescribeFastLaunchImagesResponse pHttpStatus_ =
  DescribeFastLaunchImagesResponse'
    { nextToken =
        Prelude.Nothing,
      fastLaunchImages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use for the next set of results. This value is null when
-- there are no more results to return.
describeFastLaunchImagesResponse_nextToken :: Lens.Lens' DescribeFastLaunchImagesResponse (Prelude.Maybe Prelude.Text)
describeFastLaunchImagesResponse_nextToken = Lens.lens (\DescribeFastLaunchImagesResponse' {nextToken} -> nextToken) (\s@DescribeFastLaunchImagesResponse' {} a -> s {nextToken = a} :: DescribeFastLaunchImagesResponse)

-- | A collection of details about the fast-launch enabled Windows images
-- that meet the requested criteria.
describeFastLaunchImagesResponse_fastLaunchImages :: Lens.Lens' DescribeFastLaunchImagesResponse (Prelude.Maybe [DescribeFastLaunchImagesSuccessItem])
describeFastLaunchImagesResponse_fastLaunchImages = Lens.lens (\DescribeFastLaunchImagesResponse' {fastLaunchImages} -> fastLaunchImages) (\s@DescribeFastLaunchImagesResponse' {} a -> s {fastLaunchImages = a} :: DescribeFastLaunchImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeFastLaunchImagesResponse_httpStatus :: Lens.Lens' DescribeFastLaunchImagesResponse Prelude.Int
describeFastLaunchImagesResponse_httpStatus = Lens.lens (\DescribeFastLaunchImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeFastLaunchImagesResponse' {} a -> s {httpStatus = a} :: DescribeFastLaunchImagesResponse)

instance
  Prelude.NFData
    DescribeFastLaunchImagesResponse
  where
  rnf DescribeFastLaunchImagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fastLaunchImages
      `Prelude.seq` Prelude.rnf httpStatus
