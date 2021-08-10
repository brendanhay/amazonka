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
-- Module      : Network.AWS.EC2.DescribeFpgaImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Amazon FPGA Images (AFIs) available to you. These include
-- public AFIs, private AFIs that you own, and AFIs owned by other AWS
-- accounts for which you have load permissions.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFpgaImages
  ( -- * Creating a Request
    DescribeFpgaImages (..),
    newDescribeFpgaImages,

    -- * Request Lenses
    describeFpgaImages_nextToken,
    describeFpgaImages_dryRun,
    describeFpgaImages_maxResults,
    describeFpgaImages_owners,
    describeFpgaImages_fpgaImageIds,
    describeFpgaImages_filters,

    -- * Destructuring the Response
    DescribeFpgaImagesResponse (..),
    newDescribeFpgaImagesResponse,

    -- * Response Lenses
    describeFpgaImagesResponse_nextToken,
    describeFpgaImagesResponse_fpgaImages,
    describeFpgaImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFpgaImages' smart constructor.
data DescribeFpgaImages = DescribeFpgaImages'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is
    -- the sender of the request), or an AWS owner alias (valid values are
    -- @amazon@ | @aws-marketplace@).
    owners :: Prelude.Maybe [Prelude.Text],
    -- | The AFI IDs.
    fpgaImageIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    --
    -- -   @create-time@ - The creation time of the AFI.
    --
    -- -   @fpga-image-id@ - The FPGA image identifier (AFI ID).
    --
    -- -   @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).
    --
    -- -   @name@ - The name of the AFI.
    --
    -- -   @owner-id@ - The AWS account ID of the AFI owner.
    --
    -- -   @product-code@ - The product code.
    --
    -- -   @shell-version@ - The version of the AWS Shell that was used to
    --     create the bitstream.
    --
    -- -   @state@ - The state of the AFI (@pending@ | @failed@ | @available@ |
    --     @unavailable@).
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @update-time@ - The time of the most recent update.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFpgaImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFpgaImages_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeFpgaImages_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeFpgaImages_maxResults' - The maximum number of results to return in a single call.
--
-- 'owners', 'describeFpgaImages_owners' - Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is
-- the sender of the request), or an AWS owner alias (valid values are
-- @amazon@ | @aws-marketplace@).
--
-- 'fpgaImageIds', 'describeFpgaImages_fpgaImageIds' - The AFI IDs.
--
-- 'filters', 'describeFpgaImages_filters' - The filters.
--
-- -   @create-time@ - The creation time of the AFI.
--
-- -   @fpga-image-id@ - The FPGA image identifier (AFI ID).
--
-- -   @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).
--
-- -   @name@ - The name of the AFI.
--
-- -   @owner-id@ - The AWS account ID of the AFI owner.
--
-- -   @product-code@ - The product code.
--
-- -   @shell-version@ - The version of the AWS Shell that was used to
--     create the bitstream.
--
-- -   @state@ - The state of the AFI (@pending@ | @failed@ | @available@ |
--     @unavailable@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @update-time@ - The time of the most recent update.
newDescribeFpgaImages ::
  DescribeFpgaImages
newDescribeFpgaImages =
  DescribeFpgaImages'
    { nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      owners = Prelude.Nothing,
      fpgaImageIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
describeFpgaImages_nextToken :: Lens.Lens' DescribeFpgaImages (Prelude.Maybe Prelude.Text)
describeFpgaImages_nextToken = Lens.lens (\DescribeFpgaImages' {nextToken} -> nextToken) (\s@DescribeFpgaImages' {} a -> s {nextToken = a} :: DescribeFpgaImages)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFpgaImages_dryRun :: Lens.Lens' DescribeFpgaImages (Prelude.Maybe Prelude.Bool)
describeFpgaImages_dryRun = Lens.lens (\DescribeFpgaImages' {dryRun} -> dryRun) (\s@DescribeFpgaImages' {} a -> s {dryRun = a} :: DescribeFpgaImages)

-- | The maximum number of results to return in a single call.
describeFpgaImages_maxResults :: Lens.Lens' DescribeFpgaImages (Prelude.Maybe Prelude.Natural)
describeFpgaImages_maxResults = Lens.lens (\DescribeFpgaImages' {maxResults} -> maxResults) (\s@DescribeFpgaImages' {} a -> s {maxResults = a} :: DescribeFpgaImages)

-- | Filters the AFI by owner. Specify an AWS account ID, @self@ (owner is
-- the sender of the request), or an AWS owner alias (valid values are
-- @amazon@ | @aws-marketplace@).
describeFpgaImages_owners :: Lens.Lens' DescribeFpgaImages (Prelude.Maybe [Prelude.Text])
describeFpgaImages_owners = Lens.lens (\DescribeFpgaImages' {owners} -> owners) (\s@DescribeFpgaImages' {} a -> s {owners = a} :: DescribeFpgaImages) Prelude.. Lens.mapping Lens._Coerce

-- | The AFI IDs.
describeFpgaImages_fpgaImageIds :: Lens.Lens' DescribeFpgaImages (Prelude.Maybe [Prelude.Text])
describeFpgaImages_fpgaImageIds = Lens.lens (\DescribeFpgaImages' {fpgaImageIds} -> fpgaImageIds) (\s@DescribeFpgaImages' {} a -> s {fpgaImageIds = a} :: DescribeFpgaImages) Prelude.. Lens.mapping Lens._Coerce

-- | The filters.
--
-- -   @create-time@ - The creation time of the AFI.
--
-- -   @fpga-image-id@ - The FPGA image identifier (AFI ID).
--
-- -   @fpga-image-global-id@ - The global FPGA image identifier (AGFI ID).
--
-- -   @name@ - The name of the AFI.
--
-- -   @owner-id@ - The AWS account ID of the AFI owner.
--
-- -   @product-code@ - The product code.
--
-- -   @shell-version@ - The version of the AWS Shell that was used to
--     create the bitstream.
--
-- -   @state@ - The state of the AFI (@pending@ | @failed@ | @available@ |
--     @unavailable@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @update-time@ - The time of the most recent update.
describeFpgaImages_filters :: Lens.Lens' DescribeFpgaImages (Prelude.Maybe [Filter])
describeFpgaImages_filters = Lens.lens (\DescribeFpgaImages' {filters} -> filters) (\s@DescribeFpgaImages' {} a -> s {filters = a} :: DescribeFpgaImages) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeFpgaImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFpgaImagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFpgaImagesResponse_fpgaImages
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeFpgaImages_nextToken
          Lens..~ rs
          Lens.^? describeFpgaImagesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeFpgaImages where
  type
    AWSResponse DescribeFpgaImages =
      DescribeFpgaImagesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFpgaImagesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "fpgaImageSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFpgaImages

instance Prelude.NFData DescribeFpgaImages

instance Core.ToHeaders DescribeFpgaImages where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeFpgaImages where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFpgaImages where
  toQuery DescribeFpgaImages' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeFpgaImages" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Owner" Prelude.<$> owners),
        Core.toQuery
          ( Core.toQueryList "FpgaImageId"
              Prelude.<$> fpgaImageIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeFpgaImagesResponse' smart constructor.
data DescribeFpgaImagesResponse = DescribeFpgaImagesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the FPGA images.
    fpgaImages :: Prelude.Maybe [FpgaImage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFpgaImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFpgaImagesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'fpgaImages', 'describeFpgaImagesResponse_fpgaImages' - Information about the FPGA images.
--
-- 'httpStatus', 'describeFpgaImagesResponse_httpStatus' - The response's http status code.
newDescribeFpgaImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFpgaImagesResponse
newDescribeFpgaImagesResponse pHttpStatus_ =
  DescribeFpgaImagesResponse'
    { nextToken =
        Prelude.Nothing,
      fpgaImages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeFpgaImagesResponse_nextToken :: Lens.Lens' DescribeFpgaImagesResponse (Prelude.Maybe Prelude.Text)
describeFpgaImagesResponse_nextToken = Lens.lens (\DescribeFpgaImagesResponse' {nextToken} -> nextToken) (\s@DescribeFpgaImagesResponse' {} a -> s {nextToken = a} :: DescribeFpgaImagesResponse)

-- | Information about the FPGA images.
describeFpgaImagesResponse_fpgaImages :: Lens.Lens' DescribeFpgaImagesResponse (Prelude.Maybe [FpgaImage])
describeFpgaImagesResponse_fpgaImages = Lens.lens (\DescribeFpgaImagesResponse' {fpgaImages} -> fpgaImages) (\s@DescribeFpgaImagesResponse' {} a -> s {fpgaImages = a} :: DescribeFpgaImagesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFpgaImagesResponse_httpStatus :: Lens.Lens' DescribeFpgaImagesResponse Prelude.Int
describeFpgaImagesResponse_httpStatus = Lens.lens (\DescribeFpgaImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeFpgaImagesResponse' {} a -> s {httpStatus = a} :: DescribeFpgaImagesResponse)

instance Prelude.NFData DescribeFpgaImagesResponse
