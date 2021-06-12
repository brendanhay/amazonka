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
-- Module      : Network.AWS.EC2.DescribeRegions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Regions that are enabled for your account, or all Regions.
--
-- For a list of the Regions supported by Amazon EC2, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region Regions and Endpoints>.
--
-- For information about enabling and disabling Regions for your account,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html Managing AWS Regions>
-- in the /AWS General Reference/.
module Network.AWS.EC2.DescribeRegions
  ( -- * Creating a Request
    DescribeRegions (..),
    newDescribeRegions,

    -- * Request Lenses
    describeRegions_dryRun,
    describeRegions_regionNames,
    describeRegions_filters,
    describeRegions_allRegions,

    -- * Destructuring the Response
    DescribeRegionsResponse (..),
    newDescribeRegionsResponse,

    -- * Response Lenses
    describeRegionsResponse_regions,
    describeRegionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The names of the Regions. You can specify any Regions, whether they are
    -- enabled and disabled for your account.
    regionNames :: Core.Maybe [Core.Text],
    -- | The filters.
    --
    -- -   @endpoint@ - The endpoint of the Region (for example,
    --     @ec2.us-east-1.amazonaws.com@).
    --
    -- -   @opt-in-status@ - The opt-in status of the Region
    --     (@opt-in-not-required@ | @opted-in@ | @not-opted-in@).
    --
    -- -   @region-name@ - The name of the Region (for example, @us-east-1@).
    filters :: Core.Maybe [Filter],
    -- | Indicates whether to display all Regions, including Regions that are
    -- disabled for your account.
    allRegions :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRegions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeRegions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'regionNames', 'describeRegions_regionNames' - The names of the Regions. You can specify any Regions, whether they are
-- enabled and disabled for your account.
--
-- 'filters', 'describeRegions_filters' - The filters.
--
-- -   @endpoint@ - The endpoint of the Region (for example,
--     @ec2.us-east-1.amazonaws.com@).
--
-- -   @opt-in-status@ - The opt-in status of the Region
--     (@opt-in-not-required@ | @opted-in@ | @not-opted-in@).
--
-- -   @region-name@ - The name of the Region (for example, @us-east-1@).
--
-- 'allRegions', 'describeRegions_allRegions' - Indicates whether to display all Regions, including Regions that are
-- disabled for your account.
newDescribeRegions ::
  DescribeRegions
newDescribeRegions =
  DescribeRegions'
    { dryRun = Core.Nothing,
      regionNames = Core.Nothing,
      filters = Core.Nothing,
      allRegions = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeRegions_dryRun :: Lens.Lens' DescribeRegions (Core.Maybe Core.Bool)
describeRegions_dryRun = Lens.lens (\DescribeRegions' {dryRun} -> dryRun) (\s@DescribeRegions' {} a -> s {dryRun = a} :: DescribeRegions)

-- | The names of the Regions. You can specify any Regions, whether they are
-- enabled and disabled for your account.
describeRegions_regionNames :: Lens.Lens' DescribeRegions (Core.Maybe [Core.Text])
describeRegions_regionNames = Lens.lens (\DescribeRegions' {regionNames} -> regionNames) (\s@DescribeRegions' {} a -> s {regionNames = a} :: DescribeRegions) Core.. Lens.mapping Lens._Coerce

-- | The filters.
--
-- -   @endpoint@ - The endpoint of the Region (for example,
--     @ec2.us-east-1.amazonaws.com@).
--
-- -   @opt-in-status@ - The opt-in status of the Region
--     (@opt-in-not-required@ | @opted-in@ | @not-opted-in@).
--
-- -   @region-name@ - The name of the Region (for example, @us-east-1@).
describeRegions_filters :: Lens.Lens' DescribeRegions (Core.Maybe [Filter])
describeRegions_filters = Lens.lens (\DescribeRegions' {filters} -> filters) (\s@DescribeRegions' {} a -> s {filters = a} :: DescribeRegions) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether to display all Regions, including Regions that are
-- disabled for your account.
describeRegions_allRegions :: Lens.Lens' DescribeRegions (Core.Maybe Core.Bool)
describeRegions_allRegions = Lens.lens (\DescribeRegions' {allRegions} -> allRegions) (\s@DescribeRegions' {} a -> s {allRegions = a} :: DescribeRegions)

instance Core.AWSRequest DescribeRegions where
  type
    AWSResponse DescribeRegions =
      DescribeRegionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeRegionsResponse'
            Core.<$> ( x Core..@? "regionInfo" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeRegions

instance Core.NFData DescribeRegions

instance Core.ToHeaders DescribeRegions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeRegions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRegions where
  toQuery DescribeRegions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeRegions" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "RegionName" Core.<$> regionNames),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        "AllRegions" Core.=: allRegions
      ]

-- | /See:/ 'newDescribeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { -- | Information about the Regions.
    regions :: Core.Maybe [RegionInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRegionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'describeRegionsResponse_regions' - Information about the Regions.
--
-- 'httpStatus', 'describeRegionsResponse_httpStatus' - The response's http status code.
newDescribeRegionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeRegionsResponse
newDescribeRegionsResponse pHttpStatus_ =
  DescribeRegionsResponse'
    { regions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Regions.
describeRegionsResponse_regions :: Lens.Lens' DescribeRegionsResponse (Core.Maybe [RegionInfo])
describeRegionsResponse_regions = Lens.lens (\DescribeRegionsResponse' {regions} -> regions) (\s@DescribeRegionsResponse' {} a -> s {regions = a} :: DescribeRegionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeRegionsResponse_httpStatus :: Lens.Lens' DescribeRegionsResponse Core.Int
describeRegionsResponse_httpStatus = Lens.lens (\DescribeRegionsResponse' {httpStatus} -> httpStatus) (\s@DescribeRegionsResponse' {} a -> s {httpStatus = a} :: DescribeRegionsResponse)

instance Core.NFData DescribeRegionsResponse
