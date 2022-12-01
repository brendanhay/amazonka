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
-- Module      : Amazonka.EC2.DescribeRegions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Regions that are enabled for your account, or all Regions.
--
-- For a list of the Regions supported by Amazon EC2, see
-- <https://docs.aws.amazon.com/general/latest/gr/ec2-service.html Amazon Elastic Compute Cloud endpoints and quotas>.
--
-- For information about enabling and disabling Regions for your account,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html Managing Amazon Web Services Regions>
-- in the /Amazon Web Services General Reference/.
module Amazonka.EC2.DescribeRegions
  ( -- * Creating a Request
    DescribeRegions (..),
    newDescribeRegions,

    -- * Request Lenses
    describeRegions_regionNames,
    describeRegions_filters,
    describeRegions_dryRun,
    describeRegions_allRegions,

    -- * Destructuring the Response
    DescribeRegionsResponse (..),
    newDescribeRegionsResponse,

    -- * Response Lenses
    describeRegionsResponse_regions,
    describeRegionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { -- | The names of the Regions. You can specify any Regions, whether they are
    -- enabled and disabled for your account.
    regionNames :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    --
    -- -   @endpoint@ - The endpoint of the Region (for example,
    --     @ec2.us-east-1.amazonaws.com@).
    --
    -- -   @opt-in-status@ - The opt-in status of the Region
    --     (@opt-in-not-required@ | @opted-in@ | @not-opted-in@).
    --
    -- -   @region-name@ - The name of the Region (for example, @us-east-1@).
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to display all Regions, including Regions that are
    -- disabled for your account.
    allRegions :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRegions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'dryRun', 'describeRegions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'allRegions', 'describeRegions_allRegions' - Indicates whether to display all Regions, including Regions that are
-- disabled for your account.
newDescribeRegions ::
  DescribeRegions
newDescribeRegions =
  DescribeRegions'
    { regionNames = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      allRegions = Prelude.Nothing
    }

-- | The names of the Regions. You can specify any Regions, whether they are
-- enabled and disabled for your account.
describeRegions_regionNames :: Lens.Lens' DescribeRegions (Prelude.Maybe [Prelude.Text])
describeRegions_regionNames = Lens.lens (\DescribeRegions' {regionNames} -> regionNames) (\s@DescribeRegions' {} a -> s {regionNames = a} :: DescribeRegions) Prelude.. Lens.mapping Lens.coerced

-- | The filters.
--
-- -   @endpoint@ - The endpoint of the Region (for example,
--     @ec2.us-east-1.amazonaws.com@).
--
-- -   @opt-in-status@ - The opt-in status of the Region
--     (@opt-in-not-required@ | @opted-in@ | @not-opted-in@).
--
-- -   @region-name@ - The name of the Region (for example, @us-east-1@).
describeRegions_filters :: Lens.Lens' DescribeRegions (Prelude.Maybe [Filter])
describeRegions_filters = Lens.lens (\DescribeRegions' {filters} -> filters) (\s@DescribeRegions' {} a -> s {filters = a} :: DescribeRegions) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeRegions_dryRun :: Lens.Lens' DescribeRegions (Prelude.Maybe Prelude.Bool)
describeRegions_dryRun = Lens.lens (\DescribeRegions' {dryRun} -> dryRun) (\s@DescribeRegions' {} a -> s {dryRun = a} :: DescribeRegions)

-- | Indicates whether to display all Regions, including Regions that are
-- disabled for your account.
describeRegions_allRegions :: Lens.Lens' DescribeRegions (Prelude.Maybe Prelude.Bool)
describeRegions_allRegions = Lens.lens (\DescribeRegions' {allRegions} -> allRegions) (\s@DescribeRegions' {} a -> s {allRegions = a} :: DescribeRegions)

instance Core.AWSRequest DescribeRegions where
  type
    AWSResponse DescribeRegions =
      DescribeRegionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeRegionsResponse'
            Prelude.<$> ( x Core..@? "regionInfo" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRegions where
  hashWithSalt _salt DescribeRegions' {..} =
    _salt `Prelude.hashWithSalt` regionNames
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` allRegions

instance Prelude.NFData DescribeRegions where
  rnf DescribeRegions' {..} =
    Prelude.rnf regionNames
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf allRegions

instance Core.ToHeaders DescribeRegions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeRegions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeRegions where
  toQuery DescribeRegions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeRegions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "RegionName"
              Prelude.<$> regionNames
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "AllRegions" Core.=: allRegions
      ]

-- | /See:/ 'newDescribeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { -- | Information about the Regions.
    regions :: Prelude.Maybe [RegionInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeRegionsResponse
newDescribeRegionsResponse pHttpStatus_ =
  DescribeRegionsResponse'
    { regions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Regions.
describeRegionsResponse_regions :: Lens.Lens' DescribeRegionsResponse (Prelude.Maybe [RegionInfo])
describeRegionsResponse_regions = Lens.lens (\DescribeRegionsResponse' {regions} -> regions) (\s@DescribeRegionsResponse' {} a -> s {regions = a} :: DescribeRegionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRegionsResponse_httpStatus :: Lens.Lens' DescribeRegionsResponse Prelude.Int
describeRegionsResponse_httpStatus = Lens.lens (\DescribeRegionsResponse' {httpStatus} -> httpStatus) (\s@DescribeRegionsResponse' {} a -> s {httpStatus = a} :: DescribeRegionsResponse)

instance Prelude.NFData DescribeRegionsResponse where
  rnf DescribeRegionsResponse' {..} =
    Prelude.rnf regions
      `Prelude.seq` Prelude.rnf httpStatus
