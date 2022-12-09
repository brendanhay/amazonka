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
-- Module      : Amazonka.RDS.DescribeBlueGreenDeployments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about blue\/green deployments.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon Aurora User Guide/.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeBlueGreenDeployments
  ( -- * Creating a Request
    DescribeBlueGreenDeployments (..),
    newDescribeBlueGreenDeployments,

    -- * Request Lenses
    describeBlueGreenDeployments_blueGreenDeploymentIdentifier,
    describeBlueGreenDeployments_filters,
    describeBlueGreenDeployments_marker,
    describeBlueGreenDeployments_maxRecords,

    -- * Destructuring the Response
    DescribeBlueGreenDeploymentsResponse (..),
    newDescribeBlueGreenDeploymentsResponse,

    -- * Response Lenses
    describeBlueGreenDeploymentsResponse_blueGreenDeployments,
    describeBlueGreenDeploymentsResponse_marker,
    describeBlueGreenDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBlueGreenDeployments' smart constructor.
data DescribeBlueGreenDeployments = DescribeBlueGreenDeployments'
  { -- | The blue\/green deployment identifier. If this parameter is specified,
    -- information from only the specific blue\/green deployment is returned.
    -- This parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   If supplied, must match an existing blue\/green deployment
    --     identifier.
    blueGreenDeploymentIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A filter that specifies one or more blue\/green deployments to describe.
    --
    -- Supported filters:
    --
    -- -   @blue-green-deployment-identifier@ - Accepts system-generated
    --     identifiers for blue\/green deployments. The results list only
    --     includes information about the blue\/green deployments with the
    --     specified identifiers.
    --
    -- -   @blue-green-deployment-name@ - Accepts user-supplied names for
    --     blue\/green deployments. The results list only includes information
    --     about the blue\/green deployments with the specified names.
    --
    -- -   @source@ - Accepts source databases for a blue\/green deployment.
    --     The results list only includes information about the blue\/green
    --     deployments with the specified source databases.
    --
    -- -   @target@ - Accepts target databases for a blue\/green deployment.
    --     The results list only includes information about the blue\/green
    --     deployments with the specified target databases.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeBlueGreenDeployments@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBlueGreenDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueGreenDeploymentIdentifier', 'describeBlueGreenDeployments_blueGreenDeploymentIdentifier' - The blue\/green deployment identifier. If this parameter is specified,
-- information from only the specific blue\/green deployment is returned.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match an existing blue\/green deployment
--     identifier.
--
-- 'filters', 'describeBlueGreenDeployments_filters' - A filter that specifies one or more blue\/green deployments to describe.
--
-- Supported filters:
--
-- -   @blue-green-deployment-identifier@ - Accepts system-generated
--     identifiers for blue\/green deployments. The results list only
--     includes information about the blue\/green deployments with the
--     specified identifiers.
--
-- -   @blue-green-deployment-name@ - Accepts user-supplied names for
--     blue\/green deployments. The results list only includes information
--     about the blue\/green deployments with the specified names.
--
-- -   @source@ - Accepts source databases for a blue\/green deployment.
--     The results list only includes information about the blue\/green
--     deployments with the specified source databases.
--
-- -   @target@ - Accepts target databases for a blue\/green deployment.
--     The results list only includes information about the blue\/green
--     deployments with the specified target databases.
--
-- 'marker', 'describeBlueGreenDeployments_marker' - An optional pagination token provided by a previous
-- @DescribeBlueGreenDeployments@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeBlueGreenDeployments_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeBlueGreenDeployments ::
  DescribeBlueGreenDeployments
newDescribeBlueGreenDeployments =
  DescribeBlueGreenDeployments'
    { blueGreenDeploymentIdentifier =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The blue\/green deployment identifier. If this parameter is specified,
-- information from only the specific blue\/green deployment is returned.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match an existing blue\/green deployment
--     identifier.
describeBlueGreenDeployments_blueGreenDeploymentIdentifier :: Lens.Lens' DescribeBlueGreenDeployments (Prelude.Maybe Prelude.Text)
describeBlueGreenDeployments_blueGreenDeploymentIdentifier = Lens.lens (\DescribeBlueGreenDeployments' {blueGreenDeploymentIdentifier} -> blueGreenDeploymentIdentifier) (\s@DescribeBlueGreenDeployments' {} a -> s {blueGreenDeploymentIdentifier = a} :: DescribeBlueGreenDeployments)

-- | A filter that specifies one or more blue\/green deployments to describe.
--
-- Supported filters:
--
-- -   @blue-green-deployment-identifier@ - Accepts system-generated
--     identifiers for blue\/green deployments. The results list only
--     includes information about the blue\/green deployments with the
--     specified identifiers.
--
-- -   @blue-green-deployment-name@ - Accepts user-supplied names for
--     blue\/green deployments. The results list only includes information
--     about the blue\/green deployments with the specified names.
--
-- -   @source@ - Accepts source databases for a blue\/green deployment.
--     The results list only includes information about the blue\/green
--     deployments with the specified source databases.
--
-- -   @target@ - Accepts target databases for a blue\/green deployment.
--     The results list only includes information about the blue\/green
--     deployments with the specified target databases.
describeBlueGreenDeployments_filters :: Lens.Lens' DescribeBlueGreenDeployments (Prelude.Maybe [Filter])
describeBlueGreenDeployments_filters = Lens.lens (\DescribeBlueGreenDeployments' {filters} -> filters) (\s@DescribeBlueGreenDeployments' {} a -> s {filters = a} :: DescribeBlueGreenDeployments) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeBlueGreenDeployments@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeBlueGreenDeployments_marker :: Lens.Lens' DescribeBlueGreenDeployments (Prelude.Maybe Prelude.Text)
describeBlueGreenDeployments_marker = Lens.lens (\DescribeBlueGreenDeployments' {marker} -> marker) (\s@DescribeBlueGreenDeployments' {} a -> s {marker = a} :: DescribeBlueGreenDeployments)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeBlueGreenDeployments_maxRecords :: Lens.Lens' DescribeBlueGreenDeployments (Prelude.Maybe Prelude.Natural)
describeBlueGreenDeployments_maxRecords = Lens.lens (\DescribeBlueGreenDeployments' {maxRecords} -> maxRecords) (\s@DescribeBlueGreenDeployments' {} a -> s {maxRecords = a} :: DescribeBlueGreenDeployments)

instance Core.AWSPager DescribeBlueGreenDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBlueGreenDeploymentsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeBlueGreenDeploymentsResponse_blueGreenDeployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBlueGreenDeployments_marker
          Lens..~ rs
          Lens.^? describeBlueGreenDeploymentsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeBlueGreenDeployments where
  type
    AWSResponse DescribeBlueGreenDeployments =
      DescribeBlueGreenDeploymentsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeBlueGreenDeploymentsResult"
      ( \s h x ->
          DescribeBlueGreenDeploymentsResponse'
            Prelude.<$> ( x Data..@? "BlueGreenDeployments"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeBlueGreenDeployments
  where
  hashWithSalt _salt DescribeBlueGreenDeployments' {..} =
    _salt
      `Prelude.hashWithSalt` blueGreenDeploymentIdentifier
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeBlueGreenDeployments where
  rnf DescribeBlueGreenDeployments' {..} =
    Prelude.rnf blueGreenDeploymentIdentifier
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeBlueGreenDeployments where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeBlueGreenDeployments where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBlueGreenDeployments where
  toQuery DescribeBlueGreenDeployments' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeBlueGreenDeployments" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "BlueGreenDeploymentIdentifier"
          Data.=: blueGreenDeploymentIdentifier,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | /See:/ 'newDescribeBlueGreenDeploymentsResponse' smart constructor.
data DescribeBlueGreenDeploymentsResponse = DescribeBlueGreenDeploymentsResponse'
  { -- | Contains a list of blue\/green deployments for the user.
    blueGreenDeployments :: Prelude.Maybe [BlueGreenDeployment],
    -- | A pagination token that can be used in a later
    -- DescribeBlueGreenDeployments request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBlueGreenDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueGreenDeployments', 'describeBlueGreenDeploymentsResponse_blueGreenDeployments' - Contains a list of blue\/green deployments for the user.
--
-- 'marker', 'describeBlueGreenDeploymentsResponse_marker' - A pagination token that can be used in a later
-- DescribeBlueGreenDeployments request.
--
-- 'httpStatus', 'describeBlueGreenDeploymentsResponse_httpStatus' - The response's http status code.
newDescribeBlueGreenDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBlueGreenDeploymentsResponse
newDescribeBlueGreenDeploymentsResponse pHttpStatus_ =
  DescribeBlueGreenDeploymentsResponse'
    { blueGreenDeployments =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains a list of blue\/green deployments for the user.
describeBlueGreenDeploymentsResponse_blueGreenDeployments :: Lens.Lens' DescribeBlueGreenDeploymentsResponse (Prelude.Maybe [BlueGreenDeployment])
describeBlueGreenDeploymentsResponse_blueGreenDeployments = Lens.lens (\DescribeBlueGreenDeploymentsResponse' {blueGreenDeployments} -> blueGreenDeployments) (\s@DescribeBlueGreenDeploymentsResponse' {} a -> s {blueGreenDeployments = a} :: DescribeBlueGreenDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that can be used in a later
-- DescribeBlueGreenDeployments request.
describeBlueGreenDeploymentsResponse_marker :: Lens.Lens' DescribeBlueGreenDeploymentsResponse (Prelude.Maybe Prelude.Text)
describeBlueGreenDeploymentsResponse_marker = Lens.lens (\DescribeBlueGreenDeploymentsResponse' {marker} -> marker) (\s@DescribeBlueGreenDeploymentsResponse' {} a -> s {marker = a} :: DescribeBlueGreenDeploymentsResponse)

-- | The response's http status code.
describeBlueGreenDeploymentsResponse_httpStatus :: Lens.Lens' DescribeBlueGreenDeploymentsResponse Prelude.Int
describeBlueGreenDeploymentsResponse_httpStatus = Lens.lens (\DescribeBlueGreenDeploymentsResponse' {httpStatus} -> httpStatus) (\s@DescribeBlueGreenDeploymentsResponse' {} a -> s {httpStatus = a} :: DescribeBlueGreenDeploymentsResponse)

instance
  Prelude.NFData
    DescribeBlueGreenDeploymentsResponse
  where
  rnf DescribeBlueGreenDeploymentsResponse' {..} =
    Prelude.rnf blueGreenDeployments
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
