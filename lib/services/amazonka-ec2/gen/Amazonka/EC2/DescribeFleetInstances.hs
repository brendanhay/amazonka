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
-- Module      : Amazonka.EC2.DescribeFleetInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified EC2 Fleet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#monitor-ec2-fleet Monitor your EC2 Fleet>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DescribeFleetInstances
  ( -- * Creating a Request
    DescribeFleetInstances (..),
    newDescribeFleetInstances,

    -- * Request Lenses
    describeFleetInstances_dryRun,
    describeFleetInstances_filters,
    describeFleetInstances_maxResults,
    describeFleetInstances_nextToken,
    describeFleetInstances_fleetId,

    -- * Destructuring the Response
    DescribeFleetInstancesResponse (..),
    newDescribeFleetInstancesResponse,

    -- * Response Lenses
    describeFleetInstancesResponse_activeInstances,
    describeFleetInstancesResponse_fleetId,
    describeFleetInstancesResponse_nextToken,
    describeFleetInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetInstances' smart constructor.
data DescribeFleetInstances = DescribeFleetInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @instance-type@ - The instance type.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and 1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeFleetInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeFleetInstances_filters' - The filters.
--
-- -   @instance-type@ - The instance type.
--
-- 'maxResults', 'describeFleetInstances_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'describeFleetInstances_nextToken' - The token for the next set of results.
--
-- 'fleetId', 'describeFleetInstances_fleetId' - The ID of the EC2 Fleet.
newDescribeFleetInstances ::
  -- | 'fleetId'
  Prelude.Text ->
  DescribeFleetInstances
newDescribeFleetInstances pFleetId_ =
  DescribeFleetInstances'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFleetInstances_dryRun :: Lens.Lens' DescribeFleetInstances (Prelude.Maybe Prelude.Bool)
describeFleetInstances_dryRun = Lens.lens (\DescribeFleetInstances' {dryRun} -> dryRun) (\s@DescribeFleetInstances' {} a -> s {dryRun = a} :: DescribeFleetInstances)

-- | The filters.
--
-- -   @instance-type@ - The instance type.
describeFleetInstances_filters :: Lens.Lens' DescribeFleetInstances (Prelude.Maybe [Filter])
describeFleetInstances_filters = Lens.lens (\DescribeFleetInstances' {filters} -> filters) (\s@DescribeFleetInstances' {} a -> s {filters = a} :: DescribeFleetInstances) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeFleetInstances_maxResults :: Lens.Lens' DescribeFleetInstances (Prelude.Maybe Prelude.Int)
describeFleetInstances_maxResults = Lens.lens (\DescribeFleetInstances' {maxResults} -> maxResults) (\s@DescribeFleetInstances' {} a -> s {maxResults = a} :: DescribeFleetInstances)

-- | The token for the next set of results.
describeFleetInstances_nextToken :: Lens.Lens' DescribeFleetInstances (Prelude.Maybe Prelude.Text)
describeFleetInstances_nextToken = Lens.lens (\DescribeFleetInstances' {nextToken} -> nextToken) (\s@DescribeFleetInstances' {} a -> s {nextToken = a} :: DescribeFleetInstances)

-- | The ID of the EC2 Fleet.
describeFleetInstances_fleetId :: Lens.Lens' DescribeFleetInstances Prelude.Text
describeFleetInstances_fleetId = Lens.lens (\DescribeFleetInstances' {fleetId} -> fleetId) (\s@DescribeFleetInstances' {} a -> s {fleetId = a} :: DescribeFleetInstances)

instance Core.AWSRequest DescribeFleetInstances where
  type
    AWSResponse DescribeFleetInstances =
      DescribeFleetInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFleetInstancesResponse'
            Prelude.<$> ( x Data..@? "activeInstanceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "fleetId")
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetInstances where
  hashWithSalt _salt DescribeFleetInstances' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DescribeFleetInstances where
  rnf DescribeFleetInstances' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders DescribeFleetInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeFleetInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFleetInstances where
  toQuery DescribeFleetInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeFleetInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "FleetId" Data.=: fleetId
      ]

-- | /See:/ 'newDescribeFleetInstancesResponse' smart constructor.
data DescribeFleetInstancesResponse = DescribeFleetInstancesResponse'
  { -- | The running instances. This list is refreshed periodically and might be
    -- out of date.
    activeInstances :: Prelude.Maybe [ActiveInstance],
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeInstances', 'describeFleetInstancesResponse_activeInstances' - The running instances. This list is refreshed periodically and might be
-- out of date.
--
-- 'fleetId', 'describeFleetInstancesResponse_fleetId' - The ID of the EC2 Fleet.
--
-- 'nextToken', 'describeFleetInstancesResponse_nextToken' - The token for the next set of results.
--
-- 'httpStatus', 'describeFleetInstancesResponse_httpStatus' - The response's http status code.
newDescribeFleetInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetInstancesResponse
newDescribeFleetInstancesResponse pHttpStatus_ =
  DescribeFleetInstancesResponse'
    { activeInstances =
        Prelude.Nothing,
      fleetId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The running instances. This list is refreshed periodically and might be
-- out of date.
describeFleetInstancesResponse_activeInstances :: Lens.Lens' DescribeFleetInstancesResponse (Prelude.Maybe [ActiveInstance])
describeFleetInstancesResponse_activeInstances = Lens.lens (\DescribeFleetInstancesResponse' {activeInstances} -> activeInstances) (\s@DescribeFleetInstancesResponse' {} a -> s {activeInstances = a} :: DescribeFleetInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the EC2 Fleet.
describeFleetInstancesResponse_fleetId :: Lens.Lens' DescribeFleetInstancesResponse (Prelude.Maybe Prelude.Text)
describeFleetInstancesResponse_fleetId = Lens.lens (\DescribeFleetInstancesResponse' {fleetId} -> fleetId) (\s@DescribeFleetInstancesResponse' {} a -> s {fleetId = a} :: DescribeFleetInstancesResponse)

-- | The token for the next set of results.
describeFleetInstancesResponse_nextToken :: Lens.Lens' DescribeFleetInstancesResponse (Prelude.Maybe Prelude.Text)
describeFleetInstancesResponse_nextToken = Lens.lens (\DescribeFleetInstancesResponse' {nextToken} -> nextToken) (\s@DescribeFleetInstancesResponse' {} a -> s {nextToken = a} :: DescribeFleetInstancesResponse)

-- | The response's http status code.
describeFleetInstancesResponse_httpStatus :: Lens.Lens' DescribeFleetInstancesResponse Prelude.Int
describeFleetInstancesResponse_httpStatus = Lens.lens (\DescribeFleetInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetInstancesResponse' {} a -> s {httpStatus = a} :: DescribeFleetInstancesResponse)

instance
  Prelude.NFData
    DescribeFleetInstancesResponse
  where
  rnf DescribeFleetInstancesResponse' {..} =
    Prelude.rnf activeInstances
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
