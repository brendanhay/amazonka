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
-- Module      : Amazonka.EC2.DescribeReservedInstancesModifications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the modifications made to your Reserved Instances. If no
-- parameter is specified, information about all your Reserved Instances
-- modification requests is returned. If a modification ID is specified,
-- only information about the specific modification is returned.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances>
-- in the /Amazon EC2 User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeReservedInstancesModifications
  ( -- * Creating a Request
    DescribeReservedInstancesModifications (..),
    newDescribeReservedInstancesModifications,

    -- * Request Lenses
    describeReservedInstancesModifications_filters,
    describeReservedInstancesModifications_nextToken,
    describeReservedInstancesModifications_reservedInstancesModificationIds,

    -- * Destructuring the Response
    DescribeReservedInstancesModificationsResponse (..),
    newDescribeReservedInstancesModificationsResponse,

    -- * Response Lenses
    describeReservedInstancesModificationsResponse_nextToken,
    describeReservedInstancesModificationsResponse_reservedInstancesModifications,
    describeReservedInstancesModificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeReservedInstancesModifications.
--
-- /See:/ 'newDescribeReservedInstancesModifications' smart constructor.
data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications'
  { -- | One or more filters.
    --
    -- -   @client-token@ - The idempotency token for the modification request.
    --
    -- -   @create-date@ - The time when the modification request was created.
    --
    -- -   @effective-date@ - The time when the modification becomes effective.
    --
    -- -   @modification-result.reserved-instances-id@ - The ID for the
    --     Reserved Instances created as part of the modification request. This
    --     ID is only available when the status of the modification is
    --     @fulfilled@.
    --
    -- -   @modification-result.target-configuration.availability-zone@ - The
    --     Availability Zone for the new Reserved Instances.
    --
    -- -   @modification-result.target-configuration.instance-count @ - The
    --     number of new Reserved Instances.
    --
    -- -   @modification-result.target-configuration.instance-type@ - The
    --     instance type of the new Reserved Instances.
    --
    -- -   @modification-result.target-configuration.platform@ - The network
    --     platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@).
    --
    -- -   @reserved-instances-id@ - The ID of the Reserved Instances modified.
    --
    -- -   @reserved-instances-modification-id@ - The ID of the modification
    --     request.
    --
    -- -   @status@ - The status of the Reserved Instances modification request
    --     (@processing@ | @fulfilled@ | @failed@).
    --
    -- -   @status-message@ - The reason for the status.
    --
    -- -   @update-date@ - The time when the modification request was last
    --     updated.
    filters :: Prelude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | IDs for the submitted modification request.
    reservedInstancesModificationIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstancesModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeReservedInstancesModifications_filters' - One or more filters.
--
-- -   @client-token@ - The idempotency token for the modification request.
--
-- -   @create-date@ - The time when the modification request was created.
--
-- -   @effective-date@ - The time when the modification becomes effective.
--
-- -   @modification-result.reserved-instances-id@ - The ID for the
--     Reserved Instances created as part of the modification request. This
--     ID is only available when the status of the modification is
--     @fulfilled@.
--
-- -   @modification-result.target-configuration.availability-zone@ - The
--     Availability Zone for the new Reserved Instances.
--
-- -   @modification-result.target-configuration.instance-count @ - The
--     number of new Reserved Instances.
--
-- -   @modification-result.target-configuration.instance-type@ - The
--     instance type of the new Reserved Instances.
--
-- -   @modification-result.target-configuration.platform@ - The network
--     platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@).
--
-- -   @reserved-instances-id@ - The ID of the Reserved Instances modified.
--
-- -   @reserved-instances-modification-id@ - The ID of the modification
--     request.
--
-- -   @status@ - The status of the Reserved Instances modification request
--     (@processing@ | @fulfilled@ | @failed@).
--
-- -   @status-message@ - The reason for the status.
--
-- -   @update-date@ - The time when the modification request was last
--     updated.
--
-- 'nextToken', 'describeReservedInstancesModifications_nextToken' - The token to retrieve the next page of results.
--
-- 'reservedInstancesModificationIds', 'describeReservedInstancesModifications_reservedInstancesModificationIds' - IDs for the submitted modification request.
newDescribeReservedInstancesModifications ::
  DescribeReservedInstancesModifications
newDescribeReservedInstancesModifications =
  DescribeReservedInstancesModifications'
    { filters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      reservedInstancesModificationIds =
        Prelude.Nothing
    }

-- | One or more filters.
--
-- -   @client-token@ - The idempotency token for the modification request.
--
-- -   @create-date@ - The time when the modification request was created.
--
-- -   @effective-date@ - The time when the modification becomes effective.
--
-- -   @modification-result.reserved-instances-id@ - The ID for the
--     Reserved Instances created as part of the modification request. This
--     ID is only available when the status of the modification is
--     @fulfilled@.
--
-- -   @modification-result.target-configuration.availability-zone@ - The
--     Availability Zone for the new Reserved Instances.
--
-- -   @modification-result.target-configuration.instance-count @ - The
--     number of new Reserved Instances.
--
-- -   @modification-result.target-configuration.instance-type@ - The
--     instance type of the new Reserved Instances.
--
-- -   @modification-result.target-configuration.platform@ - The network
--     platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@).
--
-- -   @reserved-instances-id@ - The ID of the Reserved Instances modified.
--
-- -   @reserved-instances-modification-id@ - The ID of the modification
--     request.
--
-- -   @status@ - The status of the Reserved Instances modification request
--     (@processing@ | @fulfilled@ | @failed@).
--
-- -   @status-message@ - The reason for the status.
--
-- -   @update-date@ - The time when the modification request was last
--     updated.
describeReservedInstancesModifications_filters :: Lens.Lens' DescribeReservedInstancesModifications (Prelude.Maybe [Filter])
describeReservedInstancesModifications_filters = Lens.lens (\DescribeReservedInstancesModifications' {filters} -> filters) (\s@DescribeReservedInstancesModifications' {} a -> s {filters = a} :: DescribeReservedInstancesModifications) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next page of results.
describeReservedInstancesModifications_nextToken :: Lens.Lens' DescribeReservedInstancesModifications (Prelude.Maybe Prelude.Text)
describeReservedInstancesModifications_nextToken = Lens.lens (\DescribeReservedInstancesModifications' {nextToken} -> nextToken) (\s@DescribeReservedInstancesModifications' {} a -> s {nextToken = a} :: DescribeReservedInstancesModifications)

-- | IDs for the submitted modification request.
describeReservedInstancesModifications_reservedInstancesModificationIds :: Lens.Lens' DescribeReservedInstancesModifications (Prelude.Maybe [Prelude.Text])
describeReservedInstancesModifications_reservedInstancesModificationIds = Lens.lens (\DescribeReservedInstancesModifications' {reservedInstancesModificationIds} -> reservedInstancesModificationIds) (\s@DescribeReservedInstancesModifications' {} a -> s {reservedInstancesModificationIds = a} :: DescribeReservedInstancesModifications) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeReservedInstancesModifications
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedInstancesModificationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedInstancesModificationsResponse_reservedInstancesModifications
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReservedInstancesModifications_nextToken
          Lens..~ rs
            Lens.^? describeReservedInstancesModificationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedInstancesModifications
  where
  type
    AWSResponse
      DescribeReservedInstancesModifications =
      DescribeReservedInstancesModificationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeReservedInstancesModificationsResponse'
            Prelude.<$> (x Data..@? "nextToken")
              Prelude.<*> ( x Data..@? "reservedInstancesModificationsSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Data.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedInstancesModifications
  where
  hashWithSalt
    _salt
    DescribeReservedInstancesModifications' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` reservedInstancesModificationIds

instance
  Prelude.NFData
    DescribeReservedInstancesModifications
  where
  rnf DescribeReservedInstancesModifications' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reservedInstancesModificationIds

instance
  Data.ToHeaders
    DescribeReservedInstancesModifications
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeReservedInstancesModifications
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeReservedInstancesModifications
  where
  toQuery DescribeReservedInstancesModifications' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeReservedInstancesModifications" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "ReservedInstancesModificationId"
              Prelude.<$> reservedInstancesModificationIds
          )
      ]

-- | Contains the output of DescribeReservedInstancesModifications.
--
-- /See:/ 'newDescribeReservedInstancesModificationsResponse' smart constructor.
data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Reserved Instance modification information.
    reservedInstancesModifications :: Prelude.Maybe [ReservedInstancesModification],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstancesModificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedInstancesModificationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'reservedInstancesModifications', 'describeReservedInstancesModificationsResponse_reservedInstancesModifications' - The Reserved Instance modification information.
--
-- 'httpStatus', 'describeReservedInstancesModificationsResponse_httpStatus' - The response's http status code.
newDescribeReservedInstancesModificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedInstancesModificationsResponse
newDescribeReservedInstancesModificationsResponse
  pHttpStatus_ =
    DescribeReservedInstancesModificationsResponse'
      { nextToken =
          Prelude.Nothing,
        reservedInstancesModifications =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeReservedInstancesModificationsResponse_nextToken :: Lens.Lens' DescribeReservedInstancesModificationsResponse (Prelude.Maybe Prelude.Text)
describeReservedInstancesModificationsResponse_nextToken = Lens.lens (\DescribeReservedInstancesModificationsResponse' {nextToken} -> nextToken) (\s@DescribeReservedInstancesModificationsResponse' {} a -> s {nextToken = a} :: DescribeReservedInstancesModificationsResponse)

-- | The Reserved Instance modification information.
describeReservedInstancesModificationsResponse_reservedInstancesModifications :: Lens.Lens' DescribeReservedInstancesModificationsResponse (Prelude.Maybe [ReservedInstancesModification])
describeReservedInstancesModificationsResponse_reservedInstancesModifications = Lens.lens (\DescribeReservedInstancesModificationsResponse' {reservedInstancesModifications} -> reservedInstancesModifications) (\s@DescribeReservedInstancesModificationsResponse' {} a -> s {reservedInstancesModifications = a} :: DescribeReservedInstancesModificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedInstancesModificationsResponse_httpStatus :: Lens.Lens' DescribeReservedInstancesModificationsResponse Prelude.Int
describeReservedInstancesModificationsResponse_httpStatus = Lens.lens (\DescribeReservedInstancesModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedInstancesModificationsResponse' {} a -> s {httpStatus = a} :: DescribeReservedInstancesModificationsResponse)

instance
  Prelude.NFData
    DescribeReservedInstancesModificationsResponse
  where
  rnf
    DescribeReservedInstancesModificationsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf reservedInstancesModifications
        `Prelude.seq` Prelude.rnf httpStatus
