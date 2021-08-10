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
-- Module      : Network.AWS.EC2.DescribeReservedInstancesModifications
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.DescribeReservedInstancesModifications
  ( -- * Creating a Request
    DescribeReservedInstancesModifications (..),
    newDescribeReservedInstancesModifications,

    -- * Request Lenses
    describeReservedInstancesModifications_nextToken,
    describeReservedInstancesModifications_reservedInstancesModificationIds,
    describeReservedInstancesModifications_filters,

    -- * Destructuring the Response
    DescribeReservedInstancesModificationsResponse (..),
    newDescribeReservedInstancesModificationsResponse,

    -- * Response Lenses
    describeReservedInstancesModificationsResponse_nextToken,
    describeReservedInstancesModificationsResponse_reservedInstancesModifications,
    describeReservedInstancesModificationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeReservedInstancesModifications.
--
-- /See:/ 'newDescribeReservedInstancesModifications' smart constructor.
data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | IDs for the submitted modification request.
    reservedInstancesModificationIds :: Prelude.Maybe [Prelude.Text],
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
    filters :: Prelude.Maybe [Filter]
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
-- 'nextToken', 'describeReservedInstancesModifications_nextToken' - The token to retrieve the next page of results.
--
-- 'reservedInstancesModificationIds', 'describeReservedInstancesModifications_reservedInstancesModificationIds' - IDs for the submitted modification request.
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
newDescribeReservedInstancesModifications ::
  DescribeReservedInstancesModifications
newDescribeReservedInstancesModifications =
  DescribeReservedInstancesModifications'
    { nextToken =
        Prelude.Nothing,
      reservedInstancesModificationIds =
        Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
describeReservedInstancesModifications_nextToken :: Lens.Lens' DescribeReservedInstancesModifications (Prelude.Maybe Prelude.Text)
describeReservedInstancesModifications_nextToken = Lens.lens (\DescribeReservedInstancesModifications' {nextToken} -> nextToken) (\s@DescribeReservedInstancesModifications' {} a -> s {nextToken = a} :: DescribeReservedInstancesModifications)

-- | IDs for the submitted modification request.
describeReservedInstancesModifications_reservedInstancesModificationIds :: Lens.Lens' DescribeReservedInstancesModifications (Prelude.Maybe [Prelude.Text])
describeReservedInstancesModifications_reservedInstancesModificationIds = Lens.lens (\DescribeReservedInstancesModifications' {reservedInstancesModificationIds} -> reservedInstancesModificationIds) (\s@DescribeReservedInstancesModifications' {} a -> s {reservedInstancesModificationIds = a} :: DescribeReservedInstancesModifications) Prelude.. Lens.mapping Lens._Coerce

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
describeReservedInstancesModifications_filters = Lens.lens (\DescribeReservedInstancesModifications' {filters} -> filters) (\s@DescribeReservedInstancesModifications' {} a -> s {filters = a} :: DescribeReservedInstancesModifications) Prelude.. Lens.mapping Lens._Coerce

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeReservedInstancesModificationsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "reservedInstancesModificationsSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedInstancesModifications

instance
  Prelude.NFData
    DescribeReservedInstancesModifications

instance
  Core.ToHeaders
    DescribeReservedInstancesModifications
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeReservedInstancesModifications
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeReservedInstancesModifications
  where
  toQuery DescribeReservedInstancesModifications' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeReservedInstancesModifications" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "ReservedInstancesModificationId"
              Prelude.<$> reservedInstancesModificationIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
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
describeReservedInstancesModificationsResponse_reservedInstancesModifications = Lens.lens (\DescribeReservedInstancesModificationsResponse' {reservedInstancesModifications} -> reservedInstancesModifications) (\s@DescribeReservedInstancesModificationsResponse' {} a -> s {reservedInstancesModifications = a} :: DescribeReservedInstancesModificationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeReservedInstancesModificationsResponse_httpStatus :: Lens.Lens' DescribeReservedInstancesModificationsResponse Prelude.Int
describeReservedInstancesModificationsResponse_httpStatus = Lens.lens (\DescribeReservedInstancesModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedInstancesModificationsResponse' {} a -> s {httpStatus = a} :: DescribeReservedInstancesModificationsResponse)

instance
  Prelude.NFData
    DescribeReservedInstancesModificationsResponse
