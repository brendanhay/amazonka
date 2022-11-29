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
-- Module      : Amazonka.EC2.DescribeInstanceCreditSpecifications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the credit option for CPU usage of the specified burstable
-- performance instances. The credit options are @standard@ and
-- @unlimited@.
--
-- If you do not specify an instance ID, Amazon EC2 returns burstable
-- performance instances with the @unlimited@ credit option, as well as
-- instances that were previously configured as T2, T3, and T3a with the
-- @unlimited@ credit option. For example, if you resize a T2 instance,
-- while it is configured as @unlimited@, to an M4 instance, Amazon EC2
-- returns the M4 instance.
--
-- If you specify one or more instance IDs, Amazon EC2 returns the credit
-- option (@standard@ or @unlimited@) of those instances. If you specify an
-- instance ID that is not valid, such as an instance that is not a
-- burstable performance instance, an error is returned.
--
-- Recently terminated instances might appear in the returned results. This
-- interval is usually less than one hour.
--
-- If an Availability Zone is experiencing a service disruption and you
-- specify instance IDs in the affected zone, or do not specify any
-- instance IDs at all, the call fails. If you specify only instance IDs in
-- an unaffected zone, the call works normally.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeInstanceCreditSpecifications
  ( -- * Creating a Request
    DescribeInstanceCreditSpecifications (..),
    newDescribeInstanceCreditSpecifications,

    -- * Request Lenses
    describeInstanceCreditSpecifications_nextToken,
    describeInstanceCreditSpecifications_filters,
    describeInstanceCreditSpecifications_dryRun,
    describeInstanceCreditSpecifications_maxResults,
    describeInstanceCreditSpecifications_instanceIds,

    -- * Destructuring the Response
    DescribeInstanceCreditSpecificationsResponse (..),
    newDescribeInstanceCreditSpecificationsResponse,

    -- * Response Lenses
    describeInstanceCreditSpecificationsResponse_nextToken,
    describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications,
    describeInstanceCreditSpecificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceCreditSpecifications' smart constructor.
data DescribeInstanceCreditSpecifications = DescribeInstanceCreditSpecifications'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filters.
    --
    -- -   @instance-id@ - The ID of the instance.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 5 and 1000. You cannot specify this
    -- parameter and the instance IDs parameter in the same call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The instance IDs.
    --
    -- Default: Describes all your instances.
    --
    -- Constraints: Maximum 1000 explicitly specified instance IDs.
    instanceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceCreditSpecifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceCreditSpecifications_nextToken' - The token to retrieve the next page of results.
--
-- 'filters', 'describeInstanceCreditSpecifications_filters' - The filters.
--
-- -   @instance-id@ - The ID of the instance.
--
-- 'dryRun', 'describeInstanceCreditSpecifications_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeInstanceCreditSpecifications_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000. You cannot specify this
-- parameter and the instance IDs parameter in the same call.
--
-- 'instanceIds', 'describeInstanceCreditSpecifications_instanceIds' - The instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 1000 explicitly specified instance IDs.
newDescribeInstanceCreditSpecifications ::
  DescribeInstanceCreditSpecifications
newDescribeInstanceCreditSpecifications =
  DescribeInstanceCreditSpecifications'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceIds = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
describeInstanceCreditSpecifications_nextToken :: Lens.Lens' DescribeInstanceCreditSpecifications (Prelude.Maybe Prelude.Text)
describeInstanceCreditSpecifications_nextToken = Lens.lens (\DescribeInstanceCreditSpecifications' {nextToken} -> nextToken) (\s@DescribeInstanceCreditSpecifications' {} a -> s {nextToken = a} :: DescribeInstanceCreditSpecifications)

-- | The filters.
--
-- -   @instance-id@ - The ID of the instance.
describeInstanceCreditSpecifications_filters :: Lens.Lens' DescribeInstanceCreditSpecifications (Prelude.Maybe [Filter])
describeInstanceCreditSpecifications_filters = Lens.lens (\DescribeInstanceCreditSpecifications' {filters} -> filters) (\s@DescribeInstanceCreditSpecifications' {} a -> s {filters = a} :: DescribeInstanceCreditSpecifications) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceCreditSpecifications_dryRun :: Lens.Lens' DescribeInstanceCreditSpecifications (Prelude.Maybe Prelude.Bool)
describeInstanceCreditSpecifications_dryRun = Lens.lens (\DescribeInstanceCreditSpecifications' {dryRun} -> dryRun) (\s@DescribeInstanceCreditSpecifications' {} a -> s {dryRun = a} :: DescribeInstanceCreditSpecifications)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000. You cannot specify this
-- parameter and the instance IDs parameter in the same call.
describeInstanceCreditSpecifications_maxResults :: Lens.Lens' DescribeInstanceCreditSpecifications (Prelude.Maybe Prelude.Natural)
describeInstanceCreditSpecifications_maxResults = Lens.lens (\DescribeInstanceCreditSpecifications' {maxResults} -> maxResults) (\s@DescribeInstanceCreditSpecifications' {} a -> s {maxResults = a} :: DescribeInstanceCreditSpecifications)

-- | The instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 1000 explicitly specified instance IDs.
describeInstanceCreditSpecifications_instanceIds :: Lens.Lens' DescribeInstanceCreditSpecifications (Prelude.Maybe [Prelude.Text])
describeInstanceCreditSpecifications_instanceIds = Lens.lens (\DescribeInstanceCreditSpecifications' {instanceIds} -> instanceIds) (\s@DescribeInstanceCreditSpecifications' {} a -> s {instanceIds = a} :: DescribeInstanceCreditSpecifications) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeInstanceCreditSpecifications
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstanceCreditSpecificationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstanceCreditSpecifications_nextToken
          Lens..~ rs
            Lens.^? describeInstanceCreditSpecificationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeInstanceCreditSpecifications
  where
  type
    AWSResponse DescribeInstanceCreditSpecifications =
      DescribeInstanceCreditSpecificationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceCreditSpecificationsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "instanceCreditSpecificationSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInstanceCreditSpecifications
  where
  hashWithSalt
    _salt
    DescribeInstanceCreditSpecifications' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` instanceIds

instance
  Prelude.NFData
    DescribeInstanceCreditSpecifications
  where
  rnf DescribeInstanceCreditSpecifications' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceIds

instance
  Core.ToHeaders
    DescribeInstanceCreditSpecifications
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeInstanceCreditSpecifications
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeInstanceCreditSpecifications
  where
  toQuery DescribeInstanceCreditSpecifications' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeInstanceCreditSpecifications" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "InstanceId"
              Prelude.<$> instanceIds
          )
      ]

-- | /See:/ 'newDescribeInstanceCreditSpecificationsResponse' smart constructor.
data DescribeInstanceCreditSpecificationsResponse = DescribeInstanceCreditSpecificationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the credit option for CPU usage of an instance.
    instanceCreditSpecifications :: Prelude.Maybe [InstanceCreditSpecification],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceCreditSpecificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceCreditSpecificationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'instanceCreditSpecifications', 'describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications' - Information about the credit option for CPU usage of an instance.
--
-- 'httpStatus', 'describeInstanceCreditSpecificationsResponse_httpStatus' - The response's http status code.
newDescribeInstanceCreditSpecificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceCreditSpecificationsResponse
newDescribeInstanceCreditSpecificationsResponse
  pHttpStatus_ =
    DescribeInstanceCreditSpecificationsResponse'
      { nextToken =
          Prelude.Nothing,
        instanceCreditSpecifications =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeInstanceCreditSpecificationsResponse_nextToken :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse (Prelude.Maybe Prelude.Text)
describeInstanceCreditSpecificationsResponse_nextToken = Lens.lens (\DescribeInstanceCreditSpecificationsResponse' {nextToken} -> nextToken) (\s@DescribeInstanceCreditSpecificationsResponse' {} a -> s {nextToken = a} :: DescribeInstanceCreditSpecificationsResponse)

-- | Information about the credit option for CPU usage of an instance.
describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse (Prelude.Maybe [InstanceCreditSpecification])
describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications = Lens.lens (\DescribeInstanceCreditSpecificationsResponse' {instanceCreditSpecifications} -> instanceCreditSpecifications) (\s@DescribeInstanceCreditSpecificationsResponse' {} a -> s {instanceCreditSpecifications = a} :: DescribeInstanceCreditSpecificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstanceCreditSpecificationsResponse_httpStatus :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse Prelude.Int
describeInstanceCreditSpecificationsResponse_httpStatus = Lens.lens (\DescribeInstanceCreditSpecificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceCreditSpecificationsResponse' {} a -> s {httpStatus = a} :: DescribeInstanceCreditSpecificationsResponse)

instance
  Prelude.NFData
    DescribeInstanceCreditSpecificationsResponse
  where
  rnf DescribeInstanceCreditSpecificationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceCreditSpecifications
      `Prelude.seq` Prelude.rnf httpStatus
