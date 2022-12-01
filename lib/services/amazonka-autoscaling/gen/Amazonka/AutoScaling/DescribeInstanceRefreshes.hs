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
-- Module      : Amazonka.AutoScaling.DescribeInstanceRefreshes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the instance refreshes for the specified Auto
-- Scaling group.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html instance refresh feature>
-- in Amazon EC2 Auto Scaling, which helps you update instances in your
-- Auto Scaling group after you make configuration changes.
--
-- To help you determine the status of an instance refresh, this operation
-- returns information about the instance refreshes you previously
-- initiated, including their status, end time, the percentage of the
-- instance refresh that is complete, and the number of instances remaining
-- to update before the instance refresh is complete.
--
-- The following are the possible statuses:
--
-- -   @Pending@ - The request was created, but the operation has not
--     started.
--
-- -   @InProgress@ - The operation is in progress.
--
-- -   @Successful@ - The operation completed successfully.
--
-- -   @Failed@ - The operation failed to complete. You can troubleshoot
--     using the status reason and the scaling activities.
--
-- -   @Cancelling@ - An ongoing operation is being cancelled. Cancellation
--     does not roll back any replacements that have already been
--     completed, but it prevents new replacements from being started.
--
-- -   @Cancelled@ - The operation is cancelled.
module Amazonka.AutoScaling.DescribeInstanceRefreshes
  ( -- * Creating a Request
    DescribeInstanceRefreshes (..),
    newDescribeInstanceRefreshes,

    -- * Request Lenses
    describeInstanceRefreshes_nextToken,
    describeInstanceRefreshes_instanceRefreshIds,
    describeInstanceRefreshes_maxRecords,
    describeInstanceRefreshes_autoScalingGroupName,

    -- * Destructuring the Response
    DescribeInstanceRefreshesResponse (..),
    newDescribeInstanceRefreshesResponse,

    -- * Response Lenses
    describeInstanceRefreshesResponse_nextToken,
    describeInstanceRefreshesResponse_instanceRefreshes,
    describeInstanceRefreshesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceRefreshes' smart constructor.
data DescribeInstanceRefreshes = DescribeInstanceRefreshes'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more instance refresh IDs.
    instanceRefreshIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceRefreshes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceRefreshes_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'instanceRefreshIds', 'describeInstanceRefreshes_instanceRefreshIds' - One or more instance refresh IDs.
--
-- 'maxRecords', 'describeInstanceRefreshes_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
--
-- 'autoScalingGroupName', 'describeInstanceRefreshes_autoScalingGroupName' - The name of the Auto Scaling group.
newDescribeInstanceRefreshes ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  DescribeInstanceRefreshes
newDescribeInstanceRefreshes pAutoScalingGroupName_ =
  DescribeInstanceRefreshes'
    { nextToken =
        Prelude.Nothing,
      instanceRefreshIds = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstanceRefreshes_nextToken :: Lens.Lens' DescribeInstanceRefreshes (Prelude.Maybe Prelude.Text)
describeInstanceRefreshes_nextToken = Lens.lens (\DescribeInstanceRefreshes' {nextToken} -> nextToken) (\s@DescribeInstanceRefreshes' {} a -> s {nextToken = a} :: DescribeInstanceRefreshes)

-- | One or more instance refresh IDs.
describeInstanceRefreshes_instanceRefreshIds :: Lens.Lens' DescribeInstanceRefreshes (Prelude.Maybe [Prelude.Text])
describeInstanceRefreshes_instanceRefreshIds = Lens.lens (\DescribeInstanceRefreshes' {instanceRefreshIds} -> instanceRefreshIds) (\s@DescribeInstanceRefreshes' {} a -> s {instanceRefreshIds = a} :: DescribeInstanceRefreshes) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeInstanceRefreshes_maxRecords :: Lens.Lens' DescribeInstanceRefreshes (Prelude.Maybe Prelude.Int)
describeInstanceRefreshes_maxRecords = Lens.lens (\DescribeInstanceRefreshes' {maxRecords} -> maxRecords) (\s@DescribeInstanceRefreshes' {} a -> s {maxRecords = a} :: DescribeInstanceRefreshes)

-- | The name of the Auto Scaling group.
describeInstanceRefreshes_autoScalingGroupName :: Lens.Lens' DescribeInstanceRefreshes Prelude.Text
describeInstanceRefreshes_autoScalingGroupName = Lens.lens (\DescribeInstanceRefreshes' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeInstanceRefreshes' {} a -> s {autoScalingGroupName = a} :: DescribeInstanceRefreshes)

instance Core.AWSRequest DescribeInstanceRefreshes where
  type
    AWSResponse DescribeInstanceRefreshes =
      DescribeInstanceRefreshesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeInstanceRefreshesResult"
      ( \s h x ->
          DescribeInstanceRefreshesResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "InstanceRefreshes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceRefreshes where
  hashWithSalt _salt DescribeInstanceRefreshes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceRefreshIds
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData DescribeInstanceRefreshes where
  rnf DescribeInstanceRefreshes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceRefreshIds
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Core.ToHeaders DescribeInstanceRefreshes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInstanceRefreshes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInstanceRefreshes where
  toQuery DescribeInstanceRefreshes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeInstanceRefreshes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "InstanceRefreshIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> instanceRefreshIds
            ),
        "MaxRecords" Core.=: maxRecords,
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeInstanceRefreshesResponse' smart constructor.
data DescribeInstanceRefreshesResponse = DescribeInstanceRefreshesResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The instance refreshes for the specified group, sorted by creation
    -- timestamp in descending order.
    instanceRefreshes :: Prelude.Maybe [InstanceRefresh],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceRefreshesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceRefreshesResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'instanceRefreshes', 'describeInstanceRefreshesResponse_instanceRefreshes' - The instance refreshes for the specified group, sorted by creation
-- timestamp in descending order.
--
-- 'httpStatus', 'describeInstanceRefreshesResponse_httpStatus' - The response's http status code.
newDescribeInstanceRefreshesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceRefreshesResponse
newDescribeInstanceRefreshesResponse pHttpStatus_ =
  DescribeInstanceRefreshesResponse'
    { nextToken =
        Prelude.Nothing,
      instanceRefreshes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeInstanceRefreshesResponse_nextToken :: Lens.Lens' DescribeInstanceRefreshesResponse (Prelude.Maybe Prelude.Text)
describeInstanceRefreshesResponse_nextToken = Lens.lens (\DescribeInstanceRefreshesResponse' {nextToken} -> nextToken) (\s@DescribeInstanceRefreshesResponse' {} a -> s {nextToken = a} :: DescribeInstanceRefreshesResponse)

-- | The instance refreshes for the specified group, sorted by creation
-- timestamp in descending order.
describeInstanceRefreshesResponse_instanceRefreshes :: Lens.Lens' DescribeInstanceRefreshesResponse (Prelude.Maybe [InstanceRefresh])
describeInstanceRefreshesResponse_instanceRefreshes = Lens.lens (\DescribeInstanceRefreshesResponse' {instanceRefreshes} -> instanceRefreshes) (\s@DescribeInstanceRefreshesResponse' {} a -> s {instanceRefreshes = a} :: DescribeInstanceRefreshesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstanceRefreshesResponse_httpStatus :: Lens.Lens' DescribeInstanceRefreshesResponse Prelude.Int
describeInstanceRefreshesResponse_httpStatus = Lens.lens (\DescribeInstanceRefreshesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceRefreshesResponse' {} a -> s {httpStatus = a} :: DescribeInstanceRefreshesResponse)

instance
  Prelude.NFData
    DescribeInstanceRefreshesResponse
  where
  rnf DescribeInstanceRefreshesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceRefreshes
      `Prelude.seq` Prelude.rnf httpStatus
