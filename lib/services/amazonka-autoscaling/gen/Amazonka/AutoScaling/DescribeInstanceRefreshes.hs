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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- To help you determine the status of an instance refresh, Amazon EC2 Auto
-- Scaling returns information about the instance refreshes you previously
-- initiated, including their status, start time, end time, the percentage
-- of the instance refresh that is complete, and the number of instances
-- remaining to update before the instance refresh is complete. If a
-- rollback is initiated while an instance refresh is in progress, Amazon
-- EC2 Auto Scaling also returns information about the rollback of the
-- instance refresh.
module Amazonka.AutoScaling.DescribeInstanceRefreshes
  ( -- * Creating a Request
    DescribeInstanceRefreshes (..),
    newDescribeInstanceRefreshes,

    -- * Request Lenses
    describeInstanceRefreshes_instanceRefreshIds,
    describeInstanceRefreshes_maxRecords,
    describeInstanceRefreshes_nextToken,
    describeInstanceRefreshes_autoScalingGroupName,

    -- * Destructuring the Response
    DescribeInstanceRefreshesResponse (..),
    newDescribeInstanceRefreshesResponse,

    -- * Response Lenses
    describeInstanceRefreshesResponse_instanceRefreshes,
    describeInstanceRefreshesResponse_nextToken,
    describeInstanceRefreshesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceRefreshes' smart constructor.
data DescribeInstanceRefreshes = DescribeInstanceRefreshes'
  { -- | One or more instance refresh IDs.
    instanceRefreshIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'instanceRefreshIds', 'describeInstanceRefreshes_instanceRefreshIds' - One or more instance refresh IDs.
--
-- 'maxRecords', 'describeInstanceRefreshes_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
--
-- 'nextToken', 'describeInstanceRefreshes_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'autoScalingGroupName', 'describeInstanceRefreshes_autoScalingGroupName' - The name of the Auto Scaling group.
newDescribeInstanceRefreshes ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  DescribeInstanceRefreshes
newDescribeInstanceRefreshes pAutoScalingGroupName_ =
  DescribeInstanceRefreshes'
    { instanceRefreshIds =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more instance refresh IDs.
describeInstanceRefreshes_instanceRefreshIds :: Lens.Lens' DescribeInstanceRefreshes (Prelude.Maybe [Prelude.Text])
describeInstanceRefreshes_instanceRefreshIds = Lens.lens (\DescribeInstanceRefreshes' {instanceRefreshIds} -> instanceRefreshIds) (\s@DescribeInstanceRefreshes' {} a -> s {instanceRefreshIds = a} :: DescribeInstanceRefreshes) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeInstanceRefreshes_maxRecords :: Lens.Lens' DescribeInstanceRefreshes (Prelude.Maybe Prelude.Int)
describeInstanceRefreshes_maxRecords = Lens.lens (\DescribeInstanceRefreshes' {maxRecords} -> maxRecords) (\s@DescribeInstanceRefreshes' {} a -> s {maxRecords = a} :: DescribeInstanceRefreshes)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstanceRefreshes_nextToken :: Lens.Lens' DescribeInstanceRefreshes (Prelude.Maybe Prelude.Text)
describeInstanceRefreshes_nextToken = Lens.lens (\DescribeInstanceRefreshes' {nextToken} -> nextToken) (\s@DescribeInstanceRefreshes' {} a -> s {nextToken = a} :: DescribeInstanceRefreshes)

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
            Prelude.<$> ( x
                            Data..@? "InstanceRefreshes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceRefreshes where
  hashWithSalt _salt DescribeInstanceRefreshes' {..} =
    _salt
      `Prelude.hashWithSalt` instanceRefreshIds
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData DescribeInstanceRefreshes where
  rnf DescribeInstanceRefreshes' {..} =
    Prelude.rnf instanceRefreshIds
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders DescribeInstanceRefreshes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeInstanceRefreshes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstanceRefreshes where
  toQuery DescribeInstanceRefreshes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeInstanceRefreshes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "InstanceRefreshIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> instanceRefreshIds
            ),
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeInstanceRefreshesResponse' smart constructor.
data DescribeInstanceRefreshesResponse = DescribeInstanceRefreshesResponse'
  { -- | The instance refreshes for the specified group, sorted by creation
    -- timestamp in descending order.
    instanceRefreshes :: Prelude.Maybe [InstanceRefresh],
    -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'instanceRefreshes', 'describeInstanceRefreshesResponse_instanceRefreshes' - The instance refreshes for the specified group, sorted by creation
-- timestamp in descending order.
--
-- 'nextToken', 'describeInstanceRefreshesResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'httpStatus', 'describeInstanceRefreshesResponse_httpStatus' - The response's http status code.
newDescribeInstanceRefreshesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceRefreshesResponse
newDescribeInstanceRefreshesResponse pHttpStatus_ =
  DescribeInstanceRefreshesResponse'
    { instanceRefreshes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance refreshes for the specified group, sorted by creation
-- timestamp in descending order.
describeInstanceRefreshesResponse_instanceRefreshes :: Lens.Lens' DescribeInstanceRefreshesResponse (Prelude.Maybe [InstanceRefresh])
describeInstanceRefreshesResponse_instanceRefreshes = Lens.lens (\DescribeInstanceRefreshesResponse' {instanceRefreshes} -> instanceRefreshes) (\s@DescribeInstanceRefreshesResponse' {} a -> s {instanceRefreshes = a} :: DescribeInstanceRefreshesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeInstanceRefreshesResponse_nextToken :: Lens.Lens' DescribeInstanceRefreshesResponse (Prelude.Maybe Prelude.Text)
describeInstanceRefreshesResponse_nextToken = Lens.lens (\DescribeInstanceRefreshesResponse' {nextToken} -> nextToken) (\s@DescribeInstanceRefreshesResponse' {} a -> s {nextToken = a} :: DescribeInstanceRefreshesResponse)

-- | The response's http status code.
describeInstanceRefreshesResponse_httpStatus :: Lens.Lens' DescribeInstanceRefreshesResponse Prelude.Int
describeInstanceRefreshesResponse_httpStatus = Lens.lens (\DescribeInstanceRefreshesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceRefreshesResponse' {} a -> s {httpStatus = a} :: DescribeInstanceRefreshesResponse)

instance
  Prelude.NFData
    DescribeInstanceRefreshesResponse
  where
  rnf DescribeInstanceRefreshesResponse' {..} =
    Prelude.rnf instanceRefreshes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
