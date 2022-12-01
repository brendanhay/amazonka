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
-- Module      : Amazonka.AutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the scaling activities in the account and Region.
--
-- When scaling events occur, you see a record of the scaling activity in
-- the scaling activities. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-verify-scaling-activity.html Verifying a scaling activity for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If the scaling event succeeds, the value of the @StatusCode@ element in
-- the response is @Successful@. If an attempt to launch instances failed,
-- the @StatusCode@ value is @Failed@ or @Cancelled@ and the
-- @StatusMessage@ element in the response indicates the cause of the
-- failure. For help interpreting the @StatusMessage@, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/CHAP_Troubleshooting.html Troubleshooting Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- This operation returns paginated results.
module Amazonka.AutoScaling.DescribeScalingActivities
  ( -- * Creating a Request
    DescribeScalingActivities (..),
    newDescribeScalingActivities,

    -- * Request Lenses
    describeScalingActivities_nextToken,
    describeScalingActivities_includeDeletedGroups,
    describeScalingActivities_maxRecords,
    describeScalingActivities_autoScalingGroupName,
    describeScalingActivities_activityIds,

    -- * Destructuring the Response
    DescribeScalingActivitiesResponse (..),
    newDescribeScalingActivitiesResponse,

    -- * Response Lenses
    describeScalingActivitiesResponse_nextToken,
    describeScalingActivitiesResponse_httpStatus,
    describeScalingActivitiesResponse_activities,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to include scaling activity from deleted Auto Scaling
    -- groups.
    includeDeletedGroups :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of items to return with this call. The default value
    -- is @100@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The activity IDs of the desired scaling activities. If you omit this
    -- property, all activities for the past six weeks are described. If
    -- unknown activities are requested, they are ignored with no error. If you
    -- specify an Auto Scaling group, the results are limited to that group.
    --
    -- Array Members: Maximum number of 50 IDs.
    activityIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingActivities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingActivities_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'includeDeletedGroups', 'describeScalingActivities_includeDeletedGroups' - Indicates whether to include scaling activity from deleted Auto Scaling
-- groups.
--
-- 'maxRecords', 'describeScalingActivities_maxRecords' - The maximum number of items to return with this call. The default value
-- is @100@ and the maximum value is @100@.
--
-- 'autoScalingGroupName', 'describeScalingActivities_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'activityIds', 'describeScalingActivities_activityIds' - The activity IDs of the desired scaling activities. If you omit this
-- property, all activities for the past six weeks are described. If
-- unknown activities are requested, they are ignored with no error. If you
-- specify an Auto Scaling group, the results are limited to that group.
--
-- Array Members: Maximum number of 50 IDs.
newDescribeScalingActivities ::
  DescribeScalingActivities
newDescribeScalingActivities =
  DescribeScalingActivities'
    { nextToken =
        Prelude.Nothing,
      includeDeletedGroups = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing,
      activityIds = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeScalingActivities_nextToken :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe Prelude.Text)
describeScalingActivities_nextToken = Lens.lens (\DescribeScalingActivities' {nextToken} -> nextToken) (\s@DescribeScalingActivities' {} a -> s {nextToken = a} :: DescribeScalingActivities)

-- | Indicates whether to include scaling activity from deleted Auto Scaling
-- groups.
describeScalingActivities_includeDeletedGroups :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe Prelude.Bool)
describeScalingActivities_includeDeletedGroups = Lens.lens (\DescribeScalingActivities' {includeDeletedGroups} -> includeDeletedGroups) (\s@DescribeScalingActivities' {} a -> s {includeDeletedGroups = a} :: DescribeScalingActivities)

-- | The maximum number of items to return with this call. The default value
-- is @100@ and the maximum value is @100@.
describeScalingActivities_maxRecords :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe Prelude.Int)
describeScalingActivities_maxRecords = Lens.lens (\DescribeScalingActivities' {maxRecords} -> maxRecords) (\s@DescribeScalingActivities' {} a -> s {maxRecords = a} :: DescribeScalingActivities)

-- | The name of the Auto Scaling group.
describeScalingActivities_autoScalingGroupName :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe Prelude.Text)
describeScalingActivities_autoScalingGroupName = Lens.lens (\DescribeScalingActivities' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeScalingActivities' {} a -> s {autoScalingGroupName = a} :: DescribeScalingActivities)

-- | The activity IDs of the desired scaling activities. If you omit this
-- property, all activities for the past six weeks are described. If
-- unknown activities are requested, they are ignored with no error. If you
-- specify an Auto Scaling group, the results are limited to that group.
--
-- Array Members: Maximum number of 50 IDs.
describeScalingActivities_activityIds :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe [Prelude.Text])
describeScalingActivities_activityIds = Lens.lens (\DescribeScalingActivities' {activityIds} -> activityIds) (\s@DescribeScalingActivities' {} a -> s {activityIds = a} :: DescribeScalingActivities) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeScalingActivities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScalingActivitiesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeScalingActivitiesResponse_activities
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeScalingActivities_nextToken
          Lens..~ rs
          Lens.^? describeScalingActivitiesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeScalingActivities where
  type
    AWSResponse DescribeScalingActivities =
      DescribeScalingActivitiesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeScalingActivitiesResult"
      ( \s h x ->
          DescribeScalingActivitiesResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "Activities" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeScalingActivities where
  hashWithSalt _salt DescribeScalingActivities' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` includeDeletedGroups
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` activityIds

instance Prelude.NFData DescribeScalingActivities where
  rnf DescribeScalingActivities' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf includeDeletedGroups
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf activityIds

instance Core.ToHeaders DescribeScalingActivities where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeScalingActivities where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeScalingActivities where
  toQuery DescribeScalingActivities' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeScalingActivities" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "IncludeDeletedGroups" Core.=: includeDeletedGroups,
        "MaxRecords" Core.=: maxRecords,
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "ActivityIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> activityIds)
      ]

-- | /See:/ 'newDescribeScalingActivitiesResponse' smart constructor.
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The scaling activities. Activities are sorted by start time. Activities
    -- still in progress are described first.
    activities :: [Activity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingActivitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingActivitiesResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'httpStatus', 'describeScalingActivitiesResponse_httpStatus' - The response's http status code.
--
-- 'activities', 'describeScalingActivitiesResponse_activities' - The scaling activities. Activities are sorted by start time. Activities
-- still in progress are described first.
newDescribeScalingActivitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScalingActivitiesResponse
newDescribeScalingActivitiesResponse pHttpStatus_ =
  DescribeScalingActivitiesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      activities = Prelude.mempty
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeScalingActivitiesResponse_nextToken :: Lens.Lens' DescribeScalingActivitiesResponse (Prelude.Maybe Prelude.Text)
describeScalingActivitiesResponse_nextToken = Lens.lens (\DescribeScalingActivitiesResponse' {nextToken} -> nextToken) (\s@DescribeScalingActivitiesResponse' {} a -> s {nextToken = a} :: DescribeScalingActivitiesResponse)

-- | The response's http status code.
describeScalingActivitiesResponse_httpStatus :: Lens.Lens' DescribeScalingActivitiesResponse Prelude.Int
describeScalingActivitiesResponse_httpStatus = Lens.lens (\DescribeScalingActivitiesResponse' {httpStatus} -> httpStatus) (\s@DescribeScalingActivitiesResponse' {} a -> s {httpStatus = a} :: DescribeScalingActivitiesResponse)

-- | The scaling activities. Activities are sorted by start time. Activities
-- still in progress are described first.
describeScalingActivitiesResponse_activities :: Lens.Lens' DescribeScalingActivitiesResponse [Activity]
describeScalingActivitiesResponse_activities = Lens.lens (\DescribeScalingActivitiesResponse' {activities} -> activities) (\s@DescribeScalingActivitiesResponse' {} a -> s {activities = a} :: DescribeScalingActivitiesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeScalingActivitiesResponse
  where
  rnf DescribeScalingActivitiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf activities
