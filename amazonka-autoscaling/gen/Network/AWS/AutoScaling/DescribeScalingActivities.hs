{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more scaling activities for the specified Auto Scaling
-- group.
--
-- To view the scaling activities from the Amazon EC2 Auto Scaling console,
-- choose the __Activity__ tab of the Auto Scaling group. When scaling
-- events occur, you see scaling activity messages in the __Activity
-- history__. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-verify-scaling-activity.html Verifying a scaling activity for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScalingActivities
  ( -- * Creating a Request
    DescribeScalingActivities (..),
    newDescribeScalingActivities,

    -- * Request Lenses
    describeScalingActivities_nextToken,
    describeScalingActivities_activityIds,
    describeScalingActivities_includeDeletedGroups,
    describeScalingActivities_autoScalingGroupName,
    describeScalingActivities_maxRecords,

    -- * Destructuring the Response
    DescribeScalingActivitiesResponse (..),
    newDescribeScalingActivitiesResponse,

    -- * Response Lenses
    describeScalingActivitiesResponse_nextToken,
    describeScalingActivitiesResponse_httpStatus,
    describeScalingActivitiesResponse_activities,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The activity IDs of the desired scaling activities. You can specify up
    -- to 50 IDs. If you omit this parameter, all activities for the past six
    -- weeks are described. If unknown activities are requested, they are
    -- ignored with no error. If you specify an Auto Scaling group, the results
    -- are limited to that group.
    activityIds :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether to include scaling activity from deleted Auto Scaling
    -- groups.
    includeDeletedGroups :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call. The default value
    -- is @100@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'activityIds', 'describeScalingActivities_activityIds' - The activity IDs of the desired scaling activities. You can specify up
-- to 50 IDs. If you omit this parameter, all activities for the past six
-- weeks are described. If unknown activities are requested, they are
-- ignored with no error. If you specify an Auto Scaling group, the results
-- are limited to that group.
--
-- 'includeDeletedGroups', 'describeScalingActivities_includeDeletedGroups' - Indicates whether to include scaling activity from deleted Auto Scaling
-- groups.
--
-- 'autoScalingGroupName', 'describeScalingActivities_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'maxRecords', 'describeScalingActivities_maxRecords' - The maximum number of items to return with this call. The default value
-- is @100@ and the maximum value is @100@.
newDescribeScalingActivities ::
  DescribeScalingActivities
newDescribeScalingActivities =
  DescribeScalingActivities'
    { nextToken =
        Prelude.Nothing,
      activityIds = Prelude.Nothing,
      includeDeletedGroups = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeScalingActivities_nextToken :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe Prelude.Text)
describeScalingActivities_nextToken = Lens.lens (\DescribeScalingActivities' {nextToken} -> nextToken) (\s@DescribeScalingActivities' {} a -> s {nextToken = a} :: DescribeScalingActivities)

-- | The activity IDs of the desired scaling activities. You can specify up
-- to 50 IDs. If you omit this parameter, all activities for the past six
-- weeks are described. If unknown activities are requested, they are
-- ignored with no error. If you specify an Auto Scaling group, the results
-- are limited to that group.
describeScalingActivities_activityIds :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe [Prelude.Text])
describeScalingActivities_activityIds = Lens.lens (\DescribeScalingActivities' {activityIds} -> activityIds) (\s@DescribeScalingActivities' {} a -> s {activityIds = a} :: DescribeScalingActivities) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether to include scaling activity from deleted Auto Scaling
-- groups.
describeScalingActivities_includeDeletedGroups :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe Prelude.Bool)
describeScalingActivities_includeDeletedGroups = Lens.lens (\DescribeScalingActivities' {includeDeletedGroups} -> includeDeletedGroups) (\s@DescribeScalingActivities' {} a -> s {includeDeletedGroups = a} :: DescribeScalingActivities)

-- | The name of the Auto Scaling group.
describeScalingActivities_autoScalingGroupName :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe Prelude.Text)
describeScalingActivities_autoScalingGroupName = Lens.lens (\DescribeScalingActivities' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeScalingActivities' {} a -> s {autoScalingGroupName = a} :: DescribeScalingActivities)

-- | The maximum number of items to return with this call. The default value
-- is @100@ and the maximum value is @100@.
describeScalingActivities_maxRecords :: Lens.Lens' DescribeScalingActivities (Prelude.Maybe Prelude.Int)
describeScalingActivities_maxRecords = Lens.lens (\DescribeScalingActivities' {maxRecords} -> maxRecords) (\s@DescribeScalingActivities' {} a -> s {maxRecords = a} :: DescribeScalingActivities)

instance Pager.AWSPager DescribeScalingActivities where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeScalingActivitiesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^. describeScalingActivitiesResponse_activities
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeScalingActivities_nextToken
          Lens..~ rs
          Lens.^? describeScalingActivitiesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeScalingActivities where
  type
    Rs DescribeScalingActivities =
      DescribeScalingActivitiesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeScalingActivitiesResult"
      ( \s h x ->
          DescribeScalingActivitiesResponse'
            Prelude.<$> (x Prelude..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..@? "Activities"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeScalingActivities

instance Prelude.NFData DescribeScalingActivities

instance Prelude.ToHeaders DescribeScalingActivities where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeScalingActivities where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeScalingActivities where
  toQuery DescribeScalingActivities' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeScalingActivities" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "ActivityIds"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> activityIds
            ),
        "IncludeDeletedGroups"
          Prelude.=: includeDeletedGroups,
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName,
        "MaxRecords" Prelude.=: maxRecords
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeScalingActivitiesResponse_activities = Lens.lens (\DescribeScalingActivitiesResponse' {activities} -> activities) (\s@DescribeScalingActivitiesResponse' {} a -> s {activities = a} :: DescribeScalingActivitiesResponse) Prelude.. Prelude._Coerce

instance
  Prelude.NFData
    DescribeScalingActivitiesResponse
