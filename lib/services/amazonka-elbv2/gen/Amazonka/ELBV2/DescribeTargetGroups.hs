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
-- Module      : Amazonka.ELBV2.DescribeTargetGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified target groups or all of your target groups. By
-- default, all target groups are described. Alternatively, you can specify
-- one of the following to filter the results: the ARN of the load
-- balancer, the names of one or more target groups, or the ARNs of one or
-- more target groups.
--
-- This operation returns paginated results.
module Amazonka.ELBV2.DescribeTargetGroups
  ( -- * Creating a Request
    DescribeTargetGroups (..),
    newDescribeTargetGroups,

    -- * Request Lenses
    describeTargetGroups_marker,
    describeTargetGroups_loadBalancerArn,
    describeTargetGroups_names,
    describeTargetGroups_targetGroupArns,
    describeTargetGroups_pageSize,

    -- * Destructuring the Response
    DescribeTargetGroupsResponse (..),
    newDescribeTargetGroupsResponse,

    -- * Response Lenses
    describeTargetGroupsResponse_nextMarker,
    describeTargetGroupsResponse_targetGroups,
    describeTargetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTargetGroups' smart constructor.
data DescribeTargetGroups = DescribeTargetGroups'
  { -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The names of the target groups.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARN) of the target groups.
    targetGroupArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTargetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeTargetGroups_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'loadBalancerArn', 'describeTargetGroups_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'names', 'describeTargetGroups_names' - The names of the target groups.
--
-- 'targetGroupArns', 'describeTargetGroups_targetGroupArns' - The Amazon Resource Names (ARN) of the target groups.
--
-- 'pageSize', 'describeTargetGroups_pageSize' - The maximum number of results to return with this call.
newDescribeTargetGroups ::
  DescribeTargetGroups
newDescribeTargetGroups =
  DescribeTargetGroups'
    { marker = Prelude.Nothing,
      loadBalancerArn = Prelude.Nothing,
      names = Prelude.Nothing,
      targetGroupArns = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeTargetGroups_marker :: Lens.Lens' DescribeTargetGroups (Prelude.Maybe Prelude.Text)
describeTargetGroups_marker = Lens.lens (\DescribeTargetGroups' {marker} -> marker) (\s@DescribeTargetGroups' {} a -> s {marker = a} :: DescribeTargetGroups)

-- | The Amazon Resource Name (ARN) of the load balancer.
describeTargetGroups_loadBalancerArn :: Lens.Lens' DescribeTargetGroups (Prelude.Maybe Prelude.Text)
describeTargetGroups_loadBalancerArn = Lens.lens (\DescribeTargetGroups' {loadBalancerArn} -> loadBalancerArn) (\s@DescribeTargetGroups' {} a -> s {loadBalancerArn = a} :: DescribeTargetGroups)

-- | The names of the target groups.
describeTargetGroups_names :: Lens.Lens' DescribeTargetGroups (Prelude.Maybe [Prelude.Text])
describeTargetGroups_names = Lens.lens (\DescribeTargetGroups' {names} -> names) (\s@DescribeTargetGroups' {} a -> s {names = a} :: DescribeTargetGroups) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARN) of the target groups.
describeTargetGroups_targetGroupArns :: Lens.Lens' DescribeTargetGroups (Prelude.Maybe [Prelude.Text])
describeTargetGroups_targetGroupArns = Lens.lens (\DescribeTargetGroups' {targetGroupArns} -> targetGroupArns) (\s@DescribeTargetGroups' {} a -> s {targetGroupArns = a} :: DescribeTargetGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with this call.
describeTargetGroups_pageSize :: Lens.Lens' DescribeTargetGroups (Prelude.Maybe Prelude.Natural)
describeTargetGroups_pageSize = Lens.lens (\DescribeTargetGroups' {pageSize} -> pageSize) (\s@DescribeTargetGroups' {} a -> s {pageSize = a} :: DescribeTargetGroups)

instance Core.AWSPager DescribeTargetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTargetGroupsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTargetGroupsResponse_targetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTargetGroups_marker
          Lens..~ rs
          Lens.^? describeTargetGroupsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeTargetGroups where
  type
    AWSResponse DescribeTargetGroups =
      DescribeTargetGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTargetGroupsResult"
      ( \s h x ->
          DescribeTargetGroupsResponse'
            Prelude.<$> (x Core..@? "NextMarker")
            Prelude.<*> ( x Core..@? "TargetGroups" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTargetGroups where
  hashWithSalt _salt DescribeTargetGroups' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` loadBalancerArn
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` targetGroupArns
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData DescribeTargetGroups where
  rnf DescribeTargetGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf loadBalancerArn
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf targetGroupArns
      `Prelude.seq` Prelude.rnf pageSize

instance Core.ToHeaders DescribeTargetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeTargetGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTargetGroups where
  toQuery DescribeTargetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeTargetGroups" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "LoadBalancerArn" Core.=: loadBalancerArn,
        "Names"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> names),
        "TargetGroupArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> targetGroupArns
            ),
        "PageSize" Core.=: pageSize
      ]

-- | /See:/ 'newDescribeTargetGroupsResponse' smart constructor.
data DescribeTargetGroupsResponse = DescribeTargetGroupsResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Information about the target groups.
    targetGroups :: Prelude.Maybe [TargetGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTargetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'describeTargetGroupsResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'targetGroups', 'describeTargetGroupsResponse_targetGroups' - Information about the target groups.
--
-- 'httpStatus', 'describeTargetGroupsResponse_httpStatus' - The response's http status code.
newDescribeTargetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTargetGroupsResponse
newDescribeTargetGroupsResponse pHttpStatus_ =
  DescribeTargetGroupsResponse'
    { nextMarker =
        Prelude.Nothing,
      targetGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeTargetGroupsResponse_nextMarker :: Lens.Lens' DescribeTargetGroupsResponse (Prelude.Maybe Prelude.Text)
describeTargetGroupsResponse_nextMarker = Lens.lens (\DescribeTargetGroupsResponse' {nextMarker} -> nextMarker) (\s@DescribeTargetGroupsResponse' {} a -> s {nextMarker = a} :: DescribeTargetGroupsResponse)

-- | Information about the target groups.
describeTargetGroupsResponse_targetGroups :: Lens.Lens' DescribeTargetGroupsResponse (Prelude.Maybe [TargetGroup])
describeTargetGroupsResponse_targetGroups = Lens.lens (\DescribeTargetGroupsResponse' {targetGroups} -> targetGroups) (\s@DescribeTargetGroupsResponse' {} a -> s {targetGroups = a} :: DescribeTargetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTargetGroupsResponse_httpStatus :: Lens.Lens' DescribeTargetGroupsResponse Prelude.Int
describeTargetGroupsResponse_httpStatus = Lens.lens (\DescribeTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeTargetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeTargetGroupsResponse)

instance Prelude.NFData DescribeTargetGroupsResponse where
  rnf DescribeTargetGroupsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf targetGroups
      `Prelude.seq` Prelude.rnf httpStatus
