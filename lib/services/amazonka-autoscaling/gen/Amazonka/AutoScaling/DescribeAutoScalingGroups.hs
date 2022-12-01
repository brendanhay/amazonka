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
-- Module      : Amazonka.AutoScaling.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the Auto Scaling groups in the account and
-- Region.
--
-- If you specify Auto Scaling group names, the output includes information
-- for only the specified Auto Scaling groups. If you specify filters, the
-- output includes information for only those Auto Scaling groups that meet
-- the filter criteria. If you do not specify group names or filters, the
-- output includes information for all Auto Scaling groups.
--
-- This operation also returns information about instances in Auto Scaling
-- groups. To retrieve information about the instances in a warm pool, you
-- must call the DescribeWarmPool API.
--
-- This operation returns paginated results.
module Amazonka.AutoScaling.DescribeAutoScalingGroups
  ( -- * Creating a Request
    DescribeAutoScalingGroups (..),
    newDescribeAutoScalingGroups,

    -- * Request Lenses
    describeAutoScalingGroups_nextToken,
    describeAutoScalingGroups_filters,
    describeAutoScalingGroups_autoScalingGroupNames,
    describeAutoScalingGroups_maxRecords,

    -- * Destructuring the Response
    DescribeAutoScalingGroupsResponse (..),
    newDescribeAutoScalingGroupsResponse,

    -- * Response Lenses
    describeAutoScalingGroupsResponse_nextToken,
    describeAutoScalingGroupsResponse_httpStatus,
    describeAutoScalingGroupsResponse_autoScalingGroups,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAutoScalingGroups' smart constructor.
data DescribeAutoScalingGroups = DescribeAutoScalingGroups'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters to limit the results based on specific tags.
    filters :: Prelude.Maybe [Filter],
    -- | The names of the Auto Scaling groups. By default, you can only specify
    -- up to 50 names. You can optionally increase this limit using the
    -- @MaxRecords@ property.
    --
    -- If you omit this property, all Auto Scaling groups are described.
    autoScalingGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoScalingGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAutoScalingGroups_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'filters', 'describeAutoScalingGroups_filters' - One or more filters to limit the results based on specific tags.
--
-- 'autoScalingGroupNames', 'describeAutoScalingGroups_autoScalingGroupNames' - The names of the Auto Scaling groups. By default, you can only specify
-- up to 50 names. You can optionally increase this limit using the
-- @MaxRecords@ property.
--
-- If you omit this property, all Auto Scaling groups are described.
--
-- 'maxRecords', 'describeAutoScalingGroups_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
newDescribeAutoScalingGroups ::
  DescribeAutoScalingGroups
newDescribeAutoScalingGroups =
  DescribeAutoScalingGroups'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      autoScalingGroupNames = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeAutoScalingGroups_nextToken :: Lens.Lens' DescribeAutoScalingGroups (Prelude.Maybe Prelude.Text)
describeAutoScalingGroups_nextToken = Lens.lens (\DescribeAutoScalingGroups' {nextToken} -> nextToken) (\s@DescribeAutoScalingGroups' {} a -> s {nextToken = a} :: DescribeAutoScalingGroups)

-- | One or more filters to limit the results based on specific tags.
describeAutoScalingGroups_filters :: Lens.Lens' DescribeAutoScalingGroups (Prelude.Maybe [Filter])
describeAutoScalingGroups_filters = Lens.lens (\DescribeAutoScalingGroups' {filters} -> filters) (\s@DescribeAutoScalingGroups' {} a -> s {filters = a} :: DescribeAutoScalingGroups) Prelude.. Lens.mapping Lens.coerced

-- | The names of the Auto Scaling groups. By default, you can only specify
-- up to 50 names. You can optionally increase this limit using the
-- @MaxRecords@ property.
--
-- If you omit this property, all Auto Scaling groups are described.
describeAutoScalingGroups_autoScalingGroupNames :: Lens.Lens' DescribeAutoScalingGroups (Prelude.Maybe [Prelude.Text])
describeAutoScalingGroups_autoScalingGroupNames = Lens.lens (\DescribeAutoScalingGroups' {autoScalingGroupNames} -> autoScalingGroupNames) (\s@DescribeAutoScalingGroups' {} a -> s {autoScalingGroupNames = a} :: DescribeAutoScalingGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeAutoScalingGroups_maxRecords :: Lens.Lens' DescribeAutoScalingGroups (Prelude.Maybe Prelude.Int)
describeAutoScalingGroups_maxRecords = Lens.lens (\DescribeAutoScalingGroups' {maxRecords} -> maxRecords) (\s@DescribeAutoScalingGroups' {} a -> s {maxRecords = a} :: DescribeAutoScalingGroups)

instance Core.AWSPager DescribeAutoScalingGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAutoScalingGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeAutoScalingGroupsResponse_autoScalingGroups
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAutoScalingGroups_nextToken
          Lens..~ rs
          Lens.^? describeAutoScalingGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAutoScalingGroups where
  type
    AWSResponse DescribeAutoScalingGroups =
      DescribeAutoScalingGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAutoScalingGroupsResult"
      ( \s h x ->
          DescribeAutoScalingGroupsResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "AutoScalingGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeAutoScalingGroups where
  hashWithSalt _salt DescribeAutoScalingGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` autoScalingGroupNames
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeAutoScalingGroups where
  rnf DescribeAutoScalingGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf autoScalingGroupNames
      `Prelude.seq` Prelude.rnf maxRecords

instance Core.ToHeaders DescribeAutoScalingGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAutoScalingGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAutoScalingGroups where
  toQuery DescribeAutoScalingGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeAutoScalingGroups" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> filters),
        "AutoScalingGroupNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> autoScalingGroupNames
            ),
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeAutoScalingGroupsResponse' smart constructor.
data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The groups.
    autoScalingGroups :: [AutoScalingGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoScalingGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAutoScalingGroupsResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'httpStatus', 'describeAutoScalingGroupsResponse_httpStatus' - The response's http status code.
--
-- 'autoScalingGroups', 'describeAutoScalingGroupsResponse_autoScalingGroups' - The groups.
newDescribeAutoScalingGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAutoScalingGroupsResponse
newDescribeAutoScalingGroupsResponse pHttpStatus_ =
  DescribeAutoScalingGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      autoScalingGroups = Prelude.mempty
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeAutoScalingGroupsResponse_nextToken :: Lens.Lens' DescribeAutoScalingGroupsResponse (Prelude.Maybe Prelude.Text)
describeAutoScalingGroupsResponse_nextToken = Lens.lens (\DescribeAutoScalingGroupsResponse' {nextToken} -> nextToken) (\s@DescribeAutoScalingGroupsResponse' {} a -> s {nextToken = a} :: DescribeAutoScalingGroupsResponse)

-- | The response's http status code.
describeAutoScalingGroupsResponse_httpStatus :: Lens.Lens' DescribeAutoScalingGroupsResponse Prelude.Int
describeAutoScalingGroupsResponse_httpStatus = Lens.lens (\DescribeAutoScalingGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoScalingGroupsResponse' {} a -> s {httpStatus = a} :: DescribeAutoScalingGroupsResponse)

-- | The groups.
describeAutoScalingGroupsResponse_autoScalingGroups :: Lens.Lens' DescribeAutoScalingGroupsResponse [AutoScalingGroup]
describeAutoScalingGroupsResponse_autoScalingGroups = Lens.lens (\DescribeAutoScalingGroupsResponse' {autoScalingGroups} -> autoScalingGroups) (\s@DescribeAutoScalingGroupsResponse' {} a -> s {autoScalingGroups = a} :: DescribeAutoScalingGroupsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeAutoScalingGroupsResponse
  where
  rnf DescribeAutoScalingGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf autoScalingGroups
