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
-- Module      : Network.AWS.AutoScaling.DescribeTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags.
--
-- You can use filters to limit the results. For example, you can query for
-- the tags for a specific Auto Scaling group. You can specify multiple
-- values for a filter. A tag must match at least one of the specified
-- values for it to be included in the results.
--
-- You can also specify multiple filters. The result includes information
-- for a particular tag only if it matches all the filters. If there\'s no
-- match, no special message is returned.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_nextToken,
    describeTags_filters,
    describeTags_maxRecords,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | One or more filters to scope the tags to return. The maximum number of
    -- filters per filter type (for example, @auto-scaling-group@) is 1000.
    filters :: Core.Maybe [Filter],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTags_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'filters', 'describeTags_filters' - One or more filters to scope the tags to return. The maximum number of
-- filters per filter type (for example, @auto-scaling-group@) is 1000.
--
-- 'maxRecords', 'describeTags_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
newDescribeTags ::
  DescribeTags
newDescribeTags =
  DescribeTags'
    { nextToken = Core.Nothing,
      filters = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeTags_nextToken :: Lens.Lens' DescribeTags (Core.Maybe Core.Text)
describeTags_nextToken = Lens.lens (\DescribeTags' {nextToken} -> nextToken) (\s@DescribeTags' {} a -> s {nextToken = a} :: DescribeTags)

-- | One or more filters to scope the tags to return. The maximum number of
-- filters per filter type (for example, @auto-scaling-group@) is 1000.
describeTags_filters :: Lens.Lens' DescribeTags (Core.Maybe [Filter])
describeTags_filters = Lens.lens (\DescribeTags' {filters} -> filters) (\s@DescribeTags' {} a -> s {filters = a} :: DescribeTags) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeTags_maxRecords :: Lens.Lens' DescribeTags (Core.Maybe Core.Int)
describeTags_maxRecords = Lens.lens (\DescribeTags' {maxRecords} -> maxRecords) (\s@DescribeTags' {} a -> s {maxRecords = a} :: DescribeTags)

instance Core.AWSPager DescribeTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_tags Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTags_nextToken
          Lens..~ rs
          Lens.^? describeTagsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTags

instance Core.NFData DescribeTags

instance Core.ToHeaders DescribeTags where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeTags where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTags where
  toQuery DescribeTags' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeTags" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> filters),
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | One or more tags.
    tags :: Core.Maybe [TagDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTagsResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'tags', 'describeTagsResponse_tags' - One or more tags.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { nextToken = Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeTagsResponse_nextToken :: Lens.Lens' DescribeTagsResponse (Core.Maybe Core.Text)
describeTagsResponse_nextToken = Lens.lens (\DescribeTagsResponse' {nextToken} -> nextToken) (\s@DescribeTagsResponse' {} a -> s {nextToken = a} :: DescribeTagsResponse)

-- | One or more tags.
describeTagsResponse_tags :: Lens.Lens' DescribeTagsResponse (Core.Maybe [TagDescription])
describeTagsResponse_tags = Lens.lens (\DescribeTagsResponse' {tags} -> tags) (\s@DescribeTagsResponse' {} a -> s {tags = a} :: DescribeTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Core.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Core.NFData DescribeTagsResponse
