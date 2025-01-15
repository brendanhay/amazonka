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
-- Module      : Amazonka.AutoScaling.DescribeTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-tagging.html Tag Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- This operation returns paginated results.
module Amazonka.AutoScaling.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_filters,
    describeTags_maxRecords,
    describeTags_nextToken,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | One or more filters to scope the tags to return. The maximum number of
    -- filters per filter type (for example, @auto-scaling-group@) is 1000.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeTags_filters' - One or more filters to scope the tags to return. The maximum number of
-- filters per filter type (for example, @auto-scaling-group@) is 1000.
--
-- 'maxRecords', 'describeTags_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
--
-- 'nextToken', 'describeTags_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newDescribeTags ::
  DescribeTags
newDescribeTags =
  DescribeTags'
    { filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | One or more filters to scope the tags to return. The maximum number of
-- filters per filter type (for example, @auto-scaling-group@) is 1000.
describeTags_filters :: Lens.Lens' DescribeTags (Prelude.Maybe [Filter])
describeTags_filters = Lens.lens (\DescribeTags' {filters} -> filters) (\s@DescribeTags' {} a -> s {filters = a} :: DescribeTags) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeTags_maxRecords :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Int)
describeTags_maxRecords = Lens.lens (\DescribeTags' {maxRecords} -> maxRecords) (\s@DescribeTags' {} a -> s {maxRecords = a} :: DescribeTags)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeTags_nextToken :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Text)
describeTags_nextToken = Lens.lens (\DescribeTags' {nextToken} -> nextToken) (\s@DescribeTags' {} a -> s {nextToken = a} :: DescribeTags)

instance Core.AWSPager DescribeTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_tags
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeTags_nextToken
              Lens..~ rs
              Lens.^? describeTagsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTags where
  hashWithSalt _salt DescribeTags' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeTags where
  rnf DescribeTags' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxRecords `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders DescribeTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTags where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTags where
  toQuery DescribeTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> filters),
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe [TagDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { nextToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeTagsResponse_nextToken :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe Prelude.Text)
describeTagsResponse_nextToken = Lens.lens (\DescribeTagsResponse' {nextToken} -> nextToken) (\s@DescribeTagsResponse' {} a -> s {nextToken = a} :: DescribeTagsResponse)

-- | One or more tags.
describeTagsResponse_tags :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe [TagDescription])
describeTagsResponse_tags = Lens.lens (\DescribeTagsResponse' {tags} -> tags) (\s@DescribeTagsResponse' {} a -> s {tags = a} :: DescribeTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Prelude.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Prelude.NFData DescribeTagsResponse where
  rnf DescribeTagsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf httpStatus
