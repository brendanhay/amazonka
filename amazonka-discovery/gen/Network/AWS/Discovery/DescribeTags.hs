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
-- Module      : Network.AWS.Discovery.DescribeTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items that have tags as specified by
-- the key-value pairs, name and value, passed to the optional parameter
-- @filters@.
--
-- There are three valid tag filter names:
--
-- -   tagKey
--
-- -   tagValue
--
-- -   configurationId
--
-- Also, all configuration items associated with your user account that
-- have tags can be listed if you call @DescribeTags@ as is without passing
-- any parameters.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_nextToken,
    describeTags_maxResults,
    describeTags_filters,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of items to return in a single page of output. The
    -- maximum value is 100.
    maxResults :: Core.Maybe Core.Int,
    -- | You can filter the list using a /key/-/value/ format. You can separate
    -- these items by using logical operators. Allowed filters include
    -- @tagKey@, @tagValue@, and @configurationId@.
    filters :: Core.Maybe [TagFilter]
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
-- 'nextToken', 'describeTags_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeTags_maxResults' - The total number of items to return in a single page of output. The
-- maximum value is 100.
--
-- 'filters', 'describeTags_filters' - You can filter the list using a /key/-/value/ format. You can separate
-- these items by using logical operators. Allowed filters include
-- @tagKey@, @tagValue@, and @configurationId@.
newDescribeTags ::
  DescribeTags
newDescribeTags =
  DescribeTags'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeTags_nextToken :: Lens.Lens' DescribeTags (Core.Maybe Core.Text)
describeTags_nextToken = Lens.lens (\DescribeTags' {nextToken} -> nextToken) (\s@DescribeTags' {} a -> s {nextToken = a} :: DescribeTags)

-- | The total number of items to return in a single page of output. The
-- maximum value is 100.
describeTags_maxResults :: Lens.Lens' DescribeTags (Core.Maybe Core.Int)
describeTags_maxResults = Lens.lens (\DescribeTags' {maxResults} -> maxResults) (\s@DescribeTags' {} a -> s {maxResults = a} :: DescribeTags)

-- | You can filter the list using a /key/-/value/ format. You can separate
-- these items by using logical operators. Allowed filters include
-- @tagKey@, @tagValue@, and @configurationId@.
describeTags_filters :: Lens.Lens' DescribeTags (Core.Maybe [TagFilter])
describeTags_filters = Lens.lens (\DescribeTags' {filters} -> filters) (\s@DescribeTags' {} a -> s {filters = a} :: DescribeTags) Core.. Lens.mapping Lens._Coerce

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTags

instance Core.NFData DescribeTags

instance Core.ToHeaders DescribeTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeTags" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribeTags where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | The call returns a token. Use this token to get the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Depending on the input, this is a list of configuration items tagged
    -- with a specific tag, or a list of tags for a specific configuration
    -- item.
    tags :: Core.Maybe [ConfigurationTag],
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
-- 'nextToken', 'describeTagsResponse_nextToken' - The call returns a token. Use this token to get the next set of results.
--
-- 'tags', 'describeTagsResponse_tags' - Depending on the input, this is a list of configuration items tagged
-- with a specific tag, or a list of tags for a specific configuration
-- item.
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

-- | The call returns a token. Use this token to get the next set of results.
describeTagsResponse_nextToken :: Lens.Lens' DescribeTagsResponse (Core.Maybe Core.Text)
describeTagsResponse_nextToken = Lens.lens (\DescribeTagsResponse' {nextToken} -> nextToken) (\s@DescribeTagsResponse' {} a -> s {nextToken = a} :: DescribeTagsResponse)

-- | Depending on the input, this is a list of configuration items tagged
-- with a specific tag, or a list of tags for a specific configuration
-- item.
describeTagsResponse_tags :: Lens.Lens' DescribeTagsResponse (Core.Maybe [ConfigurationTag])
describeTagsResponse_tags = Lens.lens (\DescribeTagsResponse' {tags} -> tags) (\s@DescribeTagsResponse' {} a -> s {tags = a} :: DescribeTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Core.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Core.NFData DescribeTagsResponse
