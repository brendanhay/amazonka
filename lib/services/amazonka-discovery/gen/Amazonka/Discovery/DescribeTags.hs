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
-- Module      : Amazonka.Discovery.DescribeTags
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Discovery.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_nextToken,
    describeTags_filters,
    describeTags_maxResults,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_tags,
    describeTagsResponse_nextToken,
    describeTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can filter the list using a /key/-/value/ format. You can separate
    -- these items by using logical operators. Allowed filters include
    -- @tagKey@, @tagValue@, and @configurationId@.
    filters :: Prelude.Maybe [TagFilter],
    -- | The total number of items to return in a single page of output. The
    -- maximum value is 100.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'nextToken', 'describeTags_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'filters', 'describeTags_filters' - You can filter the list using a /key/-/value/ format. You can separate
-- these items by using logical operators. Allowed filters include
-- @tagKey@, @tagValue@, and @configurationId@.
--
-- 'maxResults', 'describeTags_maxResults' - The total number of items to return in a single page of output. The
-- maximum value is 100.
newDescribeTags ::
  DescribeTags
newDescribeTags =
  DescribeTags'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeTags_nextToken :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Text)
describeTags_nextToken = Lens.lens (\DescribeTags' {nextToken} -> nextToken) (\s@DescribeTags' {} a -> s {nextToken = a} :: DescribeTags)

-- | You can filter the list using a /key/-/value/ format. You can separate
-- these items by using logical operators. Allowed filters include
-- @tagKey@, @tagValue@, and @configurationId@.
describeTags_filters :: Lens.Lens' DescribeTags (Prelude.Maybe [TagFilter])
describeTags_filters = Lens.lens (\DescribeTags' {filters} -> filters) (\s@DescribeTags' {} a -> s {filters = a} :: DescribeTags) Prelude.. Lens.mapping Lens.coerced

-- | The total number of items to return in a single page of output. The
-- maximum value is 100.
describeTags_maxResults :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Int)
describeTags_maxResults = Lens.lens (\DescribeTags' {maxResults} -> maxResults) (\s@DescribeTags' {} a -> s {maxResults = a} :: DescribeTags)

instance Core.AWSPager DescribeTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_tags Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTags_nextToken
          Lens..~ rs
          Lens.^? describeTagsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTags where
  hashWithSalt _salt DescribeTags' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeTags where
  rnf DescribeTags' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeTags where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | Depending on the input, this is a list of configuration items tagged
    -- with a specific tag, or a list of tags for a specific configuration
    -- item.
    tags :: Prelude.Maybe [ConfigurationTag],
    -- | The call returns a token. Use this token to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'tags', 'describeTagsResponse_tags' - Depending on the input, this is a list of configuration items tagged
-- with a specific tag, or a list of tags for a specific configuration
-- item.
--
-- 'nextToken', 'describeTagsResponse_nextToken' - The call returns a token. Use this token to get the next set of results.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { tags = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Depending on the input, this is a list of configuration items tagged
-- with a specific tag, or a list of tags for a specific configuration
-- item.
describeTagsResponse_tags :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe [ConfigurationTag])
describeTagsResponse_tags = Lens.lens (\DescribeTagsResponse' {tags} -> tags) (\s@DescribeTagsResponse' {} a -> s {tags = a} :: DescribeTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The call returns a token. Use this token to get the next set of results.
describeTagsResponse_nextToken :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe Prelude.Text)
describeTagsResponse_nextToken = Lens.lens (\DescribeTagsResponse' {nextToken} -> nextToken) (\s@DescribeTagsResponse' {} a -> s {nextToken = a} :: DescribeTagsResponse)

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Prelude.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Prelude.NFData DescribeTagsResponse where
  rnf DescribeTagsResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
