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
-- Module      : Amazonka.CloudHSMV2.ListTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of tags for the specified AWS CloudHSM cluster.
--
-- This is a paginated operation, which means that each response might
-- contain only a subset of all the tags. When the response contains only a
-- subset of tags, it includes a @NextToken@ value. Use this value in a
-- subsequent @ListTags@ request to get more tags. When you receive a
-- response with no @NextToken@ (or an empty or null value), that means
-- there are no more tags to get.
--
-- This operation returns paginated results.
module Amazonka.CloudHSMV2.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceId,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_nextToken,
    listTagsResponse_httpStatus,
    listTagsResponse_tagList,
  )
where

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | The @NextToken@ value that you received in the previous response. Use
    -- this value to get more tags.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of tags to return in the response. When there are
    -- more tags than the number you specify, the response contains a
    -- @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The cluster identifier (ID) for the cluster whose tags you are getting.
    -- To find the cluster ID, use DescribeClusters.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTags_nextToken' - The @NextToken@ value that you received in the previous response. Use
-- this value to get more tags.
--
-- 'maxResults', 'listTags_maxResults' - The maximum number of tags to return in the response. When there are
-- more tags than the number you specify, the response contains a
-- @NextToken@ value.
--
-- 'resourceId', 'listTags_resourceId' - The cluster identifier (ID) for the cluster whose tags you are getting.
-- To find the cluster ID, use DescribeClusters.
newListTags ::
  -- | 'resourceId'
  Prelude.Text ->
  ListTags
newListTags pResourceId_ =
  ListTags'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The @NextToken@ value that you received in the previous response. Use
-- this value to get more tags.
listTags_nextToken :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | The maximum number of tags to return in the response. When there are
-- more tags than the number you specify, the response contains a
-- @NextToken@ value.
listTags_maxResults :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Natural)
listTags_maxResults = Lens.lens (\ListTags' {maxResults} -> maxResults) (\s@ListTags' {} a -> s {maxResults = a} :: ListTags)

-- | The cluster identifier (ID) for the cluster whose tags you are getting.
-- To find the cluster ID, use DescribeClusters.
listTags_resourceId :: Lens.Lens' ListTags Prelude.Text
listTags_resourceId = Lens.lens (\ListTags' {resourceId} -> resourceId) (\s@ListTags' {} a -> s {resourceId = a} :: ListTags)

instance Core.AWSPager ListTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. listTagsResponse_tagList) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTags_nextToken
          Lens..~ rs
          Lens.^? listTagsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTags where
  type AWSResponse ListTags = ListTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "TagList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListTags where
  hashWithSalt _salt ListTags' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ListTags where
  rnf ListTags' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders ListTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("BaldrApiService.ListTags" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTags where
  toJSON ListTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath ListTags where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | An opaque string that indicates that the response contains only a subset
    -- of tags. Use this value in a subsequent @ListTags@ request to get more
    -- tags.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of tags.
    tagList :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsResponse_nextToken' - An opaque string that indicates that the response contains only a subset
-- of tags. Use this value in a subsequent @ListTags@ request to get more
-- tags.
--
-- 'httpStatus', 'listTagsResponse_httpStatus' - The response's http status code.
--
-- 'tagList', 'listTagsResponse_tagList' - A list of tags.
newListTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tagList = Prelude.mempty
    }

-- | An opaque string that indicates that the response contains only a subset
-- of tags. Use this value in a subsequent @ListTags@ request to get more
-- tags.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Prelude.Maybe Prelude.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Prelude.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

-- | A list of tags.
listTagsResponse_tagList :: Lens.Lens' ListTagsResponse [Tag]
listTagsResponse_tagList = Lens.lens (\ListTagsResponse' {tagList} -> tagList) (\s@ListTagsResponse' {} a -> s {tagList = a} :: ListTagsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTagsResponse where
  rnf ListTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tagList
