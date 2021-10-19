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
-- Module      : Network.AWS.Athena.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags associated with an Athena workgroup or data catalog
-- resource.
--
-- This operation returns paginated results.
module Network.AWS.Athena.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceARN,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The token for the next set of results, or null if there are no
    -- additional results for this request, where the request lists the tags
    -- for the resource with the specified ARN.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request that lists the
    -- tags for the resource.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Lists the tags for the resource with the specified ARN.
    resourceARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsForResource_nextToken' - The token for the next set of results, or null if there are no
-- additional results for this request, where the request lists the tags
-- for the resource with the specified ARN.
--
-- 'maxResults', 'listTagsForResource_maxResults' - The maximum number of results to be returned per request that lists the
-- tags for the resource.
--
-- 'resourceARN', 'listTagsForResource_resourceARN' - Lists the tags for the resource with the specified ARN.
newListTagsForResource ::
  -- | 'resourceARN'
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceARN = pResourceARN_
    }

-- | The token for the next set of results, or null if there are no
-- additional results for this request, where the request lists the tags
-- for the resource with the specified ARN.
listTagsForResource_nextToken :: Lens.Lens' ListTagsForResource (Prelude.Maybe Prelude.Text)
listTagsForResource_nextToken = Lens.lens (\ListTagsForResource' {nextToken} -> nextToken) (\s@ListTagsForResource' {} a -> s {nextToken = a} :: ListTagsForResource)

-- | The maximum number of results to be returned per request that lists the
-- tags for the resource.
listTagsForResource_maxResults :: Lens.Lens' ListTagsForResource (Prelude.Maybe Prelude.Natural)
listTagsForResource_maxResults = Lens.lens (\ListTagsForResource' {maxResults} -> maxResults) (\s@ListTagsForResource' {} a -> s {maxResults = a} :: ListTagsForResource)

-- | Lists the tags for the resource with the specified ARN.
listTagsForResource_resourceARN :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_resourceARN = Lens.lens (\ListTagsForResource' {resourceARN} -> resourceARN) (\s@ListTagsForResource' {} a -> s {resourceARN = a} :: ListTagsForResource)

instance Core.AWSPager ListTagsForResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsForResourceResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagsForResourceResponse_tags
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTagsForResource_nextToken
          Lens..~ rs
          Lens.^? listTagsForResourceResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResource

instance Prelude.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.ListTagsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ResourceARN" Core..= resourceARN)
          ]
      )

instance Core.ToPath ListTagsForResource where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTagsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | A token to be used by the next request if this request is truncated.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of tags associated with the specified resource.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsForResourceResponse_nextToken' - A token to be used by the next request if this request is truncated.
--
-- 'tags', 'listTagsForResourceResponse_tags' - The list of tags associated with the specified resource.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { nextToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to be used by the next request if this request is truncated.
listTagsForResourceResponse_nextToken :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe Prelude.Text)
listTagsForResourceResponse_nextToken = Lens.lens (\ListTagsForResourceResponse' {nextToken} -> nextToken) (\s@ListTagsForResourceResponse' {} a -> s {nextToken = a} :: ListTagsForResourceResponse)

-- | The list of tags associated with the specified resource.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe [Tag])
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse
