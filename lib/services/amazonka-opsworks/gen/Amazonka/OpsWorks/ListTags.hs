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
-- Module      : Amazonka.OpsWorks.ListTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are applied to the specified stack or layer.
module Amazonka.OpsWorks.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_maxResults,
    listTags_nextToken,
    listTags_resourceArn,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | Do not use. A validation exception occurs if you add a @MaxResults@
    -- parameter to a @ListTagsRequest@ call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Do not use. A validation exception occurs if you add a @NextToken@
    -- parameter to a @ListTagsRequest@ call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The stack or layer\'s Amazon Resource Number (ARN).
    resourceArn :: Prelude.Text
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
-- 'maxResults', 'listTags_maxResults' - Do not use. A validation exception occurs if you add a @MaxResults@
-- parameter to a @ListTagsRequest@ call.
--
-- 'nextToken', 'listTags_nextToken' - Do not use. A validation exception occurs if you add a @NextToken@
-- parameter to a @ListTagsRequest@ call.
--
-- 'resourceArn', 'listTags_resourceArn' - The stack or layer\'s Amazon Resource Number (ARN).
newListTags ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListTags
newListTags pResourceArn_ =
  ListTags'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | Do not use. A validation exception occurs if you add a @MaxResults@
-- parameter to a @ListTagsRequest@ call.
listTags_maxResults :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Int)
listTags_maxResults = Lens.lens (\ListTags' {maxResults} -> maxResults) (\s@ListTags' {} a -> s {maxResults = a} :: ListTags)

-- | Do not use. A validation exception occurs if you add a @NextToken@
-- parameter to a @ListTagsRequest@ call.
listTags_nextToken :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | The stack or layer\'s Amazon Resource Number (ARN).
listTags_resourceArn :: Lens.Lens' ListTags Prelude.Text
listTags_resourceArn = Lens.lens (\ListTags' {resourceArn} -> resourceArn) (\s@ListTags' {} a -> s {resourceArn = a} :: ListTags)

instance Core.AWSRequest ListTags where
  type AWSResponse ListTags = ListTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTags where
  hashWithSalt _salt ListTags' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ListTags where
  rnf ListTags' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders ListTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("OpsWorks_20130218.ListTags" :: Prelude.ByteString),
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
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath ListTags where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTags where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @ListTags@ request.
--
-- /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | If a paginated request does not return all of the remaining results,
    -- this parameter is set to a token that you can assign to the request
    -- object\'s @NextToken@ parameter to get the next set of results. If the
    -- previous paginated request returned all of the remaining results, this
    -- parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A set of key-value pairs that contain tag keys and tag values that are
    -- attached to a stack or layer.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listTagsResponse_nextToken' - If a paginated request does not return all of the remaining results,
-- this parameter is set to a token that you can assign to the request
-- object\'s @NextToken@ parameter to get the next set of results. If the
-- previous paginated request returned all of the remaining results, this
-- parameter is set to @null@.
--
-- 'tags', 'listTagsResponse_tags' - A set of key-value pairs that contain tag keys and tag values that are
-- attached to a stack or layer.
--
-- 'httpStatus', 'listTagsResponse_httpStatus' - The response's http status code.
newListTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { nextToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a paginated request does not return all of the remaining results,
-- this parameter is set to a token that you can assign to the request
-- object\'s @NextToken@ parameter to get the next set of results. If the
-- previous paginated request returned all of the remaining results, this
-- parameter is set to @null@.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Prelude.Maybe Prelude.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | A set of key-value pairs that contain tag keys and tag values that are
-- attached to a stack or layer.
listTagsResponse_tags :: Lens.Lens' ListTagsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsResponse_tags = Lens.lens (\ListTagsResponse' {tags} -> tags) (\s@ListTagsResponse' {} a -> s {tags = a} :: ListTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Prelude.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Prelude.NFData ListTagsResponse where
  rnf ListTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
