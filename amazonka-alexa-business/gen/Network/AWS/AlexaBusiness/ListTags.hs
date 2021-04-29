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
-- Module      : Network.AWS.AlexaBusiness.ListTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags for the specified resource.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_nextToken,
    listTags_maxResults,
    listTags_arn,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the specified resource for which to list tags.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTags_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'maxResults', 'listTags_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'arn', 'listTags_arn' - The ARN of the specified resource for which to list tags.
newListTags ::
  -- | 'arn'
  Prelude.Text ->
  ListTags
newListTags pArn_ =
  ListTags'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      arn = pArn_
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
listTags_nextToken :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listTags_maxResults :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Natural)
listTags_maxResults = Lens.lens (\ListTags' {maxResults} -> maxResults) (\s@ListTags' {} a -> s {maxResults = a} :: ListTags)

-- | The ARN of the specified resource for which to list tags.
listTags_arn :: Lens.Lens' ListTags Prelude.Text
listTags_arn = Lens.lens (\ListTags' {arn} -> arn) (\s@ListTags' {} a -> s {arn = a} :: ListTags)

instance Pager.AWSPager ListTags where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listTagsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listTagsResponse_tags Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listTags_nextToken
          Lens..~ rs
          Lens.^? listTagsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListTags where
  type Rs ListTags = ListTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTags

instance Prelude.NFData ListTags

instance Prelude.ToHeaders ListTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AlexaForBusiness.ListTags" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListTags where
  toJSON ListTags' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just ("Arn" Prelude..= arn)
          ]
      )

instance Prelude.ToPath ListTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The tags requested for the specified resource.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'tags', 'listTagsResponse_tags' - The tags requested for the specified resource.
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

-- | The token returned to indicate that there is more data available.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Prelude.Maybe Prelude.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | The tags requested for the specified resource.
listTagsResponse_tags :: Lens.Lens' ListTagsResponse (Prelude.Maybe [Tag])
listTagsResponse_tags = Lens.lens (\ListTagsResponse' {tags} -> tags) (\s@ListTagsResponse' {} a -> s {tags = a} :: ListTagsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Prelude.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Prelude.NFData ListTagsResponse
