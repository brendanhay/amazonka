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
-- Module      : Amazonka.Transfer.ListTagsForResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the tags associated with the Amazon Resource Name (ARN)
-- that you specify. The resource can be a user, server, or role.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_arn,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_arn,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | When you request additional results from the @ListTagsForResource@
    -- operation, a @NextToken@ parameter is returned in the input. You can
    -- then pass in a subsequent command to the @NextToken@ parameter to
    -- continue listing additional tags.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of tags to return as a response to the
    -- @ListTagsForResource@ request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Requests the tags associated with a particular Amazon Resource Name
    -- (ARN). An ARN is an identifier for a specific Amazon Web Services
    -- resource, such as a server, user, or role.
    arn :: Prelude.Text
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
-- 'nextToken', 'listTagsForResource_nextToken' - When you request additional results from the @ListTagsForResource@
-- operation, a @NextToken@ parameter is returned in the input. You can
-- then pass in a subsequent command to the @NextToken@ parameter to
-- continue listing additional tags.
--
-- 'maxResults', 'listTagsForResource_maxResults' - Specifies the number of tags to return as a response to the
-- @ListTagsForResource@ request.
--
-- 'arn', 'listTagsForResource_arn' - Requests the tags associated with a particular Amazon Resource Name
-- (ARN). An ARN is an identifier for a specific Amazon Web Services
-- resource, such as a server, user, or role.
newListTagsForResource ::
  -- | 'arn'
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pArn_ =
  ListTagsForResource'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      arn = pArn_
    }

-- | When you request additional results from the @ListTagsForResource@
-- operation, a @NextToken@ parameter is returned in the input. You can
-- then pass in a subsequent command to the @NextToken@ parameter to
-- continue listing additional tags.
listTagsForResource_nextToken :: Lens.Lens' ListTagsForResource (Prelude.Maybe Prelude.Text)
listTagsForResource_nextToken = Lens.lens (\ListTagsForResource' {nextToken} -> nextToken) (\s@ListTagsForResource' {} a -> s {nextToken = a} :: ListTagsForResource)

-- | Specifies the number of tags to return as a response to the
-- @ListTagsForResource@ request.
listTagsForResource_maxResults :: Lens.Lens' ListTagsForResource (Prelude.Maybe Prelude.Natural)
listTagsForResource_maxResults = Lens.lens (\ListTagsForResource' {maxResults} -> maxResults) (\s@ListTagsForResource' {} a -> s {maxResults = a} :: ListTagsForResource)

-- | Requests the tags associated with a particular Amazon Resource Name
-- (ARN). An ARN is an identifier for a specific Amazon Web Services
-- resource, such as a server, user, or role.
listTagsForResource_arn :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_arn = Lens.lens (\ListTagsForResource' {arn} -> arn) (\s@ListTagsForResource' {} a -> s {arn = a} :: ListTagsForResource)

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
            Lens.^? listTagsForResourceResponse_tags Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> (x Data..?> "Tags")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResource where
  hashWithSalt _salt ListTagsForResource' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListTagsForResource where
  rnf ListTagsForResource' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders ListTagsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListTagsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("Arn" Data..= arn)
          ]
      )

instance Data.ToPath ListTagsForResource where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | Key-value pairs that are assigned to a resource, usually for the purpose
    -- of grouping and searching for items. Tags are metadata that you define.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | When you can get additional results from the @ListTagsForResource@ call,
    -- a @NextToken@ parameter is returned in the output. You can then pass in
    -- a subsequent command to the @NextToken@ parameter to continue listing
    -- additional tags.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN you specified to list the tags of.
    arn :: Prelude.Maybe Prelude.Text,
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
-- 'tags', 'listTagsForResourceResponse_tags' - Key-value pairs that are assigned to a resource, usually for the purpose
-- of grouping and searching for items. Tags are metadata that you define.
--
-- 'nextToken', 'listTagsForResourceResponse_nextToken' - When you can get additional results from the @ListTagsForResource@ call,
-- a @NextToken@ parameter is returned in the output. You can then pass in
-- a subsequent command to the @NextToken@ parameter to continue listing
-- additional tags.
--
-- 'arn', 'listTagsForResourceResponse_arn' - The ARN you specified to list the tags of.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { tags =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Key-value pairs that are assigned to a resource, usually for the purpose
-- of grouping and searching for items. Tags are metadata that you define.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you can get additional results from the @ListTagsForResource@ call,
-- a @NextToken@ parameter is returned in the output. You can then pass in
-- a subsequent command to the @NextToken@ parameter to continue listing
-- additional tags.
listTagsForResourceResponse_nextToken :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe Prelude.Text)
listTagsForResourceResponse_nextToken = Lens.lens (\ListTagsForResourceResponse' {nextToken} -> nextToken) (\s@ListTagsForResourceResponse' {} a -> s {nextToken = a} :: ListTagsForResourceResponse)

-- | The ARN you specified to list the tags of.
listTagsForResourceResponse_arn :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe Prelude.Text)
listTagsForResourceResponse_arn = Lens.lens (\ListTagsForResourceResponse' {arn} -> arn) (\s@ListTagsForResourceResponse' {} a -> s {arn = a} :: ListTagsForResourceResponse)

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse where
  rnf ListTagsForResourceResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
