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
-- Module      : Amazonka.ResourceExplorer2.ListSupportedResourceTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all resource types currently supported by Amazon Web
-- Services Resource Explorer.
--
-- This operation returns paginated results.
module Amazonka.ResourceExplorer2.ListSupportedResourceTypes
  ( -- * Creating a Request
    ListSupportedResourceTypes (..),
    newListSupportedResourceTypes,

    -- * Request Lenses
    listSupportedResourceTypes_maxResults,
    listSupportedResourceTypes_nextToken,

    -- * Destructuring the Response
    ListSupportedResourceTypesResponse (..),
    newListSupportedResourceTypesResponse,

    -- * Response Lenses
    listSupportedResourceTypesResponse_nextToken,
    listSupportedResourceTypesResponse_resourceTypes,
    listSupportedResourceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSupportedResourceTypes' smart constructor.
data ListSupportedResourceTypes = ListSupportedResourceTypes'
  { -- | The maximum number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- appropriate to the operation. If additional items exist beyond those
    -- included in the current response, the @NextToken@ response element is
    -- present and has a value (is not null). Include that value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results.
    --
    -- An API operation can return fewer results than the maximum even when
    -- there are more results available. You should check @NextToken@ after
    -- every operation to ensure that you receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSupportedResourceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSupportedResourceTypes_maxResults' - The maximum number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- appropriate to the operation. If additional items exist beyond those
-- included in the current response, the @NextToken@ response element is
-- present and has a value (is not null). Include that value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results.
--
-- An API operation can return fewer results than the maximum even when
-- there are more results available. You should check @NextToken@ after
-- every operation to ensure that you receive all of the results.
--
-- 'nextToken', 'listSupportedResourceTypes_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
newListSupportedResourceTypes ::
  ListSupportedResourceTypes
newListSupportedResourceTypes =
  ListSupportedResourceTypes'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- appropriate to the operation. If additional items exist beyond those
-- included in the current response, the @NextToken@ response element is
-- present and has a value (is not null). Include that value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results.
--
-- An API operation can return fewer results than the maximum even when
-- there are more results available. You should check @NextToken@ after
-- every operation to ensure that you receive all of the results.
listSupportedResourceTypes_maxResults :: Lens.Lens' ListSupportedResourceTypes (Prelude.Maybe Prelude.Natural)
listSupportedResourceTypes_maxResults = Lens.lens (\ListSupportedResourceTypes' {maxResults} -> maxResults) (\s@ListSupportedResourceTypes' {} a -> s {maxResults = a} :: ListSupportedResourceTypes)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listSupportedResourceTypes_nextToken :: Lens.Lens' ListSupportedResourceTypes (Prelude.Maybe Prelude.Text)
listSupportedResourceTypes_nextToken = Lens.lens (\ListSupportedResourceTypes' {nextToken} -> nextToken) (\s@ListSupportedResourceTypes' {} a -> s {nextToken = a} :: ListSupportedResourceTypes)

instance Core.AWSPager ListSupportedResourceTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSupportedResourceTypesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSupportedResourceTypesResponse_resourceTypes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSupportedResourceTypes_nextToken
          Lens..~ rs
          Lens.^? listSupportedResourceTypesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSupportedResourceTypes where
  type
    AWSResponse ListSupportedResourceTypes =
      ListSupportedResourceTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSupportedResourceTypesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ResourceTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSupportedResourceTypes where
  hashWithSalt _salt ListSupportedResourceTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSupportedResourceTypes where
  rnf ListSupportedResourceTypes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSupportedResourceTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSupportedResourceTypes where
  toJSON ListSupportedResourceTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSupportedResourceTypes where
  toPath = Prelude.const "/ListSupportedResourceTypes"

instance Data.ToQuery ListSupportedResourceTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSupportedResourceTypesResponse' smart constructor.
data ListSupportedResourceTypesResponse = ListSupportedResourceTypesResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of resource types supported by Resource Explorer.
    resourceTypes :: Prelude.Maybe [SupportedResourceType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSupportedResourceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSupportedResourceTypesResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'resourceTypes', 'listSupportedResourceTypesResponse_resourceTypes' - The list of resource types supported by Resource Explorer.
--
-- 'httpStatus', 'listSupportedResourceTypesResponse_httpStatus' - The response's http status code.
newListSupportedResourceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSupportedResourceTypesResponse
newListSupportedResourceTypesResponse pHttpStatus_ =
  ListSupportedResourceTypesResponse'
    { nextToken =
        Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listSupportedResourceTypesResponse_nextToken :: Lens.Lens' ListSupportedResourceTypesResponse (Prelude.Maybe Prelude.Text)
listSupportedResourceTypesResponse_nextToken = Lens.lens (\ListSupportedResourceTypesResponse' {nextToken} -> nextToken) (\s@ListSupportedResourceTypesResponse' {} a -> s {nextToken = a} :: ListSupportedResourceTypesResponse)

-- | The list of resource types supported by Resource Explorer.
listSupportedResourceTypesResponse_resourceTypes :: Lens.Lens' ListSupportedResourceTypesResponse (Prelude.Maybe [SupportedResourceType])
listSupportedResourceTypesResponse_resourceTypes = Lens.lens (\ListSupportedResourceTypesResponse' {resourceTypes} -> resourceTypes) (\s@ListSupportedResourceTypesResponse' {} a -> s {resourceTypes = a} :: ListSupportedResourceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSupportedResourceTypesResponse_httpStatus :: Lens.Lens' ListSupportedResourceTypesResponse Prelude.Int
listSupportedResourceTypesResponse_httpStatus = Lens.lens (\ListSupportedResourceTypesResponse' {httpStatus} -> httpStatus) (\s@ListSupportedResourceTypesResponse' {} a -> s {httpStatus = a} :: ListSupportedResourceTypesResponse)

instance
  Prelude.NFData
    ListSupportedResourceTypesResponse
  where
  rnf ListSupportedResourceTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf httpStatus
