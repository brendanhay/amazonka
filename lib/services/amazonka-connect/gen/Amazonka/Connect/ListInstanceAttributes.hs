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
-- Module      : Amazonka.Connect.ListInstanceAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of all attribute types for the given instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListInstanceAttributes
  ( -- * Creating a Request
    ListInstanceAttributes (..),
    newListInstanceAttributes,

    -- * Request Lenses
    listInstanceAttributes_maxResults,
    listInstanceAttributes_nextToken,
    listInstanceAttributes_instanceId,

    -- * Destructuring the Response
    ListInstanceAttributesResponse (..),
    newListInstanceAttributesResponse,

    -- * Response Lenses
    listInstanceAttributesResponse_attributes,
    listInstanceAttributesResponse_nextToken,
    listInstanceAttributesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstanceAttributes' smart constructor.
data ListInstanceAttributes = ListInstanceAttributes'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listInstanceAttributes_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listInstanceAttributes_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listInstanceAttributes_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListInstanceAttributes ::
  -- | 'instanceId'
  Prelude.Text ->
  ListInstanceAttributes
newListInstanceAttributes pInstanceId_ =
  ListInstanceAttributes'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
listInstanceAttributes_maxResults :: Lens.Lens' ListInstanceAttributes (Prelude.Maybe Prelude.Natural)
listInstanceAttributes_maxResults = Lens.lens (\ListInstanceAttributes' {maxResults} -> maxResults) (\s@ListInstanceAttributes' {} a -> s {maxResults = a} :: ListInstanceAttributes)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listInstanceAttributes_nextToken :: Lens.Lens' ListInstanceAttributes (Prelude.Maybe Prelude.Text)
listInstanceAttributes_nextToken = Lens.lens (\ListInstanceAttributes' {nextToken} -> nextToken) (\s@ListInstanceAttributes' {} a -> s {nextToken = a} :: ListInstanceAttributes)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listInstanceAttributes_instanceId :: Lens.Lens' ListInstanceAttributes Prelude.Text
listInstanceAttributes_instanceId = Lens.lens (\ListInstanceAttributes' {instanceId} -> instanceId) (\s@ListInstanceAttributes' {} a -> s {instanceId = a} :: ListInstanceAttributes)

instance Core.AWSPager ListInstanceAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceAttributesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceAttributesResponse_attributes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstanceAttributes_nextToken
          Lens..~ rs
          Lens.^? listInstanceAttributesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListInstanceAttributes where
  type
    AWSResponse ListInstanceAttributes =
      ListInstanceAttributesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceAttributesResponse'
            Prelude.<$> (x Data..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstanceAttributes where
  hashWithSalt _salt ListInstanceAttributes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListInstanceAttributes where
  rnf ListInstanceAttributes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListInstanceAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListInstanceAttributes where
  toPath ListInstanceAttributes' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId, "/attributes"]

instance Data.ToQuery ListInstanceAttributes where
  toQuery ListInstanceAttributes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListInstanceAttributesResponse' smart constructor.
data ListInstanceAttributesResponse = ListInstanceAttributesResponse'
  { -- | The attribute types.
    attributes :: Prelude.Maybe [Attribute],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'listInstanceAttributesResponse_attributes' - The attribute types.
--
-- 'nextToken', 'listInstanceAttributesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listInstanceAttributesResponse_httpStatus' - The response's http status code.
newListInstanceAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceAttributesResponse
newListInstanceAttributesResponse pHttpStatus_ =
  ListInstanceAttributesResponse'
    { attributes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attribute types.
listInstanceAttributesResponse_attributes :: Lens.Lens' ListInstanceAttributesResponse (Prelude.Maybe [Attribute])
listInstanceAttributesResponse_attributes = Lens.lens (\ListInstanceAttributesResponse' {attributes} -> attributes) (\s@ListInstanceAttributesResponse' {} a -> s {attributes = a} :: ListInstanceAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listInstanceAttributesResponse_nextToken :: Lens.Lens' ListInstanceAttributesResponse (Prelude.Maybe Prelude.Text)
listInstanceAttributesResponse_nextToken = Lens.lens (\ListInstanceAttributesResponse' {nextToken} -> nextToken) (\s@ListInstanceAttributesResponse' {} a -> s {nextToken = a} :: ListInstanceAttributesResponse)

-- | The response's http status code.
listInstanceAttributesResponse_httpStatus :: Lens.Lens' ListInstanceAttributesResponse Prelude.Int
listInstanceAttributesResponse_httpStatus = Lens.lens (\ListInstanceAttributesResponse' {httpStatus} -> httpStatus) (\s@ListInstanceAttributesResponse' {} a -> s {httpStatus = a} :: ListInstanceAttributesResponse)

instance
  Prelude.NFData
    ListInstanceAttributesResponse
  where
  rnf ListInstanceAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
