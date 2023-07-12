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
-- Module      : Amazonka.AppSync.ListApiKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the API keys for a given API.
--
-- API keys are deleted automatically 60 days after they expire. However,
-- they may still be included in the response until they have actually been
-- deleted. You can safely call @DeleteApiKey@ to manually delete a key
-- before it\'s automatically deleted.
--
-- This operation returns paginated results.
module Amazonka.AppSync.ListApiKeys
  ( -- * Creating a Request
    ListApiKeys (..),
    newListApiKeys,

    -- * Request Lenses
    listApiKeys_maxResults,
    listApiKeys_nextToken,
    listApiKeys_apiId,

    -- * Destructuring the Response
    ListApiKeysResponse (..),
    newListApiKeysResponse,

    -- * Response Lenses
    listApiKeysResponse_apiKeys,
    listApiKeysResponse_nextToken,
    listApiKeysResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApiKeys' smart constructor.
data ListApiKeys = ListApiKeys'
  { -- | The maximum number of results that you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApiKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listApiKeys_maxResults' - The maximum number of results that you want the request to return.
--
-- 'nextToken', 'listApiKeys_nextToken' - An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
--
-- 'apiId', 'listApiKeys_apiId' - The API ID.
newListApiKeys ::
  -- | 'apiId'
  Prelude.Text ->
  ListApiKeys
newListApiKeys pApiId_ =
  ListApiKeys'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      apiId = pApiId_
    }

-- | The maximum number of results that you want the request to return.
listApiKeys_maxResults :: Lens.Lens' ListApiKeys (Prelude.Maybe Prelude.Natural)
listApiKeys_maxResults = Lens.lens (\ListApiKeys' {maxResults} -> maxResults) (\s@ListApiKeys' {} a -> s {maxResults = a} :: ListApiKeys)

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listApiKeys_nextToken :: Lens.Lens' ListApiKeys (Prelude.Maybe Prelude.Text)
listApiKeys_nextToken = Lens.lens (\ListApiKeys' {nextToken} -> nextToken) (\s@ListApiKeys' {} a -> s {nextToken = a} :: ListApiKeys)

-- | The API ID.
listApiKeys_apiId :: Lens.Lens' ListApiKeys Prelude.Text
listApiKeys_apiId = Lens.lens (\ListApiKeys' {apiId} -> apiId) (\s@ListApiKeys' {} a -> s {apiId = a} :: ListApiKeys)

instance Core.AWSPager ListApiKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApiKeysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApiKeysResponse_apiKeys
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listApiKeys_nextToken
          Lens..~ rs
          Lens.^? listApiKeysResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListApiKeys where
  type AWSResponse ListApiKeys = ListApiKeysResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApiKeysResponse'
            Prelude.<$> (x Data..?> "apiKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApiKeys where
  hashWithSalt _salt ListApiKeys' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData ListApiKeys where
  rnf ListApiKeys' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders ListApiKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListApiKeys where
  toPath ListApiKeys' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/apikeys"]

instance Data.ToQuery ListApiKeys where
  toQuery ListApiKeys' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListApiKeysResponse' smart constructor.
data ListApiKeysResponse = ListApiKeysResponse'
  { -- | The @ApiKey@ objects.
    apiKeys :: Prelude.Maybe [ApiKey],
    -- | An identifier to pass in the next request to this operation to return
    -- the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApiKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeys', 'listApiKeysResponse_apiKeys' - The @ApiKey@ objects.
--
-- 'nextToken', 'listApiKeysResponse_nextToken' - An identifier to pass in the next request to this operation to return
-- the next set of items in the list.
--
-- 'httpStatus', 'listApiKeysResponse_httpStatus' - The response's http status code.
newListApiKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApiKeysResponse
newListApiKeysResponse pHttpStatus_ =
  ListApiKeysResponse'
    { apiKeys = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ApiKey@ objects.
listApiKeysResponse_apiKeys :: Lens.Lens' ListApiKeysResponse (Prelude.Maybe [ApiKey])
listApiKeysResponse_apiKeys = Lens.lens (\ListApiKeysResponse' {apiKeys} -> apiKeys) (\s@ListApiKeysResponse' {} a -> s {apiKeys = a} :: ListApiKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | An identifier to pass in the next request to this operation to return
-- the next set of items in the list.
listApiKeysResponse_nextToken :: Lens.Lens' ListApiKeysResponse (Prelude.Maybe Prelude.Text)
listApiKeysResponse_nextToken = Lens.lens (\ListApiKeysResponse' {nextToken} -> nextToken) (\s@ListApiKeysResponse' {} a -> s {nextToken = a} :: ListApiKeysResponse)

-- | The response's http status code.
listApiKeysResponse_httpStatus :: Lens.Lens' ListApiKeysResponse Prelude.Int
listApiKeysResponse_httpStatus = Lens.lens (\ListApiKeysResponse' {httpStatus} -> httpStatus) (\s@ListApiKeysResponse' {} a -> s {httpStatus = a} :: ListApiKeysResponse)

instance Prelude.NFData ListApiKeysResponse where
  rnf ListApiKeysResponse' {..} =
    Prelude.rnf apiKeys
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
