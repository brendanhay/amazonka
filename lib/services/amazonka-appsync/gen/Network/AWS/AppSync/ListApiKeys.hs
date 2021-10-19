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
-- Module      : Network.AWS.AppSync.ListApiKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.AppSync.ListApiKeys
  ( -- * Creating a Request
    ListApiKeys (..),
    newListApiKeys,

    -- * Request Lenses
    listApiKeys_nextToken,
    listApiKeys_maxResults,
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

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListApiKeys' smart constructor.
data ListApiKeys = ListApiKeys'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listApiKeys_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listApiKeys_maxResults' - The maximum number of results you want the request to return.
--
-- 'apiId', 'listApiKeys_apiId' - The API ID.
newListApiKeys ::
  -- | 'apiId'
  Prelude.Text ->
  ListApiKeys
newListApiKeys pApiId_ =
  ListApiKeys'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      apiId = pApiId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listApiKeys_nextToken :: Lens.Lens' ListApiKeys (Prelude.Maybe Prelude.Text)
listApiKeys_nextToken = Lens.lens (\ListApiKeys' {nextToken} -> nextToken) (\s@ListApiKeys' {} a -> s {nextToken = a} :: ListApiKeys)

-- | The maximum number of results you want the request to return.
listApiKeys_maxResults :: Lens.Lens' ListApiKeys (Prelude.Maybe Prelude.Natural)
listApiKeys_maxResults = Lens.lens (\ListApiKeys' {maxResults} -> maxResults) (\s@ListApiKeys' {} a -> s {maxResults = a} :: ListApiKeys)

-- | The API ID.
listApiKeys_apiId :: Lens.Lens' ListApiKeys Prelude.Text
listApiKeys_apiId = Lens.lens (\ListApiKeys' {apiId} -> apiId) (\s@ListApiKeys' {} a -> s {apiId = a} :: ListApiKeys)

instance Core.AWSPager ListApiKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApiKeysResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApiKeysResponse_apiKeys Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listApiKeys_nextToken
          Lens..~ rs
          Lens.^? listApiKeysResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListApiKeys where
  type AWSResponse ListApiKeys = ListApiKeysResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApiKeysResponse'
            Prelude.<$> (x Core..?> "apiKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApiKeys

instance Prelude.NFData ListApiKeys

instance Core.ToHeaders ListApiKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListApiKeys where
  toPath ListApiKeys' {..} =
    Prelude.mconcat
      ["/v1/apis/", Core.toBS apiId, "/apikeys"]

instance Core.ToQuery ListApiKeys where
  toQuery ListApiKeys' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListApiKeysResponse' smart constructor.
data ListApiKeysResponse = ListApiKeysResponse'
  { -- | The @ApiKey@ objects.
    apiKeys :: Prelude.Maybe [ApiKey],
    -- | An identifier to be passed in the next request to this operation to
    -- return the next set of items in the list.
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
-- 'nextToken', 'listApiKeysResponse_nextToken' - An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
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

-- | An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
listApiKeysResponse_nextToken :: Lens.Lens' ListApiKeysResponse (Prelude.Maybe Prelude.Text)
listApiKeysResponse_nextToken = Lens.lens (\ListApiKeysResponse' {nextToken} -> nextToken) (\s@ListApiKeysResponse' {} a -> s {nextToken = a} :: ListApiKeysResponse)

-- | The response's http status code.
listApiKeysResponse_httpStatus :: Lens.Lens' ListApiKeysResponse Prelude.Int
listApiKeysResponse_httpStatus = Lens.lens (\ListApiKeysResponse' {httpStatus} -> httpStatus) (\s@ListApiKeysResponse' {} a -> s {httpStatus = a} :: ListApiKeysResponse)

instance Prelude.NFData ListApiKeysResponse
