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
-- Module      : Amazonka.Connect.ListSecurityKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of all security keys associated with the
-- instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListSecurityKeys
  ( -- * Creating a Request
    ListSecurityKeys (..),
    newListSecurityKeys,

    -- * Request Lenses
    listSecurityKeys_maxResults,
    listSecurityKeys_nextToken,
    listSecurityKeys_instanceId,

    -- * Destructuring the Response
    ListSecurityKeysResponse (..),
    newListSecurityKeysResponse,

    -- * Response Lenses
    listSecurityKeysResponse_nextToken,
    listSecurityKeysResponse_securityKeys,
    listSecurityKeysResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSecurityKeys' smart constructor.
data ListSecurityKeys = ListSecurityKeys'
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
-- Create a value of 'ListSecurityKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSecurityKeys_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listSecurityKeys_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listSecurityKeys_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListSecurityKeys ::
  -- | 'instanceId'
  Prelude.Text ->
  ListSecurityKeys
newListSecurityKeys pInstanceId_ =
  ListSecurityKeys'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
listSecurityKeys_maxResults :: Lens.Lens' ListSecurityKeys (Prelude.Maybe Prelude.Natural)
listSecurityKeys_maxResults = Lens.lens (\ListSecurityKeys' {maxResults} -> maxResults) (\s@ListSecurityKeys' {} a -> s {maxResults = a} :: ListSecurityKeys)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listSecurityKeys_nextToken :: Lens.Lens' ListSecurityKeys (Prelude.Maybe Prelude.Text)
listSecurityKeys_nextToken = Lens.lens (\ListSecurityKeys' {nextToken} -> nextToken) (\s@ListSecurityKeys' {} a -> s {nextToken = a} :: ListSecurityKeys)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listSecurityKeys_instanceId :: Lens.Lens' ListSecurityKeys Prelude.Text
listSecurityKeys_instanceId = Lens.lens (\ListSecurityKeys' {instanceId} -> instanceId) (\s@ListSecurityKeys' {} a -> s {instanceId = a} :: ListSecurityKeys)

instance Core.AWSPager ListSecurityKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityKeysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityKeysResponse_securityKeys
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listSecurityKeys_nextToken
              Lens..~ rs
              Lens.^? listSecurityKeysResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListSecurityKeys where
  type
    AWSResponse ListSecurityKeys =
      ListSecurityKeysResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityKeysResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SecurityKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecurityKeys where
  hashWithSalt _salt ListSecurityKeys' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListSecurityKeys where
  rnf ListSecurityKeys' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf instanceId

instance Data.ToHeaders ListSecurityKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSecurityKeys where
  toPath ListSecurityKeys' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/security-keys"
      ]

instance Data.ToQuery ListSecurityKeys where
  toQuery ListSecurityKeys' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSecurityKeysResponse' smart constructor.
data ListSecurityKeysResponse = ListSecurityKeysResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The security keys.
    securityKeys :: Prelude.Maybe [SecurityKey],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityKeysResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'securityKeys', 'listSecurityKeysResponse_securityKeys' - The security keys.
--
-- 'httpStatus', 'listSecurityKeysResponse_httpStatus' - The response's http status code.
newListSecurityKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityKeysResponse
newListSecurityKeysResponse pHttpStatus_ =
  ListSecurityKeysResponse'
    { nextToken =
        Prelude.Nothing,
      securityKeys = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listSecurityKeysResponse_nextToken :: Lens.Lens' ListSecurityKeysResponse (Prelude.Maybe Prelude.Text)
listSecurityKeysResponse_nextToken = Lens.lens (\ListSecurityKeysResponse' {nextToken} -> nextToken) (\s@ListSecurityKeysResponse' {} a -> s {nextToken = a} :: ListSecurityKeysResponse)

-- | The security keys.
listSecurityKeysResponse_securityKeys :: Lens.Lens' ListSecurityKeysResponse (Prelude.Maybe [SecurityKey])
listSecurityKeysResponse_securityKeys = Lens.lens (\ListSecurityKeysResponse' {securityKeys} -> securityKeys) (\s@ListSecurityKeysResponse' {} a -> s {securityKeys = a} :: ListSecurityKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSecurityKeysResponse_httpStatus :: Lens.Lens' ListSecurityKeysResponse Prelude.Int
listSecurityKeysResponse_httpStatus = Lens.lens (\ListSecurityKeysResponse' {httpStatus} -> httpStatus) (\s@ListSecurityKeysResponse' {} a -> s {httpStatus = a} :: ListSecurityKeysResponse)

instance Prelude.NFData ListSecurityKeysResponse where
  rnf ListSecurityKeysResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf securityKeys `Prelude.seq`
        Prelude.rnf httpStatus
