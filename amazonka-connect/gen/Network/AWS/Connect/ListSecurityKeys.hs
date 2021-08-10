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
-- Module      : Network.AWS.Connect.ListSecurityKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.Connect.ListSecurityKeys
  ( -- * Creating a Request
    ListSecurityKeys (..),
    newListSecurityKeys,

    -- * Request Lenses
    listSecurityKeys_nextToken,
    listSecurityKeys_maxResults,
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSecurityKeys' smart constructor.
data ListSecurityKeys = ListSecurityKeys'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance.
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
-- 'nextToken', 'listSecurityKeys_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listSecurityKeys_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listSecurityKeys_instanceId' - The identifier of the Amazon Connect instance.
newListSecurityKeys ::
  -- | 'instanceId'
  Prelude.Text ->
  ListSecurityKeys
newListSecurityKeys pInstanceId_ =
  ListSecurityKeys'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listSecurityKeys_nextToken :: Lens.Lens' ListSecurityKeys (Prelude.Maybe Prelude.Text)
listSecurityKeys_nextToken = Lens.lens (\ListSecurityKeys' {nextToken} -> nextToken) (\s@ListSecurityKeys' {} a -> s {nextToken = a} :: ListSecurityKeys)

-- | The maximum number of results to return per page.
listSecurityKeys_maxResults :: Lens.Lens' ListSecurityKeys (Prelude.Maybe Prelude.Natural)
listSecurityKeys_maxResults = Lens.lens (\ListSecurityKeys' {maxResults} -> maxResults) (\s@ListSecurityKeys' {} a -> s {maxResults = a} :: ListSecurityKeys)

-- | The identifier of the Amazon Connect instance.
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityKeysResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "SecurityKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecurityKeys

instance Prelude.NFData ListSecurityKeys

instance Core.ToHeaders ListSecurityKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListSecurityKeys where
  toPath ListSecurityKeys' {..} =
    Prelude.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/security-keys"
      ]

instance Core.ToQuery ListSecurityKeys where
  toQuery ListSecurityKeys' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
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
listSecurityKeysResponse_securityKeys = Lens.lens (\ListSecurityKeysResponse' {securityKeys} -> securityKeys) (\s@ListSecurityKeysResponse' {} a -> s {securityKeys = a} :: ListSecurityKeysResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSecurityKeysResponse_httpStatus :: Lens.Lens' ListSecurityKeysResponse Prelude.Int
listSecurityKeysResponse_httpStatus = Lens.lens (\ListSecurityKeysResponse' {httpStatus} -> httpStatus) (\s@ListSecurityKeysResponse' {} a -> s {httpStatus = a} :: ListSecurityKeysResponse)

instance Prelude.NFData ListSecurityKeysResponse
