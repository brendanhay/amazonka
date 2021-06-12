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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSecurityKeys' smart constructor.
data ListSecurityKeys = ListSecurityKeys'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListSecurityKeys
newListSecurityKeys pInstanceId_ =
  ListSecurityKeys'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listSecurityKeys_nextToken :: Lens.Lens' ListSecurityKeys (Core.Maybe Core.Text)
listSecurityKeys_nextToken = Lens.lens (\ListSecurityKeys' {nextToken} -> nextToken) (\s@ListSecurityKeys' {} a -> s {nextToken = a} :: ListSecurityKeys)

-- | The maximum number of results to return per page.
listSecurityKeys_maxResults :: Lens.Lens' ListSecurityKeys (Core.Maybe Core.Natural)
listSecurityKeys_maxResults = Lens.lens (\ListSecurityKeys' {maxResults} -> maxResults) (\s@ListSecurityKeys' {} a -> s {maxResults = a} :: ListSecurityKeys)

-- | The identifier of the Amazon Connect instance.
listSecurityKeys_instanceId :: Lens.Lens' ListSecurityKeys Core.Text
listSecurityKeys_instanceId = Lens.lens (\ListSecurityKeys' {instanceId} -> instanceId) (\s@ListSecurityKeys' {} a -> s {instanceId = a} :: ListSecurityKeys)

instance Core.AWSPager ListSecurityKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityKeysResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityKeysResponse_securityKeys
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSecurityKeys_nextToken
          Lens..~ rs
          Lens.^? listSecurityKeysResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListSecurityKeys where
  type
    AWSResponse ListSecurityKeys =
      ListSecurityKeysResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityKeysResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "SecurityKeys" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSecurityKeys

instance Core.NFData ListSecurityKeys

instance Core.ToHeaders ListSecurityKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListSecurityKeys where
  toPath ListSecurityKeys' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/security-keys"
      ]

instance Core.ToQuery ListSecurityKeys where
  toQuery ListSecurityKeys' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSecurityKeysResponse' smart constructor.
data ListSecurityKeysResponse = ListSecurityKeysResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The security keys.
    securityKeys :: Core.Maybe [SecurityKey],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListSecurityKeysResponse
newListSecurityKeysResponse pHttpStatus_ =
  ListSecurityKeysResponse'
    { nextToken = Core.Nothing,
      securityKeys = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listSecurityKeysResponse_nextToken :: Lens.Lens' ListSecurityKeysResponse (Core.Maybe Core.Text)
listSecurityKeysResponse_nextToken = Lens.lens (\ListSecurityKeysResponse' {nextToken} -> nextToken) (\s@ListSecurityKeysResponse' {} a -> s {nextToken = a} :: ListSecurityKeysResponse)

-- | The security keys.
listSecurityKeysResponse_securityKeys :: Lens.Lens' ListSecurityKeysResponse (Core.Maybe [SecurityKey])
listSecurityKeysResponse_securityKeys = Lens.lens (\ListSecurityKeysResponse' {securityKeys} -> securityKeys) (\s@ListSecurityKeysResponse' {} a -> s {securityKeys = a} :: ListSecurityKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSecurityKeysResponse_httpStatus :: Lens.Lens' ListSecurityKeysResponse Core.Int
listSecurityKeysResponse_httpStatus = Lens.lens (\ListSecurityKeysResponse' {httpStatus} -> httpStatus) (\s@ListSecurityKeysResponse' {} a -> s {httpStatus = a} :: ListSecurityKeysResponse)

instance Core.NFData ListSecurityKeysResponse
