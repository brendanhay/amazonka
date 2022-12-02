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
-- Module      : Amazonka.Transfer.ListHostKeys
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of host keys for the server that\'s specified by the
-- @ServerId@ parameter.
module Amazonka.Transfer.ListHostKeys
  ( -- * Creating a Request
    ListHostKeys (..),
    newListHostKeys,

    -- * Request Lenses
    listHostKeys_nextToken,
    listHostKeys_maxResults,
    listHostKeys_serverId,

    -- * Destructuring the Response
    ListHostKeysResponse (..),
    newListHostKeysResponse,

    -- * Response Lenses
    listHostKeysResponse_nextToken,
    listHostKeysResponse_httpStatus,
    listHostKeysResponse_serverId,
    listHostKeysResponse_hostKeys,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListHostKeys' smart constructor.
data ListHostKeys = ListHostKeys'
  { -- | When there are additional results that were not returned, a @NextToken@
    -- parameter is returned. You can use that value for a subsequent call to
    -- @ListHostKeys@ to continue listing results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of host keys to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the server that contains the host keys that you want
    -- to view.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHostKeys_nextToken' - When there are additional results that were not returned, a @NextToken@
-- parameter is returned. You can use that value for a subsequent call to
-- @ListHostKeys@ to continue listing results.
--
-- 'maxResults', 'listHostKeys_maxResults' - The maximum number of host keys to return.
--
-- 'serverId', 'listHostKeys_serverId' - The identifier of the server that contains the host keys that you want
-- to view.
newListHostKeys ::
  -- | 'serverId'
  Prelude.Text ->
  ListHostKeys
newListHostKeys pServerId_ =
  ListHostKeys'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      serverId = pServerId_
    }

-- | When there are additional results that were not returned, a @NextToken@
-- parameter is returned. You can use that value for a subsequent call to
-- @ListHostKeys@ to continue listing results.
listHostKeys_nextToken :: Lens.Lens' ListHostKeys (Prelude.Maybe Prelude.Text)
listHostKeys_nextToken = Lens.lens (\ListHostKeys' {nextToken} -> nextToken) (\s@ListHostKeys' {} a -> s {nextToken = a} :: ListHostKeys)

-- | The maximum number of host keys to return.
listHostKeys_maxResults :: Lens.Lens' ListHostKeys (Prelude.Maybe Prelude.Natural)
listHostKeys_maxResults = Lens.lens (\ListHostKeys' {maxResults} -> maxResults) (\s@ListHostKeys' {} a -> s {maxResults = a} :: ListHostKeys)

-- | The identifier of the server that contains the host keys that you want
-- to view.
listHostKeys_serverId :: Lens.Lens' ListHostKeys Prelude.Text
listHostKeys_serverId = Lens.lens (\ListHostKeys' {serverId} -> serverId) (\s@ListHostKeys' {} a -> s {serverId = a} :: ListHostKeys)

instance Core.AWSRequest ListHostKeys where
  type AWSResponse ListHostKeys = ListHostKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHostKeysResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
            Prelude.<*> (x Data..?> "HostKeys" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListHostKeys where
  hashWithSalt _salt ListHostKeys' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData ListHostKeys where
  rnf ListHostKeys' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf serverId

instance Data.ToHeaders ListHostKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListHostKeys" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHostKeys where
  toJSON ListHostKeys' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("ServerId" Data..= serverId)
          ]
      )

instance Data.ToPath ListHostKeys where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHostKeys where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHostKeysResponse' smart constructor.
data ListHostKeysResponse = ListHostKeysResponse'
  { -- | Returns a token that you can use to call @ListHostKeys@ again and
    -- receive additional results, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the server identifier that contains the listed host keys.
    serverId :: Prelude.Text,
    -- | Returns an array, where each item contains the details of a host key.
    hostKeys :: [ListedHostKey]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHostKeysResponse_nextToken' - Returns a token that you can use to call @ListHostKeys@ again and
-- receive additional results, if there are any.
--
-- 'httpStatus', 'listHostKeysResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'listHostKeysResponse_serverId' - Returns the server identifier that contains the listed host keys.
--
-- 'hostKeys', 'listHostKeysResponse_hostKeys' - Returns an array, where each item contains the details of a host key.
newListHostKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  ListHostKeysResponse
newListHostKeysResponse pHttpStatus_ pServerId_ =
  ListHostKeysResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      serverId = pServerId_,
      hostKeys = Prelude.mempty
    }

-- | Returns a token that you can use to call @ListHostKeys@ again and
-- receive additional results, if there are any.
listHostKeysResponse_nextToken :: Lens.Lens' ListHostKeysResponse (Prelude.Maybe Prelude.Text)
listHostKeysResponse_nextToken = Lens.lens (\ListHostKeysResponse' {nextToken} -> nextToken) (\s@ListHostKeysResponse' {} a -> s {nextToken = a} :: ListHostKeysResponse)

-- | The response's http status code.
listHostKeysResponse_httpStatus :: Lens.Lens' ListHostKeysResponse Prelude.Int
listHostKeysResponse_httpStatus = Lens.lens (\ListHostKeysResponse' {httpStatus} -> httpStatus) (\s@ListHostKeysResponse' {} a -> s {httpStatus = a} :: ListHostKeysResponse)

-- | Returns the server identifier that contains the listed host keys.
listHostKeysResponse_serverId :: Lens.Lens' ListHostKeysResponse Prelude.Text
listHostKeysResponse_serverId = Lens.lens (\ListHostKeysResponse' {serverId} -> serverId) (\s@ListHostKeysResponse' {} a -> s {serverId = a} :: ListHostKeysResponse)

-- | Returns an array, where each item contains the details of a host key.
listHostKeysResponse_hostKeys :: Lens.Lens' ListHostKeysResponse [ListedHostKey]
listHostKeysResponse_hostKeys = Lens.lens (\ListHostKeysResponse' {hostKeys} -> hostKeys) (\s@ListHostKeysResponse' {} a -> s {hostKeys = a} :: ListHostKeysResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListHostKeysResponse where
  rnf ListHostKeysResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf hostKeys
