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
-- Module      : Amazonka.PaymentCryptography.ListKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the keys in the caller\'s Amazon Web Services account and Amazon
-- Web Services Region. You can filter the list of keys.
--
-- This is a paginated operation, which means that each response might
-- contain only a subset of all the keys. When the response contains only a
-- subset of keys, it includes a @NextToken@ value. Use this value in a
-- subsequent @ListKeys@ request to get more keys. When you receive a
-- response with no NextToken (or an empty or null value), that means there
-- are no more keys to get.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   CreateKey
--
-- -   DeleteKey
--
-- -   GetKey
--
-- This operation returns paginated results.
module Amazonka.PaymentCryptography.ListKeys
  ( -- * Creating a Request
    ListKeys (..),
    newListKeys,

    -- * Request Lenses
    listKeys_keyState,
    listKeys_maxResults,
    listKeys_nextToken,

    -- * Destructuring the Response
    ListKeysResponse (..),
    newListKeysResponse,

    -- * Response Lenses
    listKeysResponse_nextToken,
    listKeysResponse_httpStatus,
    listKeysResponse_keys,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKeys' smart constructor.
data ListKeys = ListKeys'
  { -- | The key state of the keys you want to list.
    keyState :: Prelude.Maybe KeyState,
    -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, Amazon Web Services Payment Cryptography
    -- does not return more than the specified number of items, but it might
    -- return fewer.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextToken@ from the
    -- truncated response you just received.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyState', 'listKeys_keyState' - The key state of the keys you want to list.
--
-- 'maxResults', 'listKeys_maxResults' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, Amazon Web Services Payment Cryptography
-- does not return more than the specified number of items, but it might
-- return fewer.
--
-- 'nextToken', 'listKeys_nextToken' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextToken@ from the
-- truncated response you just received.
newListKeys ::
  ListKeys
newListKeys =
  ListKeys'
    { keyState = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The key state of the keys you want to list.
listKeys_keyState :: Lens.Lens' ListKeys (Prelude.Maybe KeyState)
listKeys_keyState = Lens.lens (\ListKeys' {keyState} -> keyState) (\s@ListKeys' {} a -> s {keyState = a} :: ListKeys)

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, Amazon Web Services Payment Cryptography
-- does not return more than the specified number of items, but it might
-- return fewer.
listKeys_maxResults :: Lens.Lens' ListKeys (Prelude.Maybe Prelude.Natural)
listKeys_maxResults = Lens.lens (\ListKeys' {maxResults} -> maxResults) (\s@ListKeys' {} a -> s {maxResults = a} :: ListKeys)

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextToken@ from the
-- truncated response you just received.
listKeys_nextToken :: Lens.Lens' ListKeys (Prelude.Maybe Prelude.Text)
listKeys_nextToken = Lens.lens (\ListKeys' {nextToken} -> nextToken) (\s@ListKeys' {} a -> s {nextToken = a} :: ListKeys)

instance Core.AWSPager ListKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKeysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listKeysResponse_keys) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listKeys_nextToken
          Lens..~ rs
          Lens.^? listKeysResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListKeys where
  type AWSResponse ListKeys = ListKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeysResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Keys" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListKeys where
  hashWithSalt _salt ListKeys' {..} =
    _salt
      `Prelude.hashWithSalt` keyState
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListKeys where
  rnf ListKeys' {..} =
    Prelude.rnf keyState
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.ListKeys" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListKeys where
  toJSON ListKeys' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyState" Data..=) Prelude.<$> keyState,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListKeys where
  toPath = Prelude.const "/"

instance Data.ToQuery ListKeys where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListKeysResponse' smart constructor.
data ListKeysResponse = ListKeysResponse'
  { -- | The token for the next set of results, or an empty or null value if
    -- there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of keys created within the caller\'s Amazon Web Services
    -- account and Amazon Web Services Region.
    keys :: [KeySummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKeysResponse_nextToken' - The token for the next set of results, or an empty or null value if
-- there are no more results.
--
-- 'httpStatus', 'listKeysResponse_httpStatus' - The response's http status code.
--
-- 'keys', 'listKeysResponse_keys' - The list of keys created within the caller\'s Amazon Web Services
-- account and Amazon Web Services Region.
newListKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKeysResponse
newListKeysResponse pHttpStatus_ =
  ListKeysResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      keys = Prelude.mempty
    }

-- | The token for the next set of results, or an empty or null value if
-- there are no more results.
listKeysResponse_nextToken :: Lens.Lens' ListKeysResponse (Prelude.Maybe Prelude.Text)
listKeysResponse_nextToken = Lens.lens (\ListKeysResponse' {nextToken} -> nextToken) (\s@ListKeysResponse' {} a -> s {nextToken = a} :: ListKeysResponse)

-- | The response's http status code.
listKeysResponse_httpStatus :: Lens.Lens' ListKeysResponse Prelude.Int
listKeysResponse_httpStatus = Lens.lens (\ListKeysResponse' {httpStatus} -> httpStatus) (\s@ListKeysResponse' {} a -> s {httpStatus = a} :: ListKeysResponse)

-- | The list of keys created within the caller\'s Amazon Web Services
-- account and Amazon Web Services Region.
listKeysResponse_keys :: Lens.Lens' ListKeysResponse [KeySummary]
listKeysResponse_keys = Lens.lens (\ListKeysResponse' {keys} -> keys) (\s@ListKeysResponse' {} a -> s {keys = a} :: ListKeysResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListKeysResponse where
  rnf ListKeysResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keys
