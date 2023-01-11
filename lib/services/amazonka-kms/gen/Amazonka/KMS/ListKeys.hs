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
-- Module      : Amazonka.KMS.ListKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all KMS keys in the caller\'s Amazon Web Services account
-- and Region.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ListKeys>
-- (IAM policy)
--
-- __Related operations:__
--
-- -   CreateKey
--
-- -   DescribeKey
--
-- -   ListAliases
--
-- -   ListResourceTags
--
-- This operation returns paginated results.
module Amazonka.KMS.ListKeys
  ( -- * Creating a Request
    ListKeys (..),
    newListKeys,

    -- * Request Lenses
    listKeys_limit,
    listKeys_marker,

    -- * Destructuring the Response
    ListKeysResponse (..),
    newListKeysResponse,

    -- * Response Lenses
    listKeysResponse_keys,
    listKeysResponse_nextMarker,
    listKeysResponse_truncated,
    listKeysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKeys' smart constructor.
data ListKeys = ListKeys'
  { -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, KMS does not return more than the specified
    -- number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and
    -- 1000, inclusive. If you do not include a value, it defaults to 100.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Prelude.Maybe Prelude.Text
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
-- 'limit', 'listKeys_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 1000, inclusive. If you do not include a value, it defaults to 100.
--
-- 'marker', 'listKeys_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
newListKeys ::
  ListKeys
newListKeys =
  ListKeys'
    { limit = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 1000, inclusive. If you do not include a value, it defaults to 100.
listKeys_limit :: Lens.Lens' ListKeys (Prelude.Maybe Prelude.Natural)
listKeys_limit = Lens.lens (\ListKeys' {limit} -> limit) (\s@ListKeys' {} a -> s {limit = a} :: ListKeys)

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
listKeys_marker :: Lens.Lens' ListKeys (Prelude.Maybe Prelude.Text)
listKeys_marker = Lens.lens (\ListKeys' {marker} -> marker) (\s@ListKeys' {} a -> s {marker = a} :: ListKeys)

instance Core.AWSPager ListKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKeysResponse_truncated Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listKeysResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listKeys_marker
          Lens..~ rs
          Lens.^? listKeysResponse_nextMarker Prelude.. Lens._Just

instance Core.AWSRequest ListKeys where
  type AWSResponse ListKeys = ListKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeysResponse'
            Prelude.<$> (x Data..?> "Keys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (x Data..?> "Truncated")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKeys where
  hashWithSalt _salt ListKeys' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker

instance Prelude.NFData ListKeys where
  rnf ListKeys' {..} =
    Prelude.rnf limit `Prelude.seq` Prelude.rnf marker

instance Data.ToHeaders ListKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.ListKeys" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListKeys where
  toJSON ListKeys' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker
          ]
      )

instance Data.ToPath ListKeys where
  toPath = Prelude.const "/"

instance Data.ToQuery ListKeys where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListKeysResponse' smart constructor.
data ListKeysResponse = ListKeysResponse'
  { -- | A list of KMS keys.
    keys :: Prelude.Maybe [KeyListEntry],
    -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'keys', 'listKeysResponse_keys' - A list of KMS keys.
--
-- 'nextMarker', 'listKeysResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- 'truncated', 'listKeysResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
--
-- 'httpStatus', 'listKeysResponse_httpStatus' - The response's http status code.
newListKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKeysResponse
newListKeysResponse pHttpStatus_ =
  ListKeysResponse'
    { keys = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      truncated = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of KMS keys.
listKeysResponse_keys :: Lens.Lens' ListKeysResponse (Prelude.Maybe [KeyListEntry])
listKeysResponse_keys = Lens.lens (\ListKeysResponse' {keys} -> keys) (\s@ListKeysResponse' {} a -> s {keys = a} :: ListKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
listKeysResponse_nextMarker :: Lens.Lens' ListKeysResponse (Prelude.Maybe Prelude.Text)
listKeysResponse_nextMarker = Lens.lens (\ListKeysResponse' {nextMarker} -> nextMarker) (\s@ListKeysResponse' {} a -> s {nextMarker = a} :: ListKeysResponse)

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
listKeysResponse_truncated :: Lens.Lens' ListKeysResponse (Prelude.Maybe Prelude.Bool)
listKeysResponse_truncated = Lens.lens (\ListKeysResponse' {truncated} -> truncated) (\s@ListKeysResponse' {} a -> s {truncated = a} :: ListKeysResponse)

-- | The response's http status code.
listKeysResponse_httpStatus :: Lens.Lens' ListKeysResponse Prelude.Int
listKeysResponse_httpStatus = Lens.lens (\ListKeysResponse' {httpStatus} -> httpStatus) (\s@ListKeysResponse' {} a -> s {httpStatus = a} :: ListKeysResponse)

instance Prelude.NFData ListKeysResponse where
  rnf ListKeysResponse' {..} =
    Prelude.rnf keys
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf truncated
      `Prelude.seq` Prelude.rnf httpStatus
