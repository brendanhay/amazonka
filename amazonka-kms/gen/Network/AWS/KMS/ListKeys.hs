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
-- Module      : Network.AWS.KMS.ListKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all customer master keys (CMKs) in the caller\'s AWS
-- account and Region.
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
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
module Network.AWS.KMS.ListKeys
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
    listKeysResponse_nextMarker,
    listKeysResponse_keys,
    listKeysResponse_truncated,
    listKeysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListKeys' smart constructor.
data ListKeys = ListKeys'
  { -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, AWS KMS does not return more than the
    -- specified number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and
    -- 1000, inclusive. If you do not include a value, it defaults to 100.
    limit :: Core.Maybe Core.Natural,
    -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listKeys_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, AWS KMS does not return more than the
-- specified number of items, but it might return fewer.
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
    { limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, AWS KMS does not return more than the
-- specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and
-- 1000, inclusive. If you do not include a value, it defaults to 100.
listKeys_limit :: Lens.Lens' ListKeys (Core.Maybe Core.Natural)
listKeys_limit = Lens.lens (\ListKeys' {limit} -> limit) (\s@ListKeys' {} a -> s {limit = a} :: ListKeys)

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
listKeys_marker :: Lens.Lens' ListKeys (Core.Maybe Core.Text)
listKeys_marker = Lens.lens (\ListKeys' {marker} -> marker) (\s@ListKeys' {} a -> s {marker = a} :: ListKeys)

instance Core.AWSPager ListKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKeysResponse_truncated Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listKeysResponse_nextMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listKeys_marker
          Lens..~ rs
          Lens.^? listKeysResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListKeys where
  type AWSResponse ListKeys = ListKeysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeysResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "Keys" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Truncated")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListKeys

instance Core.NFData ListKeys

instance Core.ToHeaders ListKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.ListKeys" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListKeys where
  toJSON ListKeys' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath ListKeys where
  toPath = Core.const "/"

instance Core.ToQuery ListKeys where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListKeysResponse' smart constructor.
data ListKeysResponse = ListKeysResponse'
  { -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Core.Maybe Core.Text,
    -- | A list of customer master keys (CMKs).
    keys :: Core.Maybe [KeyListEntry],
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listKeysResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- 'keys', 'listKeysResponse_keys' - A list of customer master keys (CMKs).
--
-- 'truncated', 'listKeysResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
--
-- 'httpStatus', 'listKeysResponse_httpStatus' - The response's http status code.
newListKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListKeysResponse
newListKeysResponse pHttpStatus_ =
  ListKeysResponse'
    { nextMarker = Core.Nothing,
      keys = Core.Nothing,
      truncated = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
listKeysResponse_nextMarker :: Lens.Lens' ListKeysResponse (Core.Maybe Core.Text)
listKeysResponse_nextMarker = Lens.lens (\ListKeysResponse' {nextMarker} -> nextMarker) (\s@ListKeysResponse' {} a -> s {nextMarker = a} :: ListKeysResponse)

-- | A list of customer master keys (CMKs).
listKeysResponse_keys :: Lens.Lens' ListKeysResponse (Core.Maybe [KeyListEntry])
listKeysResponse_keys = Lens.lens (\ListKeysResponse' {keys} -> keys) (\s@ListKeysResponse' {} a -> s {keys = a} :: ListKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
listKeysResponse_truncated :: Lens.Lens' ListKeysResponse (Core.Maybe Core.Bool)
listKeysResponse_truncated = Lens.lens (\ListKeysResponse' {truncated} -> truncated) (\s@ListKeysResponse' {} a -> s {truncated = a} :: ListKeysResponse)

-- | The response's http status code.
listKeysResponse_httpStatus :: Lens.Lens' ListKeysResponse Core.Int
listKeysResponse_httpStatus = Lens.lens (\ListKeysResponse' {httpStatus} -> httpStatus) (\s@ListKeysResponse' {} a -> s {httpStatus = a} :: ListKeysResponse)

instance Core.NFData ListKeysResponse
