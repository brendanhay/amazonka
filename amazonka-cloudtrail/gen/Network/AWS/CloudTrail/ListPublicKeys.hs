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
-- Module      : Network.AWS.CloudTrail.ListPublicKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all public keys whose private keys were used to sign the digest
-- files within the specified time range. The public key is needed to
-- validate digest files that were signed with its corresponding private
-- key.
--
-- CloudTrail uses different private\/public key pairs per region. Each
-- digest file is signed with a private key unique to its region.
-- Therefore, when you validate a digest file from a particular region, you
-- must look in the same region for its corresponding public key.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListPublicKeys
  ( -- * Creating a Request
    ListPublicKeys (..),
    newListPublicKeys,

    -- * Request Lenses
    listPublicKeys_nextToken,
    listPublicKeys_startTime,
    listPublicKeys_endTime,

    -- * Destructuring the Response
    ListPublicKeysResponse (..),
    newListPublicKeysResponse,

    -- * Response Lenses
    listPublicKeysResponse_nextToken,
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests the public keys for a specified time range.
--
-- /See:/ 'newListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { -- | Reserved for future use.
    nextToken :: Core.Maybe Core.Text,
    -- | Optionally specifies, in UTC, the start of the time range to look up
    -- public keys for CloudTrail digest files. If not specified, the current
    -- time is used, and the current public key is returned.
    startTime :: Core.Maybe Core.POSIX,
    -- | Optionally specifies, in UTC, the end of the time range to look up
    -- public keys for CloudTrail digest files. If not specified, the current
    -- time is used.
    endTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPublicKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPublicKeys_nextToken' - Reserved for future use.
--
-- 'startTime', 'listPublicKeys_startTime' - Optionally specifies, in UTC, the start of the time range to look up
-- public keys for CloudTrail digest files. If not specified, the current
-- time is used, and the current public key is returned.
--
-- 'endTime', 'listPublicKeys_endTime' - Optionally specifies, in UTC, the end of the time range to look up
-- public keys for CloudTrail digest files. If not specified, the current
-- time is used.
newListPublicKeys ::
  ListPublicKeys
newListPublicKeys =
  ListPublicKeys'
    { nextToken = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing
    }

-- | Reserved for future use.
listPublicKeys_nextToken :: Lens.Lens' ListPublicKeys (Core.Maybe Core.Text)
listPublicKeys_nextToken = Lens.lens (\ListPublicKeys' {nextToken} -> nextToken) (\s@ListPublicKeys' {} a -> s {nextToken = a} :: ListPublicKeys)

-- | Optionally specifies, in UTC, the start of the time range to look up
-- public keys for CloudTrail digest files. If not specified, the current
-- time is used, and the current public key is returned.
listPublicKeys_startTime :: Lens.Lens' ListPublicKeys (Core.Maybe Core.UTCTime)
listPublicKeys_startTime = Lens.lens (\ListPublicKeys' {startTime} -> startTime) (\s@ListPublicKeys' {} a -> s {startTime = a} :: ListPublicKeys) Core.. Lens.mapping Core._Time

-- | Optionally specifies, in UTC, the end of the time range to look up
-- public keys for CloudTrail digest files. If not specified, the current
-- time is used.
listPublicKeys_endTime :: Lens.Lens' ListPublicKeys (Core.Maybe Core.UTCTime)
listPublicKeys_endTime = Lens.lens (\ListPublicKeys' {endTime} -> endTime) (\s@ListPublicKeys' {} a -> s {endTime = a} :: ListPublicKeys) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListPublicKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPublicKeysResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPublicKeysResponse_publicKeyList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPublicKeys_nextToken
          Lens..~ rs
          Lens.^? listPublicKeysResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListPublicKeys where
  type
    AWSResponse ListPublicKeys =
      ListPublicKeysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPublicKeysResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "PublicKeyList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPublicKeys

instance Core.NFData ListPublicKeys

instance Core.ToHeaders ListPublicKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListPublicKeys" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPublicKeys where
  toJSON ListPublicKeys' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("StartTime" Core..=) Core.<$> startTime,
            ("EndTime" Core..=) Core.<$> endTime
          ]
      )

instance Core.ToPath ListPublicKeys where
  toPath = Core.const "/"

instance Core.ToQuery ListPublicKeys where
  toQuery = Core.const Core.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { -- | Reserved for future use.
    nextToken :: Core.Maybe Core.Text,
    -- | Contains an array of PublicKey objects.
    --
    -- The returned public keys may have validity time ranges that overlap.
    publicKeyList :: Core.Maybe [PublicKey],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPublicKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPublicKeysResponse_nextToken' - Reserved for future use.
--
-- 'publicKeyList', 'listPublicKeysResponse_publicKeyList' - Contains an array of PublicKey objects.
--
-- The returned public keys may have validity time ranges that overlap.
--
-- 'httpStatus', 'listPublicKeysResponse_httpStatus' - The response's http status code.
newListPublicKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPublicKeysResponse
newListPublicKeysResponse pHttpStatus_ =
  ListPublicKeysResponse'
    { nextToken = Core.Nothing,
      publicKeyList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Reserved for future use.
listPublicKeysResponse_nextToken :: Lens.Lens' ListPublicKeysResponse (Core.Maybe Core.Text)
listPublicKeysResponse_nextToken = Lens.lens (\ListPublicKeysResponse' {nextToken} -> nextToken) (\s@ListPublicKeysResponse' {} a -> s {nextToken = a} :: ListPublicKeysResponse)

-- | Contains an array of PublicKey objects.
--
-- The returned public keys may have validity time ranges that overlap.
listPublicKeysResponse_publicKeyList :: Lens.Lens' ListPublicKeysResponse (Core.Maybe [PublicKey])
listPublicKeysResponse_publicKeyList = Lens.lens (\ListPublicKeysResponse' {publicKeyList} -> publicKeyList) (\s@ListPublicKeysResponse' {} a -> s {publicKeyList = a} :: ListPublicKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPublicKeysResponse_httpStatus :: Lens.Lens' ListPublicKeysResponse Core.Int
listPublicKeysResponse_httpStatus = Lens.lens (\ListPublicKeysResponse' {httpStatus} -> httpStatus) (\s@ListPublicKeysResponse' {} a -> s {httpStatus = a} :: ListPublicKeysResponse)

instance Core.NFData ListPublicKeysResponse
