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
-- Module      : Amazonka.CloudTrail.ListPublicKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all public keys whose private keys were used to sign the digest
-- files within the specified time range. The public key is needed to
-- validate digest files that were signed with its corresponding private
-- key.
--
-- CloudTrail uses different private and public key pairs per region. Each
-- digest file is signed with a private key unique to its region. When you
-- validate a digest file from a specific region, you must look in the same
-- region for its corresponding public key.
--
-- This operation returns paginated results.
module Amazonka.CloudTrail.ListPublicKeys
  ( -- * Creating a Request
    ListPublicKeys (..),
    newListPublicKeys,

    -- * Request Lenses
    listPublicKeys_endTime,
    listPublicKeys_nextToken,
    listPublicKeys_startTime,

    -- * Destructuring the Response
    ListPublicKeysResponse (..),
    newListPublicKeysResponse,

    -- * Response Lenses
    listPublicKeysResponse_nextToken,
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests the public keys for a specified time range.
--
-- /See:/ 'newListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { -- | Optionally specifies, in UTC, the end of the time range to look up
    -- public keys for CloudTrail digest files. If not specified, the current
    -- time is used.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Reserved for future use.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optionally specifies, in UTC, the start of the time range to look up
    -- public keys for CloudTrail digest files. If not specified, the current
    -- time is used, and the current public key is returned.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPublicKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'listPublicKeys_endTime' - Optionally specifies, in UTC, the end of the time range to look up
-- public keys for CloudTrail digest files. If not specified, the current
-- time is used.
--
-- 'nextToken', 'listPublicKeys_nextToken' - Reserved for future use.
--
-- 'startTime', 'listPublicKeys_startTime' - Optionally specifies, in UTC, the start of the time range to look up
-- public keys for CloudTrail digest files. If not specified, the current
-- time is used, and the current public key is returned.
newListPublicKeys ::
  ListPublicKeys
newListPublicKeys =
  ListPublicKeys'
    { endTime = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Optionally specifies, in UTC, the end of the time range to look up
-- public keys for CloudTrail digest files. If not specified, the current
-- time is used.
listPublicKeys_endTime :: Lens.Lens' ListPublicKeys (Prelude.Maybe Prelude.UTCTime)
listPublicKeys_endTime = Lens.lens (\ListPublicKeys' {endTime} -> endTime) (\s@ListPublicKeys' {} a -> s {endTime = a} :: ListPublicKeys) Prelude.. Lens.mapping Data._Time

-- | Reserved for future use.
listPublicKeys_nextToken :: Lens.Lens' ListPublicKeys (Prelude.Maybe Prelude.Text)
listPublicKeys_nextToken = Lens.lens (\ListPublicKeys' {nextToken} -> nextToken) (\s@ListPublicKeys' {} a -> s {nextToken = a} :: ListPublicKeys)

-- | Optionally specifies, in UTC, the start of the time range to look up
-- public keys for CloudTrail digest files. If not specified, the current
-- time is used, and the current public key is returned.
listPublicKeys_startTime :: Lens.Lens' ListPublicKeys (Prelude.Maybe Prelude.UTCTime)
listPublicKeys_startTime = Lens.lens (\ListPublicKeys' {startTime} -> startTime) (\s@ListPublicKeys' {} a -> s {startTime = a} :: ListPublicKeys) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager ListPublicKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPublicKeysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPublicKeysResponse_publicKeyList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPublicKeys_nextToken
              Lens..~ rs
              Lens.^? listPublicKeysResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPublicKeys where
  type
    AWSResponse ListPublicKeys =
      ListPublicKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPublicKeysResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PublicKeyList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPublicKeys where
  hashWithSalt _salt ListPublicKeys' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ListPublicKeys where
  rnf ListPublicKeys' {..} =
    Prelude.rnf endTime `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf startTime

instance Data.ToHeaders ListPublicKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListPublicKeys" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPublicKeys where
  toJSON ListPublicKeys' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )

instance Data.ToPath ListPublicKeys where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPublicKeys where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { -- | Reserved for future use.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains an array of PublicKey objects.
    --
    -- The returned public keys may have validity time ranges that overlap.
    publicKeyList :: Prelude.Maybe [PublicKey],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListPublicKeysResponse
newListPublicKeysResponse pHttpStatus_ =
  ListPublicKeysResponse'
    { nextToken =
        Prelude.Nothing,
      publicKeyList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Reserved for future use.
listPublicKeysResponse_nextToken :: Lens.Lens' ListPublicKeysResponse (Prelude.Maybe Prelude.Text)
listPublicKeysResponse_nextToken = Lens.lens (\ListPublicKeysResponse' {nextToken} -> nextToken) (\s@ListPublicKeysResponse' {} a -> s {nextToken = a} :: ListPublicKeysResponse)

-- | Contains an array of PublicKey objects.
--
-- The returned public keys may have validity time ranges that overlap.
listPublicKeysResponse_publicKeyList :: Lens.Lens' ListPublicKeysResponse (Prelude.Maybe [PublicKey])
listPublicKeysResponse_publicKeyList = Lens.lens (\ListPublicKeysResponse' {publicKeyList} -> publicKeyList) (\s@ListPublicKeysResponse' {} a -> s {publicKeyList = a} :: ListPublicKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPublicKeysResponse_httpStatus :: Lens.Lens' ListPublicKeysResponse Prelude.Int
listPublicKeysResponse_httpStatus = Lens.lens (\ListPublicKeysResponse' {httpStatus} -> httpStatus) (\s@ListPublicKeysResponse' {} a -> s {httpStatus = a} :: ListPublicKeysResponse)

instance Prelude.NFData ListPublicKeysResponse where
  rnf ListPublicKeysResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf publicKeyList `Prelude.seq`
        Prelude.rnf httpStatus
