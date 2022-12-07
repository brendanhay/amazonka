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
-- Module      : Amazonka.Nimble.PutLaunchProfileMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add\/update users with given persona to launch profile membership.
module Amazonka.Nimble.PutLaunchProfileMembers
  ( -- * Creating a Request
    PutLaunchProfileMembers (..),
    newPutLaunchProfileMembers,

    -- * Request Lenses
    putLaunchProfileMembers_clientToken,
    putLaunchProfileMembers_identityStoreId,
    putLaunchProfileMembers_launchProfileId,
    putLaunchProfileMembers_members,
    putLaunchProfileMembers_studioId,

    -- * Destructuring the Response
    PutLaunchProfileMembersResponse (..),
    newPutLaunchProfileMembersResponse,

    -- * Response Lenses
    putLaunchProfileMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLaunchProfileMembers' smart constructor.
data PutLaunchProfileMembers = PutLaunchProfileMembers'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the identity store.
    identityStoreId :: Prelude.Text,
    -- | The Launch Profile ID.
    launchProfileId :: Prelude.Text,
    -- | A list of members.
    members :: Prelude.NonEmpty NewLaunchProfileMember,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLaunchProfileMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'putLaunchProfileMembers_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'identityStoreId', 'putLaunchProfileMembers_identityStoreId' - The ID of the identity store.
--
-- 'launchProfileId', 'putLaunchProfileMembers_launchProfileId' - The Launch Profile ID.
--
-- 'members', 'putLaunchProfileMembers_members' - A list of members.
--
-- 'studioId', 'putLaunchProfileMembers_studioId' - The studio ID.
newPutLaunchProfileMembers ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'members'
  Prelude.NonEmpty NewLaunchProfileMember ->
  -- | 'studioId'
  Prelude.Text ->
  PutLaunchProfileMembers
newPutLaunchProfileMembers
  pIdentityStoreId_
  pLaunchProfileId_
  pMembers_
  pStudioId_ =
    PutLaunchProfileMembers'
      { clientToken =
          Prelude.Nothing,
        identityStoreId = pIdentityStoreId_,
        launchProfileId = pLaunchProfileId_,
        members = Lens.coerced Lens.# pMembers_,
        studioId = pStudioId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
putLaunchProfileMembers_clientToken :: Lens.Lens' PutLaunchProfileMembers (Prelude.Maybe Prelude.Text)
putLaunchProfileMembers_clientToken = Lens.lens (\PutLaunchProfileMembers' {clientToken} -> clientToken) (\s@PutLaunchProfileMembers' {} a -> s {clientToken = a} :: PutLaunchProfileMembers)

-- | The ID of the identity store.
putLaunchProfileMembers_identityStoreId :: Lens.Lens' PutLaunchProfileMembers Prelude.Text
putLaunchProfileMembers_identityStoreId = Lens.lens (\PutLaunchProfileMembers' {identityStoreId} -> identityStoreId) (\s@PutLaunchProfileMembers' {} a -> s {identityStoreId = a} :: PutLaunchProfileMembers)

-- | The Launch Profile ID.
putLaunchProfileMembers_launchProfileId :: Lens.Lens' PutLaunchProfileMembers Prelude.Text
putLaunchProfileMembers_launchProfileId = Lens.lens (\PutLaunchProfileMembers' {launchProfileId} -> launchProfileId) (\s@PutLaunchProfileMembers' {} a -> s {launchProfileId = a} :: PutLaunchProfileMembers)

-- | A list of members.
putLaunchProfileMembers_members :: Lens.Lens' PutLaunchProfileMembers (Prelude.NonEmpty NewLaunchProfileMember)
putLaunchProfileMembers_members = Lens.lens (\PutLaunchProfileMembers' {members} -> members) (\s@PutLaunchProfileMembers' {} a -> s {members = a} :: PutLaunchProfileMembers) Prelude.. Lens.coerced

-- | The studio ID.
putLaunchProfileMembers_studioId :: Lens.Lens' PutLaunchProfileMembers Prelude.Text
putLaunchProfileMembers_studioId = Lens.lens (\PutLaunchProfileMembers' {studioId} -> studioId) (\s@PutLaunchProfileMembers' {} a -> s {studioId = a} :: PutLaunchProfileMembers)

instance Core.AWSRequest PutLaunchProfileMembers where
  type
    AWSResponse PutLaunchProfileMembers =
      PutLaunchProfileMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutLaunchProfileMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLaunchProfileMembers where
  hashWithSalt _salt PutLaunchProfileMembers' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` members
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData PutLaunchProfileMembers where
  rnf PutLaunchProfileMembers' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf members
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders PutLaunchProfileMembers where
  toHeaders PutLaunchProfileMembers' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON PutLaunchProfileMembers where
  toJSON PutLaunchProfileMembers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("identityStoreId" Data..= identityStoreId),
            Prelude.Just ("members" Data..= members)
          ]
      )

instance Data.ToPath PutLaunchProfileMembers where
  toPath PutLaunchProfileMembers' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/launch-profiles/",
        Data.toBS launchProfileId,
        "/membership"
      ]

instance Data.ToQuery PutLaunchProfileMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLaunchProfileMembersResponse' smart constructor.
data PutLaunchProfileMembersResponse = PutLaunchProfileMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLaunchProfileMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putLaunchProfileMembersResponse_httpStatus' - The response's http status code.
newPutLaunchProfileMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutLaunchProfileMembersResponse
newPutLaunchProfileMembersResponse pHttpStatus_ =
  PutLaunchProfileMembersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putLaunchProfileMembersResponse_httpStatus :: Lens.Lens' PutLaunchProfileMembersResponse Prelude.Int
putLaunchProfileMembersResponse_httpStatus = Lens.lens (\PutLaunchProfileMembersResponse' {httpStatus} -> httpStatus) (\s@PutLaunchProfileMembersResponse' {} a -> s {httpStatus = a} :: PutLaunchProfileMembersResponse)

instance
  Prelude.NFData
    PutLaunchProfileMembersResponse
  where
  rnf PutLaunchProfileMembersResponse' {..} =
    Prelude.rnf httpStatus
