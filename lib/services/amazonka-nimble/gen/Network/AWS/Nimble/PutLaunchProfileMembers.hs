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
-- Module      : Network.AWS.Nimble.PutLaunchProfileMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add\/update users with given persona to launch profile membership.
module Network.AWS.Nimble.PutLaunchProfileMembers
  ( -- * Creating a Request
    PutLaunchProfileMembers (..),
    newPutLaunchProfileMembers,

    -- * Request Lenses
    putLaunchProfileMembers_clientToken,
    putLaunchProfileMembers_studioId,
    putLaunchProfileMembers_members,
    putLaunchProfileMembers_launchProfileId,
    putLaunchProfileMembers_identityStoreId,

    -- * Destructuring the Response
    PutLaunchProfileMembersResponse (..),
    newPutLaunchProfileMembersResponse,

    -- * Response Lenses
    putLaunchProfileMembersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Nimble.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Information about a launch profile membership.
--
-- /See:/ 'newPutLaunchProfileMembers' smart constructor.
data PutLaunchProfileMembers = PutLaunchProfileMembers'
  { -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | A list of members.
    members :: Prelude.NonEmpty NewLaunchProfileMember,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Text,
    -- | The ID of the identity store.
    identityStoreId :: Prelude.Text
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
-- 'clientToken', 'putLaunchProfileMembers_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'studioId', 'putLaunchProfileMembers_studioId' - The studio ID.
--
-- 'members', 'putLaunchProfileMembers_members' - A list of members.
--
-- 'launchProfileId', 'putLaunchProfileMembers_launchProfileId' - The launch profile ID.
--
-- 'identityStoreId', 'putLaunchProfileMembers_identityStoreId' - The ID of the identity store.
newPutLaunchProfileMembers ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'members'
  Prelude.NonEmpty NewLaunchProfileMember ->
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'identityStoreId'
  Prelude.Text ->
  PutLaunchProfileMembers
newPutLaunchProfileMembers
  pStudioId_
  pMembers_
  pLaunchProfileId_
  pIdentityStoreId_ =
    PutLaunchProfileMembers'
      { clientToken =
          Prelude.Nothing,
        studioId = pStudioId_,
        members = Lens.coerced Lens.# pMembers_,
        launchProfileId = pLaunchProfileId_,
        identityStoreId = pIdentityStoreId_
      }

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
putLaunchProfileMembers_clientToken :: Lens.Lens' PutLaunchProfileMembers (Prelude.Maybe Prelude.Text)
putLaunchProfileMembers_clientToken = Lens.lens (\PutLaunchProfileMembers' {clientToken} -> clientToken) (\s@PutLaunchProfileMembers' {} a -> s {clientToken = a} :: PutLaunchProfileMembers)

-- | The studio ID.
putLaunchProfileMembers_studioId :: Lens.Lens' PutLaunchProfileMembers Prelude.Text
putLaunchProfileMembers_studioId = Lens.lens (\PutLaunchProfileMembers' {studioId} -> studioId) (\s@PutLaunchProfileMembers' {} a -> s {studioId = a} :: PutLaunchProfileMembers)

-- | A list of members.
putLaunchProfileMembers_members :: Lens.Lens' PutLaunchProfileMembers (Prelude.NonEmpty NewLaunchProfileMember)
putLaunchProfileMembers_members = Lens.lens (\PutLaunchProfileMembers' {members} -> members) (\s@PutLaunchProfileMembers' {} a -> s {members = a} :: PutLaunchProfileMembers) Prelude.. Lens.coerced

-- | The launch profile ID.
putLaunchProfileMembers_launchProfileId :: Lens.Lens' PutLaunchProfileMembers Prelude.Text
putLaunchProfileMembers_launchProfileId = Lens.lens (\PutLaunchProfileMembers' {launchProfileId} -> launchProfileId) (\s@PutLaunchProfileMembers' {} a -> s {launchProfileId = a} :: PutLaunchProfileMembers)

-- | The ID of the identity store.
putLaunchProfileMembers_identityStoreId :: Lens.Lens' PutLaunchProfileMembers Prelude.Text
putLaunchProfileMembers_identityStoreId = Lens.lens (\PutLaunchProfileMembers' {identityStoreId} -> identityStoreId) (\s@PutLaunchProfileMembers' {} a -> s {identityStoreId = a} :: PutLaunchProfileMembers)

instance Core.AWSRequest PutLaunchProfileMembers where
  type
    AWSResponse PutLaunchProfileMembers =
      PutLaunchProfileMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutLaunchProfileMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLaunchProfileMembers

instance Prelude.NFData PutLaunchProfileMembers

instance Core.ToHeaders PutLaunchProfileMembers where
  toHeaders PutLaunchProfileMembers' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON PutLaunchProfileMembers where
  toJSON PutLaunchProfileMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("members" Core..= members),
            Prelude.Just
              ("identityStoreId" Core..= identityStoreId)
          ]
      )

instance Core.ToPath PutLaunchProfileMembers where
  toPath PutLaunchProfileMembers' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles/",
        Core.toBS launchProfileId,
        "/membership"
      ]

instance Core.ToQuery PutLaunchProfileMembers where
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
