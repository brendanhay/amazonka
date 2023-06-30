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
-- Module      : Amazonka.IdentityStore.GetUserId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @UserId@ in an identity store.
module Amazonka.IdentityStore.GetUserId
  ( -- * Creating a Request
    GetUserId (..),
    newGetUserId,

    -- * Request Lenses
    getUserId_identityStoreId,
    getUserId_alternateIdentifier,

    -- * Destructuring the Response
    GetUserIdResponse (..),
    newGetUserIdResponse,

    -- * Response Lenses
    getUserIdResponse_httpStatus,
    getUserIdResponse_userId,
    getUserIdResponse_identityStoreId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUserId' smart constructor.
data GetUserId = GetUserId'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | A unique identifier for a user or group that is not the primary
    -- identifier. This value can be an identifier from an external identity
    -- provider (IdP) that is associated with the user, the group, or a unique
    -- attribute. For example, a unique @UserDisplayName@.
    alternateIdentifier :: AlternateIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'getUserId_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'alternateIdentifier', 'getUserId_alternateIdentifier' - A unique identifier for a user or group that is not the primary
-- identifier. This value can be an identifier from an external identity
-- provider (IdP) that is associated with the user, the group, or a unique
-- attribute. For example, a unique @UserDisplayName@.
newGetUserId ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'alternateIdentifier'
  AlternateIdentifier ->
  GetUserId
newGetUserId pIdentityStoreId_ pAlternateIdentifier_ =
  GetUserId'
    { identityStoreId = pIdentityStoreId_,
      alternateIdentifier = pAlternateIdentifier_
    }

-- | The globally unique identifier for the identity store.
getUserId_identityStoreId :: Lens.Lens' GetUserId Prelude.Text
getUserId_identityStoreId = Lens.lens (\GetUserId' {identityStoreId} -> identityStoreId) (\s@GetUserId' {} a -> s {identityStoreId = a} :: GetUserId)

-- | A unique identifier for a user or group that is not the primary
-- identifier. This value can be an identifier from an external identity
-- provider (IdP) that is associated with the user, the group, or a unique
-- attribute. For example, a unique @UserDisplayName@.
getUserId_alternateIdentifier :: Lens.Lens' GetUserId AlternateIdentifier
getUserId_alternateIdentifier = Lens.lens (\GetUserId' {alternateIdentifier} -> alternateIdentifier) (\s@GetUserId' {} a -> s {alternateIdentifier = a} :: GetUserId)

instance Core.AWSRequest GetUserId where
  type AWSResponse GetUserId = GetUserIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserIdResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UserId")
            Prelude.<*> (x Data..:> "IdentityStoreId")
      )

instance Prelude.Hashable GetUserId where
  hashWithSalt _salt GetUserId' {..} =
    _salt
      `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` alternateIdentifier

instance Prelude.NFData GetUserId where
  rnf GetUserId' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf alternateIdentifier

instance Data.ToHeaders GetUserId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSIdentityStore.GetUserId" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUserId where
  toJSON GetUserId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just
              ("AlternateIdentifier" Data..= alternateIdentifier)
          ]
      )

instance Data.ToPath GetUserId where
  toPath = Prelude.const "/"

instance Data.ToQuery GetUserId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUserIdResponse' smart constructor.
data GetUserIdResponse = GetUserIdResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier for a user in the identity store.
    userId :: Prelude.Text,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getUserIdResponse_httpStatus' - The response's http status code.
--
-- 'userId', 'getUserIdResponse_userId' - The identifier for a user in the identity store.
--
-- 'identityStoreId', 'getUserIdResponse_identityStoreId' - The globally unique identifier for the identity store.
newGetUserIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'identityStoreId'
  Prelude.Text ->
  GetUserIdResponse
newGetUserIdResponse
  pHttpStatus_
  pUserId_
  pIdentityStoreId_ =
    GetUserIdResponse'
      { httpStatus = pHttpStatus_,
        userId = pUserId_,
        identityStoreId = pIdentityStoreId_
      }

-- | The response's http status code.
getUserIdResponse_httpStatus :: Lens.Lens' GetUserIdResponse Prelude.Int
getUserIdResponse_httpStatus = Lens.lens (\GetUserIdResponse' {httpStatus} -> httpStatus) (\s@GetUserIdResponse' {} a -> s {httpStatus = a} :: GetUserIdResponse)

-- | The identifier for a user in the identity store.
getUserIdResponse_userId :: Lens.Lens' GetUserIdResponse Prelude.Text
getUserIdResponse_userId = Lens.lens (\GetUserIdResponse' {userId} -> userId) (\s@GetUserIdResponse' {} a -> s {userId = a} :: GetUserIdResponse)

-- | The globally unique identifier for the identity store.
getUserIdResponse_identityStoreId :: Lens.Lens' GetUserIdResponse Prelude.Text
getUserIdResponse_identityStoreId = Lens.lens (\GetUserIdResponse' {identityStoreId} -> identityStoreId) (\s@GetUserIdResponse' {} a -> s {identityStoreId = a} :: GetUserIdResponse)

instance Prelude.NFData GetUserIdResponse where
  rnf GetUserIdResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf identityStoreId
