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
-- Module      : Amazonka.IdentityStore.GetGroupId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves @GroupId@ in an identity store.
module Amazonka.IdentityStore.GetGroupId
  ( -- * Creating a Request
    GetGroupId (..),
    newGetGroupId,

    -- * Request Lenses
    getGroupId_identityStoreId,
    getGroupId_alternateIdentifier,

    -- * Destructuring the Response
    GetGroupIdResponse (..),
    newGetGroupIdResponse,

    -- * Response Lenses
    getGroupIdResponse_httpStatus,
    getGroupIdResponse_groupId,
    getGroupIdResponse_identityStoreId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGroupId' smart constructor.
data GetGroupId = GetGroupId'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | A unique identifier for a user or group that is not the primary
    -- identifier. This value can be an identifier from an external identity
    -- provider (IdP) that is associated with the user, the group, or a unique
    -- attribute. For example, a unique @GroupDisplayName@.
    alternateIdentifier :: AlternateIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'getGroupId_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'alternateIdentifier', 'getGroupId_alternateIdentifier' - A unique identifier for a user or group that is not the primary
-- identifier. This value can be an identifier from an external identity
-- provider (IdP) that is associated with the user, the group, or a unique
-- attribute. For example, a unique @GroupDisplayName@.
newGetGroupId ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'alternateIdentifier'
  AlternateIdentifier ->
  GetGroupId
newGetGroupId pIdentityStoreId_ pAlternateIdentifier_ =
  GetGroupId'
    { identityStoreId = pIdentityStoreId_,
      alternateIdentifier = pAlternateIdentifier_
    }

-- | The globally unique identifier for the identity store.
getGroupId_identityStoreId :: Lens.Lens' GetGroupId Prelude.Text
getGroupId_identityStoreId = Lens.lens (\GetGroupId' {identityStoreId} -> identityStoreId) (\s@GetGroupId' {} a -> s {identityStoreId = a} :: GetGroupId)

-- | A unique identifier for a user or group that is not the primary
-- identifier. This value can be an identifier from an external identity
-- provider (IdP) that is associated with the user, the group, or a unique
-- attribute. For example, a unique @GroupDisplayName@.
getGroupId_alternateIdentifier :: Lens.Lens' GetGroupId AlternateIdentifier
getGroupId_alternateIdentifier = Lens.lens (\GetGroupId' {alternateIdentifier} -> alternateIdentifier) (\s@GetGroupId' {} a -> s {alternateIdentifier = a} :: GetGroupId)

instance Core.AWSRequest GetGroupId where
  type AWSResponse GetGroupId = GetGroupIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupIdResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "GroupId")
            Prelude.<*> (x Data..:> "IdentityStoreId")
      )

instance Prelude.Hashable GetGroupId where
  hashWithSalt _salt GetGroupId' {..} =
    _salt
      `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` alternateIdentifier

instance Prelude.NFData GetGroupId where
  rnf GetGroupId' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf alternateIdentifier

instance Data.ToHeaders GetGroupId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.GetGroupId" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetGroupId where
  toJSON GetGroupId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just
              ("AlternateIdentifier" Data..= alternateIdentifier)
          ]
      )

instance Data.ToPath GetGroupId where
  toPath = Prelude.const "/"

instance Data.ToQuery GetGroupId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupIdResponse' smart constructor.
data GetGroupIdResponse = GetGroupIdResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier for a group in the identity store.
    groupId :: Prelude.Text,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getGroupIdResponse_httpStatus' - The response's http status code.
--
-- 'groupId', 'getGroupIdResponse_groupId' - The identifier for a group in the identity store.
--
-- 'identityStoreId', 'getGroupIdResponse_identityStoreId' - The globally unique identifier for the identity store.
newGetGroupIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'identityStoreId'
  Prelude.Text ->
  GetGroupIdResponse
newGetGroupIdResponse
  pHttpStatus_
  pGroupId_
  pIdentityStoreId_ =
    GetGroupIdResponse'
      { httpStatus = pHttpStatus_,
        groupId = pGroupId_,
        identityStoreId = pIdentityStoreId_
      }

-- | The response's http status code.
getGroupIdResponse_httpStatus :: Lens.Lens' GetGroupIdResponse Prelude.Int
getGroupIdResponse_httpStatus = Lens.lens (\GetGroupIdResponse' {httpStatus} -> httpStatus) (\s@GetGroupIdResponse' {} a -> s {httpStatus = a} :: GetGroupIdResponse)

-- | The identifier for a group in the identity store.
getGroupIdResponse_groupId :: Lens.Lens' GetGroupIdResponse Prelude.Text
getGroupIdResponse_groupId = Lens.lens (\GetGroupIdResponse' {groupId} -> groupId) (\s@GetGroupIdResponse' {} a -> s {groupId = a} :: GetGroupIdResponse)

-- | The globally unique identifier for the identity store.
getGroupIdResponse_identityStoreId :: Lens.Lens' GetGroupIdResponse Prelude.Text
getGroupIdResponse_identityStoreId = Lens.lens (\GetGroupIdResponse' {identityStoreId} -> identityStoreId) (\s@GetGroupIdResponse' {} a -> s {identityStoreId = a} :: GetGroupIdResponse)

instance Prelude.NFData GetGroupIdResponse where
  rnf GetGroupIdResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf identityStoreId
