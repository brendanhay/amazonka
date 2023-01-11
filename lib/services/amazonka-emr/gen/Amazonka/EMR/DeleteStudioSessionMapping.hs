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
-- Module      : Amazonka.EMR.DeleteStudioSessionMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user or group from an Amazon EMR Studio.
module Amazonka.EMR.DeleteStudioSessionMapping
  ( -- * Creating a Request
    DeleteStudioSessionMapping (..),
    newDeleteStudioSessionMapping,

    -- * Request Lenses
    deleteStudioSessionMapping_identityId,
    deleteStudioSessionMapping_identityName,
    deleteStudioSessionMapping_studioId,
    deleteStudioSessionMapping_identityType,

    -- * Destructuring the Response
    DeleteStudioSessionMappingResponse (..),
    newDeleteStudioSessionMappingResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStudioSessionMapping' smart constructor.
data DeleteStudioSessionMapping = DeleteStudioSessionMapping'
  { -- | The globally unique identifier (GUID) of the user or group to remove
    -- from the Amazon EMR Studio. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /IAM Identity Center Identity Store API Reference/. Either
    -- @IdentityName@ or @IdentityId@ must be specified.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The name of the user name or group to remove from the Amazon EMR Studio.
    -- For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /IAM Identity Center Store API Reference/. Either @IdentityName@
    -- or @IdentityId@ must be specified.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Text,
    -- | Specifies whether the identity to delete from the Amazon EMR Studio is a
    -- user or a group.
    identityType :: IdentityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioSessionMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'deleteStudioSessionMapping_identityId' - The globally unique identifier (GUID) of the user or group to remove
-- from the Amazon EMR Studio. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /IAM Identity Center Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
--
-- 'identityName', 'deleteStudioSessionMapping_identityName' - The name of the user name or group to remove from the Amazon EMR Studio.
-- For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /IAM Identity Center Store API Reference/. Either @IdentityName@
-- or @IdentityId@ must be specified.
--
-- 'studioId', 'deleteStudioSessionMapping_studioId' - The ID of the Amazon EMR Studio.
--
-- 'identityType', 'deleteStudioSessionMapping_identityType' - Specifies whether the identity to delete from the Amazon EMR Studio is a
-- user or a group.
newDeleteStudioSessionMapping ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'identityType'
  IdentityType ->
  DeleteStudioSessionMapping
newDeleteStudioSessionMapping
  pStudioId_
  pIdentityType_ =
    DeleteStudioSessionMapping'
      { identityId =
          Prelude.Nothing,
        identityName = Prelude.Nothing,
        studioId = pStudioId_,
        identityType = pIdentityType_
      }

-- | The globally unique identifier (GUID) of the user or group to remove
-- from the Amazon EMR Studio. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /IAM Identity Center Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
deleteStudioSessionMapping_identityId :: Lens.Lens' DeleteStudioSessionMapping (Prelude.Maybe Prelude.Text)
deleteStudioSessionMapping_identityId = Lens.lens (\DeleteStudioSessionMapping' {identityId} -> identityId) (\s@DeleteStudioSessionMapping' {} a -> s {identityId = a} :: DeleteStudioSessionMapping)

-- | The name of the user name or group to remove from the Amazon EMR Studio.
-- For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /IAM Identity Center Store API Reference/. Either @IdentityName@
-- or @IdentityId@ must be specified.
deleteStudioSessionMapping_identityName :: Lens.Lens' DeleteStudioSessionMapping (Prelude.Maybe Prelude.Text)
deleteStudioSessionMapping_identityName = Lens.lens (\DeleteStudioSessionMapping' {identityName} -> identityName) (\s@DeleteStudioSessionMapping' {} a -> s {identityName = a} :: DeleteStudioSessionMapping)

-- | The ID of the Amazon EMR Studio.
deleteStudioSessionMapping_studioId :: Lens.Lens' DeleteStudioSessionMapping Prelude.Text
deleteStudioSessionMapping_studioId = Lens.lens (\DeleteStudioSessionMapping' {studioId} -> studioId) (\s@DeleteStudioSessionMapping' {} a -> s {studioId = a} :: DeleteStudioSessionMapping)

-- | Specifies whether the identity to delete from the Amazon EMR Studio is a
-- user or a group.
deleteStudioSessionMapping_identityType :: Lens.Lens' DeleteStudioSessionMapping IdentityType
deleteStudioSessionMapping_identityType = Lens.lens (\DeleteStudioSessionMapping' {identityType} -> identityType) (\s@DeleteStudioSessionMapping' {} a -> s {identityType = a} :: DeleteStudioSessionMapping)

instance Core.AWSRequest DeleteStudioSessionMapping where
  type
    AWSResponse DeleteStudioSessionMapping =
      DeleteStudioSessionMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteStudioSessionMappingResponse'

instance Prelude.Hashable DeleteStudioSessionMapping where
  hashWithSalt _salt DeleteStudioSessionMapping' {..} =
    _salt `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` identityName
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` identityType

instance Prelude.NFData DeleteStudioSessionMapping where
  rnf DeleteStudioSessionMapping' {..} =
    Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf identityName
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf identityType

instance Data.ToHeaders DeleteStudioSessionMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.DeleteStudioSessionMapping" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStudioSessionMapping where
  toJSON DeleteStudioSessionMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdentityId" Data..=) Prelude.<$> identityId,
            ("IdentityName" Data..=) Prelude.<$> identityName,
            Prelude.Just ("StudioId" Data..= studioId),
            Prelude.Just ("IdentityType" Data..= identityType)
          ]
      )

instance Data.ToPath DeleteStudioSessionMapping where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteStudioSessionMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioSessionMappingResponse' smart constructor.
data DeleteStudioSessionMappingResponse = DeleteStudioSessionMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioSessionMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStudioSessionMappingResponse ::
  DeleteStudioSessionMappingResponse
newDeleteStudioSessionMappingResponse =
  DeleteStudioSessionMappingResponse'

instance
  Prelude.NFData
    DeleteStudioSessionMappingResponse
  where
  rnf _ = ()
