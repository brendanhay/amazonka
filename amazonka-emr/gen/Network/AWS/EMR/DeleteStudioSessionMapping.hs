{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.DeleteStudioSessionMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user or group from an Amazon EMR Studio.
module Network.AWS.EMR.DeleteStudioSessionMapping
  ( -- * Creating a Request
    DeleteStudioSessionMapping (..),
    newDeleteStudioSessionMapping,

    -- * Request Lenses
    deleteStudioSessionMapping_identityName,
    deleteStudioSessionMapping_identityId,
    deleteStudioSessionMapping_studioId,
    deleteStudioSessionMapping_identityType,

    -- * Destructuring the Response
    DeleteStudioSessionMappingResponse (..),
    newDeleteStudioSessionMappingResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStudioSessionMapping' smart constructor.
data DeleteStudioSessionMapping = DeleteStudioSessionMapping'
  { -- | The name of the user name or group to remove from the Amazon EMR Studio.
    -- For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The globally unique identifier (GUID) of the user or group to remove
    -- from the Amazon EMR Studio. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Text,
    -- | Specifies whether the identity to delete from the Amazon EMR Studio is a
    -- user or a group.
    identityType :: IdentityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioSessionMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityName', 'deleteStudioSessionMapping_identityName' - The name of the user name or group to remove from the Amazon EMR Studio.
-- For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
--
-- 'identityId', 'deleteStudioSessionMapping_identityId' - The globally unique identifier (GUID) of the user or group to remove
-- from the Amazon EMR Studio. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
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
      { identityName =
          Prelude.Nothing,
        identityId = Prelude.Nothing,
        studioId = pStudioId_,
        identityType = pIdentityType_
      }

-- | The name of the user name or group to remove from the Amazon EMR Studio.
-- For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
deleteStudioSessionMapping_identityName :: Lens.Lens' DeleteStudioSessionMapping (Prelude.Maybe Prelude.Text)
deleteStudioSessionMapping_identityName = Lens.lens (\DeleteStudioSessionMapping' {identityName} -> identityName) (\s@DeleteStudioSessionMapping' {} a -> s {identityName = a} :: DeleteStudioSessionMapping)

-- | The globally unique identifier (GUID) of the user or group to remove
-- from the Amazon EMR Studio. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
deleteStudioSessionMapping_identityId :: Lens.Lens' DeleteStudioSessionMapping (Prelude.Maybe Prelude.Text)
deleteStudioSessionMapping_identityId = Lens.lens (\DeleteStudioSessionMapping' {identityId} -> identityId) (\s@DeleteStudioSessionMapping' {} a -> s {identityId = a} :: DeleteStudioSessionMapping)

-- | The ID of the Amazon EMR Studio.
deleteStudioSessionMapping_studioId :: Lens.Lens' DeleteStudioSessionMapping Prelude.Text
deleteStudioSessionMapping_studioId = Lens.lens (\DeleteStudioSessionMapping' {studioId} -> studioId) (\s@DeleteStudioSessionMapping' {} a -> s {studioId = a} :: DeleteStudioSessionMapping)

-- | Specifies whether the identity to delete from the Amazon EMR Studio is a
-- user or a group.
deleteStudioSessionMapping_identityType :: Lens.Lens' DeleteStudioSessionMapping IdentityType
deleteStudioSessionMapping_identityType = Lens.lens (\DeleteStudioSessionMapping' {identityType} -> identityType) (\s@DeleteStudioSessionMapping' {} a -> s {identityType = a} :: DeleteStudioSessionMapping)

instance
  Prelude.AWSRequest
    DeleteStudioSessionMapping
  where
  type
    Rs DeleteStudioSessionMapping =
      DeleteStudioSessionMappingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteStudioSessionMappingResponse'

instance Prelude.Hashable DeleteStudioSessionMapping

instance Prelude.NFData DeleteStudioSessionMapping

instance Prelude.ToHeaders DeleteStudioSessionMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.DeleteStudioSessionMapping" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteStudioSessionMapping where
  toJSON DeleteStudioSessionMapping' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IdentityName" Prelude..=)
              Prelude.<$> identityName,
            ("IdentityId" Prelude..=) Prelude.<$> identityId,
            Prelude.Just ("StudioId" Prelude..= studioId),
            Prelude.Just
              ("IdentityType" Prelude..= identityType)
          ]
      )

instance Prelude.ToPath DeleteStudioSessionMapping where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteStudioSessionMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioSessionMappingResponse' smart constructor.
data DeleteStudioSessionMappingResponse = DeleteStudioSessionMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
