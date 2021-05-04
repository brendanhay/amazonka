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
-- Module      : Network.AWS.EMR.UpdateStudioSessionMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the session policy attached to the user or group for the
-- specified Amazon EMR Studio.
module Network.AWS.EMR.UpdateStudioSessionMapping
  ( -- * Creating a Request
    UpdateStudioSessionMapping (..),
    newUpdateStudioSessionMapping,

    -- * Request Lenses
    updateStudioSessionMapping_identityName,
    updateStudioSessionMapping_identityId,
    updateStudioSessionMapping_studioId,
    updateStudioSessionMapping_identityType,
    updateStudioSessionMapping_sessionPolicyArn,

    -- * Destructuring the Response
    UpdateStudioSessionMappingResponse (..),
    newUpdateStudioSessionMappingResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateStudioSessionMapping' smart constructor.
data UpdateStudioSessionMapping = UpdateStudioSessionMapping'
  { -- | The name of the user or group to update. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The globally unique identifier (GUID) of the user or group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Text,
    -- | Specifies whether the identity to update is a user or a group.
    identityType :: IdentityType,
    -- | The Amazon Resource Name (ARN) of the session policy to associate with
    -- the specified user or group.
    sessionPolicyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioSessionMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityName', 'updateStudioSessionMapping_identityName' - The name of the user or group to update. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
--
-- 'identityId', 'updateStudioSessionMapping_identityId' - The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
--
-- 'studioId', 'updateStudioSessionMapping_studioId' - The ID of the Amazon EMR Studio.
--
-- 'identityType', 'updateStudioSessionMapping_identityType' - Specifies whether the identity to update is a user or a group.
--
-- 'sessionPolicyArn', 'updateStudioSessionMapping_sessionPolicyArn' - The Amazon Resource Name (ARN) of the session policy to associate with
-- the specified user or group.
newUpdateStudioSessionMapping ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'identityType'
  IdentityType ->
  -- | 'sessionPolicyArn'
  Prelude.Text ->
  UpdateStudioSessionMapping
newUpdateStudioSessionMapping
  pStudioId_
  pIdentityType_
  pSessionPolicyArn_ =
    UpdateStudioSessionMapping'
      { identityName =
          Prelude.Nothing,
        identityId = Prelude.Nothing,
        studioId = pStudioId_,
        identityType = pIdentityType_,
        sessionPolicyArn = pSessionPolicyArn_
      }

-- | The name of the user or group to update. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
updateStudioSessionMapping_identityName :: Lens.Lens' UpdateStudioSessionMapping (Prelude.Maybe Prelude.Text)
updateStudioSessionMapping_identityName = Lens.lens (\UpdateStudioSessionMapping' {identityName} -> identityName) (\s@UpdateStudioSessionMapping' {} a -> s {identityName = a} :: UpdateStudioSessionMapping)

-- | The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
updateStudioSessionMapping_identityId :: Lens.Lens' UpdateStudioSessionMapping (Prelude.Maybe Prelude.Text)
updateStudioSessionMapping_identityId = Lens.lens (\UpdateStudioSessionMapping' {identityId} -> identityId) (\s@UpdateStudioSessionMapping' {} a -> s {identityId = a} :: UpdateStudioSessionMapping)

-- | The ID of the Amazon EMR Studio.
updateStudioSessionMapping_studioId :: Lens.Lens' UpdateStudioSessionMapping Prelude.Text
updateStudioSessionMapping_studioId = Lens.lens (\UpdateStudioSessionMapping' {studioId} -> studioId) (\s@UpdateStudioSessionMapping' {} a -> s {studioId = a} :: UpdateStudioSessionMapping)

-- | Specifies whether the identity to update is a user or a group.
updateStudioSessionMapping_identityType :: Lens.Lens' UpdateStudioSessionMapping IdentityType
updateStudioSessionMapping_identityType = Lens.lens (\UpdateStudioSessionMapping' {identityType} -> identityType) (\s@UpdateStudioSessionMapping' {} a -> s {identityType = a} :: UpdateStudioSessionMapping)

-- | The Amazon Resource Name (ARN) of the session policy to associate with
-- the specified user or group.
updateStudioSessionMapping_sessionPolicyArn :: Lens.Lens' UpdateStudioSessionMapping Prelude.Text
updateStudioSessionMapping_sessionPolicyArn = Lens.lens (\UpdateStudioSessionMapping' {sessionPolicyArn} -> sessionPolicyArn) (\s@UpdateStudioSessionMapping' {} a -> s {sessionPolicyArn = a} :: UpdateStudioSessionMapping)

instance
  Prelude.AWSRequest
    UpdateStudioSessionMapping
  where
  type
    Rs UpdateStudioSessionMapping =
      UpdateStudioSessionMappingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateStudioSessionMappingResponse'

instance Prelude.Hashable UpdateStudioSessionMapping

instance Prelude.NFData UpdateStudioSessionMapping

instance Prelude.ToHeaders UpdateStudioSessionMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.UpdateStudioSessionMapping" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateStudioSessionMapping where
  toJSON UpdateStudioSessionMapping' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IdentityName" Prelude..=)
              Prelude.<$> identityName,
            ("IdentityId" Prelude..=) Prelude.<$> identityId,
            Prelude.Just ("StudioId" Prelude..= studioId),
            Prelude.Just
              ("IdentityType" Prelude..= identityType),
            Prelude.Just
              ("SessionPolicyArn" Prelude..= sessionPolicyArn)
          ]
      )

instance Prelude.ToPath UpdateStudioSessionMapping where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateStudioSessionMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStudioSessionMappingResponse' smart constructor.
data UpdateStudioSessionMappingResponse = UpdateStudioSessionMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioSessionMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateStudioSessionMappingResponse ::
  UpdateStudioSessionMappingResponse
newUpdateStudioSessionMappingResponse =
  UpdateStudioSessionMappingResponse'

instance
  Prelude.NFData
    UpdateStudioSessionMappingResponse
