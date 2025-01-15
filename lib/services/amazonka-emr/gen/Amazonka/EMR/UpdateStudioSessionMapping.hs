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
-- Module      : Amazonka.EMR.UpdateStudioSessionMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the session policy attached to the user or group for the
-- specified Amazon EMR Studio.
module Amazonka.EMR.UpdateStudioSessionMapping
  ( -- * Creating a Request
    UpdateStudioSessionMapping (..),
    newUpdateStudioSessionMapping,

    -- * Request Lenses
    updateStudioSessionMapping_identityId,
    updateStudioSessionMapping_identityName,
    updateStudioSessionMapping_studioId,
    updateStudioSessionMapping_identityType,
    updateStudioSessionMapping_sessionPolicyArn,

    -- * Destructuring the Response
    UpdateStudioSessionMappingResponse (..),
    newUpdateStudioSessionMappingResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStudioSessionMapping' smart constructor.
data UpdateStudioSessionMapping = UpdateStudioSessionMapping'
  { -- | The globally unique identifier (GUID) of the user or group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /IAM Identity Center Identity Store API Reference/. Either
    -- @IdentityName@ or @IdentityId@ must be specified.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The name of the user or group to update. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /IAM Identity Center Identity Store API Reference/. Either
    -- @IdentityName@ or @IdentityId@ must be specified.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Text,
    -- | Specifies whether the identity to update is a user or a group.
    identityType :: IdentityType,
    -- | The Amazon Resource Name (ARN) of the session policy to associate with
    -- the specified user or group.
    sessionPolicyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioSessionMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'updateStudioSessionMapping_identityId' - The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /IAM Identity Center Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
--
-- 'identityName', 'updateStudioSessionMapping_identityName' - The name of the user or group to update. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /IAM Identity Center Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
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
      { identityId =
          Prelude.Nothing,
        identityName = Prelude.Nothing,
        studioId = pStudioId_,
        identityType = pIdentityType_,
        sessionPolicyArn = pSessionPolicyArn_
      }

-- | The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /IAM Identity Center Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
updateStudioSessionMapping_identityId :: Lens.Lens' UpdateStudioSessionMapping (Prelude.Maybe Prelude.Text)
updateStudioSessionMapping_identityId = Lens.lens (\UpdateStudioSessionMapping' {identityId} -> identityId) (\s@UpdateStudioSessionMapping' {} a -> s {identityId = a} :: UpdateStudioSessionMapping)

-- | The name of the user or group to update. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /IAM Identity Center Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
updateStudioSessionMapping_identityName :: Lens.Lens' UpdateStudioSessionMapping (Prelude.Maybe Prelude.Text)
updateStudioSessionMapping_identityName = Lens.lens (\UpdateStudioSessionMapping' {identityName} -> identityName) (\s@UpdateStudioSessionMapping' {} a -> s {identityName = a} :: UpdateStudioSessionMapping)

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

instance Core.AWSRequest UpdateStudioSessionMapping where
  type
    AWSResponse UpdateStudioSessionMapping =
      UpdateStudioSessionMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateStudioSessionMappingResponse'

instance Prelude.Hashable UpdateStudioSessionMapping where
  hashWithSalt _salt UpdateStudioSessionMapping' {..} =
    _salt
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` identityName
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` sessionPolicyArn

instance Prelude.NFData UpdateStudioSessionMapping where
  rnf UpdateStudioSessionMapping' {..} =
    Prelude.rnf identityId `Prelude.seq`
      Prelude.rnf identityName `Prelude.seq`
        Prelude.rnf studioId `Prelude.seq`
          Prelude.rnf identityType `Prelude.seq`
            Prelude.rnf sessionPolicyArn

instance Data.ToHeaders UpdateStudioSessionMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.UpdateStudioSessionMapping" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStudioSessionMapping where
  toJSON UpdateStudioSessionMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdentityId" Data..=) Prelude.<$> identityId,
            ("IdentityName" Data..=) Prelude.<$> identityName,
            Prelude.Just ("StudioId" Data..= studioId),
            Prelude.Just ("IdentityType" Data..= identityType),
            Prelude.Just
              ("SessionPolicyArn" Data..= sessionPolicyArn)
          ]
      )

instance Data.ToPath UpdateStudioSessionMapping where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStudioSessionMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStudioSessionMappingResponse' smart constructor.
data UpdateStudioSessionMappingResponse = UpdateStudioSessionMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
