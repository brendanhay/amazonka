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
-- Module      : Amazonka.EMR.CreateStudioSessionMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Maps a user or group to the Amazon EMR Studio specified by @StudioId@,
-- and applies a session policy to refine Studio permissions for that user
-- or group. Use @CreateStudioSessionMapping@ to assign users to a Studio
-- when you use Amazon Web Services SSO authentication. For instructions on
-- how to assign users to a Studio when you use IAM authentication, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-studio-manage-users.html#emr-studio-assign-users-groups Assign a user or group to your EMR Studio>.
module Amazonka.EMR.CreateStudioSessionMapping
  ( -- * Creating a Request
    CreateStudioSessionMapping (..),
    newCreateStudioSessionMapping,

    -- * Request Lenses
    createStudioSessionMapping_identityName,
    createStudioSessionMapping_identityId,
    createStudioSessionMapping_studioId,
    createStudioSessionMapping_identityType,
    createStudioSessionMapping_sessionPolicyArn,

    -- * Destructuring the Response
    CreateStudioSessionMappingResponse (..),
    newCreateStudioSessionMappingResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStudioSessionMapping' smart constructor.
data CreateStudioSessionMapping = CreateStudioSessionMapping'
  { -- | The name of the user or group. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /Amazon Web Services SSO Identity Store API Reference/. Either
    -- @IdentityName@ or @IdentityId@ must be specified, but not both.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The globally unique identifier (GUID) of the user or group from the
    -- Amazon Web Services SSO Identity Store. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /Amazon Web Services SSO Identity Store API Reference/. Either
    -- @IdentityName@ or @IdentityId@ must be specified, but not both.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio to which the user or group will be
    -- mapped.
    studioId :: Prelude.Text,
    -- | Specifies whether the identity to map to the Amazon EMR Studio is a user
    -- or a group.
    identityType :: IdentityType,
    -- | The Amazon Resource Name (ARN) for the session policy that will be
    -- applied to the user or group. You should specify the ARN for the session
    -- policy that you want to apply, not the ARN of your user role. For more
    -- information, see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-studio-user-role.html Create an EMR Studio User Role with Session Policies>.
    sessionPolicyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStudioSessionMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityName', 'createStudioSessionMapping_identityName' - The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /Amazon Web Services SSO Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified, but not both.
--
-- 'identityId', 'createStudioSessionMapping_identityId' - The globally unique identifier (GUID) of the user or group from the
-- Amazon Web Services SSO Identity Store. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /Amazon Web Services SSO Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified, but not both.
--
-- 'studioId', 'createStudioSessionMapping_studioId' - The ID of the Amazon EMR Studio to which the user or group will be
-- mapped.
--
-- 'identityType', 'createStudioSessionMapping_identityType' - Specifies whether the identity to map to the Amazon EMR Studio is a user
-- or a group.
--
-- 'sessionPolicyArn', 'createStudioSessionMapping_sessionPolicyArn' - The Amazon Resource Name (ARN) for the session policy that will be
-- applied to the user or group. You should specify the ARN for the session
-- policy that you want to apply, not the ARN of your user role. For more
-- information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-studio-user-role.html Create an EMR Studio User Role with Session Policies>.
newCreateStudioSessionMapping ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'identityType'
  IdentityType ->
  -- | 'sessionPolicyArn'
  Prelude.Text ->
  CreateStudioSessionMapping
newCreateStudioSessionMapping
  pStudioId_
  pIdentityType_
  pSessionPolicyArn_ =
    CreateStudioSessionMapping'
      { identityName =
          Prelude.Nothing,
        identityId = Prelude.Nothing,
        studioId = pStudioId_,
        identityType = pIdentityType_,
        sessionPolicyArn = pSessionPolicyArn_
      }

-- | The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /Amazon Web Services SSO Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified, but not both.
createStudioSessionMapping_identityName :: Lens.Lens' CreateStudioSessionMapping (Prelude.Maybe Prelude.Text)
createStudioSessionMapping_identityName = Lens.lens (\CreateStudioSessionMapping' {identityName} -> identityName) (\s@CreateStudioSessionMapping' {} a -> s {identityName = a} :: CreateStudioSessionMapping)

-- | The globally unique identifier (GUID) of the user or group from the
-- Amazon Web Services SSO Identity Store. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /Amazon Web Services SSO Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified, but not both.
createStudioSessionMapping_identityId :: Lens.Lens' CreateStudioSessionMapping (Prelude.Maybe Prelude.Text)
createStudioSessionMapping_identityId = Lens.lens (\CreateStudioSessionMapping' {identityId} -> identityId) (\s@CreateStudioSessionMapping' {} a -> s {identityId = a} :: CreateStudioSessionMapping)

-- | The ID of the Amazon EMR Studio to which the user or group will be
-- mapped.
createStudioSessionMapping_studioId :: Lens.Lens' CreateStudioSessionMapping Prelude.Text
createStudioSessionMapping_studioId = Lens.lens (\CreateStudioSessionMapping' {studioId} -> studioId) (\s@CreateStudioSessionMapping' {} a -> s {studioId = a} :: CreateStudioSessionMapping)

-- | Specifies whether the identity to map to the Amazon EMR Studio is a user
-- or a group.
createStudioSessionMapping_identityType :: Lens.Lens' CreateStudioSessionMapping IdentityType
createStudioSessionMapping_identityType = Lens.lens (\CreateStudioSessionMapping' {identityType} -> identityType) (\s@CreateStudioSessionMapping' {} a -> s {identityType = a} :: CreateStudioSessionMapping)

-- | The Amazon Resource Name (ARN) for the session policy that will be
-- applied to the user or group. You should specify the ARN for the session
-- policy that you want to apply, not the ARN of your user role. For more
-- information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-studio-user-role.html Create an EMR Studio User Role with Session Policies>.
createStudioSessionMapping_sessionPolicyArn :: Lens.Lens' CreateStudioSessionMapping Prelude.Text
createStudioSessionMapping_sessionPolicyArn = Lens.lens (\CreateStudioSessionMapping' {sessionPolicyArn} -> sessionPolicyArn) (\s@CreateStudioSessionMapping' {} a -> s {sessionPolicyArn = a} :: CreateStudioSessionMapping)

instance Core.AWSRequest CreateStudioSessionMapping where
  type
    AWSResponse CreateStudioSessionMapping =
      CreateStudioSessionMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      CreateStudioSessionMappingResponse'

instance Prelude.Hashable CreateStudioSessionMapping where
  hashWithSalt _salt CreateStudioSessionMapping' {..} =
    _salt `Prelude.hashWithSalt` identityName
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` sessionPolicyArn

instance Prelude.NFData CreateStudioSessionMapping where
  rnf CreateStudioSessionMapping' {..} =
    Prelude.rnf identityName
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf identityType
      `Prelude.seq` Prelude.rnf sessionPolicyArn

instance Data.ToHeaders CreateStudioSessionMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.CreateStudioSessionMapping" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStudioSessionMapping where
  toJSON CreateStudioSessionMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdentityName" Data..=) Prelude.<$> identityName,
            ("IdentityId" Data..=) Prelude.<$> identityId,
            Prelude.Just ("StudioId" Data..= studioId),
            Prelude.Just ("IdentityType" Data..= identityType),
            Prelude.Just
              ("SessionPolicyArn" Data..= sessionPolicyArn)
          ]
      )

instance Data.ToPath CreateStudioSessionMapping where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStudioSessionMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStudioSessionMappingResponse' smart constructor.
data CreateStudioSessionMappingResponse = CreateStudioSessionMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStudioSessionMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateStudioSessionMappingResponse ::
  CreateStudioSessionMappingResponse
newCreateStudioSessionMappingResponse =
  CreateStudioSessionMappingResponse'

instance
  Prelude.NFData
    CreateStudioSessionMappingResponse
  where
  rnf _ = ()
