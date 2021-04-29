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
-- Module      : Network.AWS.EMR.CreateStudioSessionMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Maps a user or group to the Amazon EMR Studio specified by @StudioId@,
-- and applies a session policy to refine Studio permissions for that user
-- or group.
module Network.AWS.EMR.CreateStudioSessionMapping
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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStudioSessionMapping' smart constructor.
data CreateStudioSessionMapping = CreateStudioSessionMapping'
  { -- | The name of the user or group. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The globally unique identifier (GUID) of the user or group from the AWS
    -- SSO Identity Store. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio to which the user or group will be
    -- mapped.
    studioId :: Prelude.Text,
    -- | Specifies whether the identity to map to the Amazon EMR Studio is a user
    -- or a group.
    identityType :: IdentityType,
    -- | The Amazon Resource Name (ARN) for the session policy that will be
    -- applied to the user or group. Session policies refine Studio user
    -- permissions without the need to use multiple IAM user roles.
    sessionPolicyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
--
-- 'identityId', 'createStudioSessionMapping_identityId' - The globally unique identifier (GUID) of the user or group from the AWS
-- SSO Identity Store. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
--
-- 'studioId', 'createStudioSessionMapping_studioId' - The ID of the Amazon EMR Studio to which the user or group will be
-- mapped.
--
-- 'identityType', 'createStudioSessionMapping_identityType' - Specifies whether the identity to map to the Amazon EMR Studio is a user
-- or a group.
--
-- 'sessionPolicyArn', 'createStudioSessionMapping_sessionPolicyArn' - The Amazon Resource Name (ARN) for the session policy that will be
-- applied to the user or group. Session policies refine Studio user
-- permissions without the need to use multiple IAM user roles.
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
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
createStudioSessionMapping_identityName :: Lens.Lens' CreateStudioSessionMapping (Prelude.Maybe Prelude.Text)
createStudioSessionMapping_identityName = Lens.lens (\CreateStudioSessionMapping' {identityName} -> identityName) (\s@CreateStudioSessionMapping' {} a -> s {identityName = a} :: CreateStudioSessionMapping)

-- | The globally unique identifier (GUID) of the user or group from the AWS
-- SSO Identity Store. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
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
-- applied to the user or group. Session policies refine Studio user
-- permissions without the need to use multiple IAM user roles.
createStudioSessionMapping_sessionPolicyArn :: Lens.Lens' CreateStudioSessionMapping Prelude.Text
createStudioSessionMapping_sessionPolicyArn = Lens.lens (\CreateStudioSessionMapping' {sessionPolicyArn} -> sessionPolicyArn) (\s@CreateStudioSessionMapping' {} a -> s {sessionPolicyArn = a} :: CreateStudioSessionMapping)

instance
  Prelude.AWSRequest
    CreateStudioSessionMapping
  where
  type
    Rs CreateStudioSessionMapping =
      CreateStudioSessionMappingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      CreateStudioSessionMappingResponse'

instance Prelude.Hashable CreateStudioSessionMapping

instance Prelude.NFData CreateStudioSessionMapping

instance Prelude.ToHeaders CreateStudioSessionMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.CreateStudioSessionMapping" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateStudioSessionMapping where
  toJSON CreateStudioSessionMapping' {..} =
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

instance Prelude.ToPath CreateStudioSessionMapping where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateStudioSessionMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStudioSessionMappingResponse' smart constructor.
data CreateStudioSessionMappingResponse = CreateStudioSessionMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
