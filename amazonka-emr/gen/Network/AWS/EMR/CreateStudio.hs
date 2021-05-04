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
-- Module      : Network.AWS.EMR.CreateStudio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon EMR Studio.
module Network.AWS.EMR.CreateStudio
  ( -- * Creating a Request
    CreateStudio (..),
    newCreateStudio,

    -- * Request Lenses
    createStudio_tags,
    createStudio_description,
    createStudio_name,
    createStudio_authMode,
    createStudio_vpcId,
    createStudio_subnetIds,
    createStudio_serviceRole,
    createStudio_userRole,
    createStudio_workspaceSecurityGroupId,
    createStudio_engineSecurityGroupId,
    createStudio_defaultS3Location,

    -- * Destructuring the Response
    CreateStudioResponse (..),
    newCreateStudioResponse,

    -- * Response Lenses
    createStudioResponse_url,
    createStudioResponse_studioId,
    createStudioResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStudio' smart constructor.
data CreateStudio = CreateStudio'
  { -- | A list of tags to associate with the Amazon EMR Studio. Tags are
    -- user-defined key-value pairs that consist of a required key string with
    -- a maximum of 128 characters, and an optional value string with a maximum
    -- of 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | A detailed description of the Amazon EMR Studio.
    description :: Prelude.Maybe Prelude.Text,
    -- | A descriptive name for the Amazon EMR Studio.
    name :: Prelude.Text,
    -- | Specifies whether the Studio authenticates users using single sign-on
    -- (SSO) or IAM. Amazon EMR Studio currently only supports SSO
    -- authentication.
    authMode :: AuthMode,
    -- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate
    -- with the Studio.
    vpcId :: Prelude.Text,
    -- | A list of subnet IDs to associate with the Amazon EMR Studio. A Studio
    -- can have a maximum of 5 subnets. The subnets must belong to the VPC
    -- specified by @VpcId@. Studio users can create a Workspace in any of the
    -- specified subnets.
    subnetIds :: [Prelude.Text],
    -- | The IAM role that will be assumed by the Amazon EMR Studio. The service
    -- role provides a way for Amazon EMR Studio to interoperate with other AWS
    -- services.
    serviceRole :: Prelude.Text,
    -- | The IAM user role that will be assumed by users and groups logged in to
    -- an Amazon EMR Studio. The permissions attached to this IAM role can be
    -- scoped down for each user or group using session policies.
    userRole :: Prelude.Text,
    -- | The ID of the Amazon EMR Studio Workspace security group. The Workspace
    -- security group allows outbound network traffic to resources in the
    -- Engine security group, and it must be in the same VPC specified by
    -- @VpcId@.
    workspaceSecurityGroupId :: Prelude.Text,
    -- | The ID of the Amazon EMR Studio Engine security group. The Engine
    -- security group allows inbound network traffic from the Workspace
    -- security group, and it must be in the same VPC specified by @VpcId@.
    engineSecurityGroupId :: Prelude.Text,
    -- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces
    -- and notebook files. A Studio user can select an alternative Amazon S3
    -- location when creating a Workspace.
    defaultS3Location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createStudio_tags' - A list of tags to associate with the Amazon EMR Studio. Tags are
-- user-defined key-value pairs that consist of a required key string with
-- a maximum of 128 characters, and an optional value string with a maximum
-- of 256 characters.
--
-- 'description', 'createStudio_description' - A detailed description of the Amazon EMR Studio.
--
-- 'name', 'createStudio_name' - A descriptive name for the Amazon EMR Studio.
--
-- 'authMode', 'createStudio_authMode' - Specifies whether the Studio authenticates users using single sign-on
-- (SSO) or IAM. Amazon EMR Studio currently only supports SSO
-- authentication.
--
-- 'vpcId', 'createStudio_vpcId' - The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate
-- with the Studio.
--
-- 'subnetIds', 'createStudio_subnetIds' - A list of subnet IDs to associate with the Amazon EMR Studio. A Studio
-- can have a maximum of 5 subnets. The subnets must belong to the VPC
-- specified by @VpcId@. Studio users can create a Workspace in any of the
-- specified subnets.
--
-- 'serviceRole', 'createStudio_serviceRole' - The IAM role that will be assumed by the Amazon EMR Studio. The service
-- role provides a way for Amazon EMR Studio to interoperate with other AWS
-- services.
--
-- 'userRole', 'createStudio_userRole' - The IAM user role that will be assumed by users and groups logged in to
-- an Amazon EMR Studio. The permissions attached to this IAM role can be
-- scoped down for each user or group using session policies.
--
-- 'workspaceSecurityGroupId', 'createStudio_workspaceSecurityGroupId' - The ID of the Amazon EMR Studio Workspace security group. The Workspace
-- security group allows outbound network traffic to resources in the
-- Engine security group, and it must be in the same VPC specified by
-- @VpcId@.
--
-- 'engineSecurityGroupId', 'createStudio_engineSecurityGroupId' - The ID of the Amazon EMR Studio Engine security group. The Engine
-- security group allows inbound network traffic from the Workspace
-- security group, and it must be in the same VPC specified by @VpcId@.
--
-- 'defaultS3Location', 'createStudio_defaultS3Location' - The default Amazon S3 location to back up Amazon EMR Studio Workspaces
-- and notebook files. A Studio user can select an alternative Amazon S3
-- location when creating a Workspace.
newCreateStudio ::
  -- | 'name'
  Prelude.Text ->
  -- | 'authMode'
  AuthMode ->
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'serviceRole'
  Prelude.Text ->
  -- | 'userRole'
  Prelude.Text ->
  -- | 'workspaceSecurityGroupId'
  Prelude.Text ->
  -- | 'engineSecurityGroupId'
  Prelude.Text ->
  -- | 'defaultS3Location'
  Prelude.Text ->
  CreateStudio
newCreateStudio
  pName_
  pAuthMode_
  pVpcId_
  pServiceRole_
  pUserRole_
  pWorkspaceSecurityGroupId_
  pEngineSecurityGroupId_
  pDefaultS3Location_ =
    CreateStudio'
      { tags = Prelude.Nothing,
        description = Prelude.Nothing,
        name = pName_,
        authMode = pAuthMode_,
        vpcId = pVpcId_,
        subnetIds = Prelude.mempty,
        serviceRole = pServiceRole_,
        userRole = pUserRole_,
        workspaceSecurityGroupId =
          pWorkspaceSecurityGroupId_,
        engineSecurityGroupId = pEngineSecurityGroupId_,
        defaultS3Location = pDefaultS3Location_
      }

-- | A list of tags to associate with the Amazon EMR Studio. Tags are
-- user-defined key-value pairs that consist of a required key string with
-- a maximum of 128 characters, and an optional value string with a maximum
-- of 256 characters.
createStudio_tags :: Lens.Lens' CreateStudio (Prelude.Maybe [Tag])
createStudio_tags = Lens.lens (\CreateStudio' {tags} -> tags) (\s@CreateStudio' {} a -> s {tags = a} :: CreateStudio) Prelude.. Lens.mapping Prelude._Coerce

-- | A detailed description of the Amazon EMR Studio.
createStudio_description :: Lens.Lens' CreateStudio (Prelude.Maybe Prelude.Text)
createStudio_description = Lens.lens (\CreateStudio' {description} -> description) (\s@CreateStudio' {} a -> s {description = a} :: CreateStudio)

-- | A descriptive name for the Amazon EMR Studio.
createStudio_name :: Lens.Lens' CreateStudio Prelude.Text
createStudio_name = Lens.lens (\CreateStudio' {name} -> name) (\s@CreateStudio' {} a -> s {name = a} :: CreateStudio)

-- | Specifies whether the Studio authenticates users using single sign-on
-- (SSO) or IAM. Amazon EMR Studio currently only supports SSO
-- authentication.
createStudio_authMode :: Lens.Lens' CreateStudio AuthMode
createStudio_authMode = Lens.lens (\CreateStudio' {authMode} -> authMode) (\s@CreateStudio' {} a -> s {authMode = a} :: CreateStudio)

-- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate
-- with the Studio.
createStudio_vpcId :: Lens.Lens' CreateStudio Prelude.Text
createStudio_vpcId = Lens.lens (\CreateStudio' {vpcId} -> vpcId) (\s@CreateStudio' {} a -> s {vpcId = a} :: CreateStudio)

-- | A list of subnet IDs to associate with the Amazon EMR Studio. A Studio
-- can have a maximum of 5 subnets. The subnets must belong to the VPC
-- specified by @VpcId@. Studio users can create a Workspace in any of the
-- specified subnets.
createStudio_subnetIds :: Lens.Lens' CreateStudio [Prelude.Text]
createStudio_subnetIds = Lens.lens (\CreateStudio' {subnetIds} -> subnetIds) (\s@CreateStudio' {} a -> s {subnetIds = a} :: CreateStudio) Prelude.. Prelude._Coerce

-- | The IAM role that will be assumed by the Amazon EMR Studio. The service
-- role provides a way for Amazon EMR Studio to interoperate with other AWS
-- services.
createStudio_serviceRole :: Lens.Lens' CreateStudio Prelude.Text
createStudio_serviceRole = Lens.lens (\CreateStudio' {serviceRole} -> serviceRole) (\s@CreateStudio' {} a -> s {serviceRole = a} :: CreateStudio)

-- | The IAM user role that will be assumed by users and groups logged in to
-- an Amazon EMR Studio. The permissions attached to this IAM role can be
-- scoped down for each user or group using session policies.
createStudio_userRole :: Lens.Lens' CreateStudio Prelude.Text
createStudio_userRole = Lens.lens (\CreateStudio' {userRole} -> userRole) (\s@CreateStudio' {} a -> s {userRole = a} :: CreateStudio)

-- | The ID of the Amazon EMR Studio Workspace security group. The Workspace
-- security group allows outbound network traffic to resources in the
-- Engine security group, and it must be in the same VPC specified by
-- @VpcId@.
createStudio_workspaceSecurityGroupId :: Lens.Lens' CreateStudio Prelude.Text
createStudio_workspaceSecurityGroupId = Lens.lens (\CreateStudio' {workspaceSecurityGroupId} -> workspaceSecurityGroupId) (\s@CreateStudio' {} a -> s {workspaceSecurityGroupId = a} :: CreateStudio)

-- | The ID of the Amazon EMR Studio Engine security group. The Engine
-- security group allows inbound network traffic from the Workspace
-- security group, and it must be in the same VPC specified by @VpcId@.
createStudio_engineSecurityGroupId :: Lens.Lens' CreateStudio Prelude.Text
createStudio_engineSecurityGroupId = Lens.lens (\CreateStudio' {engineSecurityGroupId} -> engineSecurityGroupId) (\s@CreateStudio' {} a -> s {engineSecurityGroupId = a} :: CreateStudio)

-- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces
-- and notebook files. A Studio user can select an alternative Amazon S3
-- location when creating a Workspace.
createStudio_defaultS3Location :: Lens.Lens' CreateStudio Prelude.Text
createStudio_defaultS3Location = Lens.lens (\CreateStudio' {defaultS3Location} -> defaultS3Location) (\s@CreateStudio' {} a -> s {defaultS3Location = a} :: CreateStudio)

instance Prelude.AWSRequest CreateStudio where
  type Rs CreateStudio = CreateStudioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStudioResponse'
            Prelude.<$> (x Prelude..?> "Url")
            Prelude.<*> (x Prelude..?> "StudioId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStudio

instance Prelude.NFData CreateStudio

instance Prelude.ToHeaders CreateStudio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.CreateStudio" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateStudio where
  toJSON CreateStudio' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("AuthMode" Prelude..= authMode),
            Prelude.Just ("VpcId" Prelude..= vpcId),
            Prelude.Just ("SubnetIds" Prelude..= subnetIds),
            Prelude.Just ("ServiceRole" Prelude..= serviceRole),
            Prelude.Just ("UserRole" Prelude..= userRole),
            Prelude.Just
              ( "WorkspaceSecurityGroupId"
                  Prelude..= workspaceSecurityGroupId
              ),
            Prelude.Just
              ( "EngineSecurityGroupId"
                  Prelude..= engineSecurityGroupId
              ),
            Prelude.Just
              ("DefaultS3Location" Prelude..= defaultS3Location)
          ]
      )

instance Prelude.ToPath CreateStudio where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { -- | The unique Studio access URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'createStudioResponse_url' - The unique Studio access URL.
--
-- 'studioId', 'createStudioResponse_studioId' - The ID of the Amazon EMR Studio.
--
-- 'httpStatus', 'createStudioResponse_httpStatus' - The response's http status code.
newCreateStudioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStudioResponse
newCreateStudioResponse pHttpStatus_ =
  CreateStudioResponse'
    { url = Prelude.Nothing,
      studioId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Studio access URL.
createStudioResponse_url :: Lens.Lens' CreateStudioResponse (Prelude.Maybe Prelude.Text)
createStudioResponse_url = Lens.lens (\CreateStudioResponse' {url} -> url) (\s@CreateStudioResponse' {} a -> s {url = a} :: CreateStudioResponse)

-- | The ID of the Amazon EMR Studio.
createStudioResponse_studioId :: Lens.Lens' CreateStudioResponse (Prelude.Maybe Prelude.Text)
createStudioResponse_studioId = Lens.lens (\CreateStudioResponse' {studioId} -> studioId) (\s@CreateStudioResponse' {} a -> s {studioId = a} :: CreateStudioResponse)

-- | The response's http status code.
createStudioResponse_httpStatus :: Lens.Lens' CreateStudioResponse Prelude.Int
createStudioResponse_httpStatus = Lens.lens (\CreateStudioResponse' {httpStatus} -> httpStatus) (\s@CreateStudioResponse' {} a -> s {httpStatus = a} :: CreateStudioResponse)

instance Prelude.NFData CreateStudioResponse
