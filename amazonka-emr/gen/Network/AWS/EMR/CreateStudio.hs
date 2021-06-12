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

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStudio' smart constructor.
data CreateStudio = CreateStudio'
  { -- | A list of tags to associate with the Amazon EMR Studio. Tags are
    -- user-defined key-value pairs that consist of a required key string with
    -- a maximum of 128 characters, and an optional value string with a maximum
    -- of 256 characters.
    tags :: Core.Maybe [Tag],
    -- | A detailed description of the Amazon EMR Studio.
    description :: Core.Maybe Core.Text,
    -- | A descriptive name for the Amazon EMR Studio.
    name :: Core.Text,
    -- | Specifies whether the Studio authenticates users using single sign-on
    -- (SSO) or IAM. Amazon EMR Studio currently only supports SSO
    -- authentication.
    authMode :: AuthMode,
    -- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate
    -- with the Studio.
    vpcId :: Core.Text,
    -- | A list of subnet IDs to associate with the Amazon EMR Studio. A Studio
    -- can have a maximum of 5 subnets. The subnets must belong to the VPC
    -- specified by @VpcId@. Studio users can create a Workspace in any of the
    -- specified subnets.
    subnetIds :: [Core.Text],
    -- | The IAM role that will be assumed by the Amazon EMR Studio. The service
    -- role provides a way for Amazon EMR Studio to interoperate with other AWS
    -- services.
    serviceRole :: Core.Text,
    -- | The IAM user role that will be assumed by users and groups logged in to
    -- an Amazon EMR Studio. The permissions attached to this IAM role can be
    -- scoped down for each user or group using session policies.
    userRole :: Core.Text,
    -- | The ID of the Amazon EMR Studio Workspace security group. The Workspace
    -- security group allows outbound network traffic to resources in the
    -- Engine security group, and it must be in the same VPC specified by
    -- @VpcId@.
    workspaceSecurityGroupId :: Core.Text,
    -- | The ID of the Amazon EMR Studio Engine security group. The Engine
    -- security group allows inbound network traffic from the Workspace
    -- security group, and it must be in the same VPC specified by @VpcId@.
    engineSecurityGroupId :: Core.Text,
    -- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces
    -- and notebook files. A Studio user can select an alternative Amazon S3
    -- location when creating a Workspace.
    defaultS3Location :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'authMode'
  AuthMode ->
  -- | 'vpcId'
  Core.Text ->
  -- | 'serviceRole'
  Core.Text ->
  -- | 'userRole'
  Core.Text ->
  -- | 'workspaceSecurityGroupId'
  Core.Text ->
  -- | 'engineSecurityGroupId'
  Core.Text ->
  -- | 'defaultS3Location'
  Core.Text ->
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
      { tags = Core.Nothing,
        description = Core.Nothing,
        name = pName_,
        authMode = pAuthMode_,
        vpcId = pVpcId_,
        subnetIds = Core.mempty,
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
createStudio_tags :: Lens.Lens' CreateStudio (Core.Maybe [Tag])
createStudio_tags = Lens.lens (\CreateStudio' {tags} -> tags) (\s@CreateStudio' {} a -> s {tags = a} :: CreateStudio) Core.. Lens.mapping Lens._Coerce

-- | A detailed description of the Amazon EMR Studio.
createStudio_description :: Lens.Lens' CreateStudio (Core.Maybe Core.Text)
createStudio_description = Lens.lens (\CreateStudio' {description} -> description) (\s@CreateStudio' {} a -> s {description = a} :: CreateStudio)

-- | A descriptive name for the Amazon EMR Studio.
createStudio_name :: Lens.Lens' CreateStudio Core.Text
createStudio_name = Lens.lens (\CreateStudio' {name} -> name) (\s@CreateStudio' {} a -> s {name = a} :: CreateStudio)

-- | Specifies whether the Studio authenticates users using single sign-on
-- (SSO) or IAM. Amazon EMR Studio currently only supports SSO
-- authentication.
createStudio_authMode :: Lens.Lens' CreateStudio AuthMode
createStudio_authMode = Lens.lens (\CreateStudio' {authMode} -> authMode) (\s@CreateStudio' {} a -> s {authMode = a} :: CreateStudio)

-- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate
-- with the Studio.
createStudio_vpcId :: Lens.Lens' CreateStudio Core.Text
createStudio_vpcId = Lens.lens (\CreateStudio' {vpcId} -> vpcId) (\s@CreateStudio' {} a -> s {vpcId = a} :: CreateStudio)

-- | A list of subnet IDs to associate with the Amazon EMR Studio. A Studio
-- can have a maximum of 5 subnets. The subnets must belong to the VPC
-- specified by @VpcId@. Studio users can create a Workspace in any of the
-- specified subnets.
createStudio_subnetIds :: Lens.Lens' CreateStudio [Core.Text]
createStudio_subnetIds = Lens.lens (\CreateStudio' {subnetIds} -> subnetIds) (\s@CreateStudio' {} a -> s {subnetIds = a} :: CreateStudio) Core.. Lens._Coerce

-- | The IAM role that will be assumed by the Amazon EMR Studio. The service
-- role provides a way for Amazon EMR Studio to interoperate with other AWS
-- services.
createStudio_serviceRole :: Lens.Lens' CreateStudio Core.Text
createStudio_serviceRole = Lens.lens (\CreateStudio' {serviceRole} -> serviceRole) (\s@CreateStudio' {} a -> s {serviceRole = a} :: CreateStudio)

-- | The IAM user role that will be assumed by users and groups logged in to
-- an Amazon EMR Studio. The permissions attached to this IAM role can be
-- scoped down for each user or group using session policies.
createStudio_userRole :: Lens.Lens' CreateStudio Core.Text
createStudio_userRole = Lens.lens (\CreateStudio' {userRole} -> userRole) (\s@CreateStudio' {} a -> s {userRole = a} :: CreateStudio)

-- | The ID of the Amazon EMR Studio Workspace security group. The Workspace
-- security group allows outbound network traffic to resources in the
-- Engine security group, and it must be in the same VPC specified by
-- @VpcId@.
createStudio_workspaceSecurityGroupId :: Lens.Lens' CreateStudio Core.Text
createStudio_workspaceSecurityGroupId = Lens.lens (\CreateStudio' {workspaceSecurityGroupId} -> workspaceSecurityGroupId) (\s@CreateStudio' {} a -> s {workspaceSecurityGroupId = a} :: CreateStudio)

-- | The ID of the Amazon EMR Studio Engine security group. The Engine
-- security group allows inbound network traffic from the Workspace
-- security group, and it must be in the same VPC specified by @VpcId@.
createStudio_engineSecurityGroupId :: Lens.Lens' CreateStudio Core.Text
createStudio_engineSecurityGroupId = Lens.lens (\CreateStudio' {engineSecurityGroupId} -> engineSecurityGroupId) (\s@CreateStudio' {} a -> s {engineSecurityGroupId = a} :: CreateStudio)

-- | The default Amazon S3 location to back up Amazon EMR Studio Workspaces
-- and notebook files. A Studio user can select an alternative Amazon S3
-- location when creating a Workspace.
createStudio_defaultS3Location :: Lens.Lens' CreateStudio Core.Text
createStudio_defaultS3Location = Lens.lens (\CreateStudio' {defaultS3Location} -> defaultS3Location) (\s@CreateStudio' {} a -> s {defaultS3Location = a} :: CreateStudio)

instance Core.AWSRequest CreateStudio where
  type AWSResponse CreateStudio = CreateStudioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStudioResponse'
            Core.<$> (x Core..?> "Url")
            Core.<*> (x Core..?> "StudioId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateStudio

instance Core.NFData CreateStudio

instance Core.ToHeaders CreateStudio where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("ElasticMapReduce.CreateStudio" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateStudio where
  toJSON CreateStudio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("AuthMode" Core..= authMode),
            Core.Just ("VpcId" Core..= vpcId),
            Core.Just ("SubnetIds" Core..= subnetIds),
            Core.Just ("ServiceRole" Core..= serviceRole),
            Core.Just ("UserRole" Core..= userRole),
            Core.Just
              ( "WorkspaceSecurityGroupId"
                  Core..= workspaceSecurityGroupId
              ),
            Core.Just
              ( "EngineSecurityGroupId"
                  Core..= engineSecurityGroupId
              ),
            Core.Just
              ("DefaultS3Location" Core..= defaultS3Location)
          ]
      )

instance Core.ToPath CreateStudio where
  toPath = Core.const "/"

instance Core.ToQuery CreateStudio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { -- | The unique Studio access URL.
    url :: Core.Maybe Core.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateStudioResponse
newCreateStudioResponse pHttpStatus_ =
  CreateStudioResponse'
    { url = Core.Nothing,
      studioId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Studio access URL.
createStudioResponse_url :: Lens.Lens' CreateStudioResponse (Core.Maybe Core.Text)
createStudioResponse_url = Lens.lens (\CreateStudioResponse' {url} -> url) (\s@CreateStudioResponse' {} a -> s {url = a} :: CreateStudioResponse)

-- | The ID of the Amazon EMR Studio.
createStudioResponse_studioId :: Lens.Lens' CreateStudioResponse (Core.Maybe Core.Text)
createStudioResponse_studioId = Lens.lens (\CreateStudioResponse' {studioId} -> studioId) (\s@CreateStudioResponse' {} a -> s {studioId = a} :: CreateStudioResponse)

-- | The response's http status code.
createStudioResponse_httpStatus :: Lens.Lens' CreateStudioResponse Core.Int
createStudioResponse_httpStatus = Lens.lens (\CreateStudioResponse' {httpStatus} -> httpStatus) (\s@CreateStudioResponse' {} a -> s {httpStatus = a} :: CreateStudioResponse)

instance Core.NFData CreateStudioResponse
