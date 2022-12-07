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
-- Module      : Amazonka.EMR.CreateStudio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon EMR Studio.
module Amazonka.EMR.CreateStudio
  ( -- * Creating a Request
    CreateStudio (..),
    newCreateStudio,

    -- * Request Lenses
    createStudio_tags,
    createStudio_idpRelayStateParameterName,
    createStudio_idpAuthUrl,
    createStudio_description,
    createStudio_userRole,
    createStudio_name,
    createStudio_authMode,
    createStudio_vpcId,
    createStudio_subnetIds,
    createStudio_serviceRole,
    createStudio_workspaceSecurityGroupId,
    createStudio_engineSecurityGroupId,
    createStudio_defaultS3Location,

    -- * Destructuring the Response
    CreateStudioResponse (..),
    newCreateStudioResponse,

    -- * Response Lenses
    createStudioResponse_studioId,
    createStudioResponse_url,
    createStudioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStudio' smart constructor.
data CreateStudio = CreateStudio'
  { -- | A list of tags to associate with the Amazon EMR Studio. Tags are
    -- user-defined key-value pairs that consist of a required key string with
    -- a maximum of 128 characters, and an optional value string with a maximum
    -- of 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | The name that your identity provider (IdP) uses for its @RelayState@
    -- parameter. For example, @RelayState@ or @TargetSource@. Specify this
    -- value when you use IAM authentication and want to let federated users
    -- log in to a Studio using the Studio URL. The @RelayState@ parameter
    -- differs by IdP.
    idpRelayStateParameterName :: Prelude.Maybe Prelude.Text,
    -- | The authentication endpoint of your identity provider (IdP). Specify
    -- this value when you use IAM authentication and want to let federated
    -- users log in to a Studio with the Studio URL and credentials from your
    -- IdP. Amazon EMR Studio redirects users to this endpoint to enter
    -- credentials.
    idpAuthUrl :: Prelude.Maybe Prelude.Text,
    -- | A detailed description of the Amazon EMR Studio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The IAM user role that users and groups assume when logged in to an
    -- Amazon EMR Studio. Only specify a @UserRole@ when you use Amazon Web
    -- Services SSO authentication. The permissions attached to the @UserRole@
    -- can be scoped down for each user or group using session policies.
    userRole :: Prelude.Maybe Prelude.Text,
    -- | A descriptive name for the Amazon EMR Studio.
    name :: Prelude.Text,
    -- | Specifies whether the Studio authenticates users using IAM or Amazon Web
    -- Services SSO.
    authMode :: AuthMode,
    -- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate
    -- with the Studio.
    vpcId :: Prelude.Text,
    -- | A list of subnet IDs to associate with the Amazon EMR Studio. A Studio
    -- can have a maximum of 5 subnets. The subnets must belong to the VPC
    -- specified by @VpcId@. Studio users can create a Workspace in any of the
    -- specified subnets.
    subnetIds :: [Prelude.Text],
    -- | The IAM role that the Amazon EMR Studio assumes. The service role
    -- provides a way for Amazon EMR Studio to interoperate with other Amazon
    -- Web Services services.
    serviceRole :: Prelude.Text,
    -- | The ID of the Amazon EMR Studio Workspace security group. The Workspace
    -- security group allows outbound network traffic to resources in the
    -- Engine security group, and it must be in the same VPC specified by
    -- @VpcId@.
    workspaceSecurityGroupId :: Prelude.Text,
    -- | The ID of the Amazon EMR Studio Engine security group. The Engine
    -- security group allows inbound network traffic from the Workspace
    -- security group, and it must be in the same VPC specified by @VpcId@.
    engineSecurityGroupId :: Prelude.Text,
    -- | The Amazon S3 location to back up Amazon EMR Studio Workspaces and
    -- notebook files.
    defaultS3Location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'idpRelayStateParameterName', 'createStudio_idpRelayStateParameterName' - The name that your identity provider (IdP) uses for its @RelayState@
-- parameter. For example, @RelayState@ or @TargetSource@. Specify this
-- value when you use IAM authentication and want to let federated users
-- log in to a Studio using the Studio URL. The @RelayState@ parameter
-- differs by IdP.
--
-- 'idpAuthUrl', 'createStudio_idpAuthUrl' - The authentication endpoint of your identity provider (IdP). Specify
-- this value when you use IAM authentication and want to let federated
-- users log in to a Studio with the Studio URL and credentials from your
-- IdP. Amazon EMR Studio redirects users to this endpoint to enter
-- credentials.
--
-- 'description', 'createStudio_description' - A detailed description of the Amazon EMR Studio.
--
-- 'userRole', 'createStudio_userRole' - The IAM user role that users and groups assume when logged in to an
-- Amazon EMR Studio. Only specify a @UserRole@ when you use Amazon Web
-- Services SSO authentication. The permissions attached to the @UserRole@
-- can be scoped down for each user or group using session policies.
--
-- 'name', 'createStudio_name' - A descriptive name for the Amazon EMR Studio.
--
-- 'authMode', 'createStudio_authMode' - Specifies whether the Studio authenticates users using IAM or Amazon Web
-- Services SSO.
--
-- 'vpcId', 'createStudio_vpcId' - The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate
-- with the Studio.
--
-- 'subnetIds', 'createStudio_subnetIds' - A list of subnet IDs to associate with the Amazon EMR Studio. A Studio
-- can have a maximum of 5 subnets. The subnets must belong to the VPC
-- specified by @VpcId@. Studio users can create a Workspace in any of the
-- specified subnets.
--
-- 'serviceRole', 'createStudio_serviceRole' - The IAM role that the Amazon EMR Studio assumes. The service role
-- provides a way for Amazon EMR Studio to interoperate with other Amazon
-- Web Services services.
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
-- 'defaultS3Location', 'createStudio_defaultS3Location' - The Amazon S3 location to back up Amazon EMR Studio Workspaces and
-- notebook files.
newCreateStudio ::
  -- | 'name'
  Prelude.Text ->
  -- | 'authMode'
  AuthMode ->
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'serviceRole'
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
  pWorkspaceSecurityGroupId_
  pEngineSecurityGroupId_
  pDefaultS3Location_ =
    CreateStudio'
      { tags = Prelude.Nothing,
        idpRelayStateParameterName = Prelude.Nothing,
        idpAuthUrl = Prelude.Nothing,
        description = Prelude.Nothing,
        userRole = Prelude.Nothing,
        name = pName_,
        authMode = pAuthMode_,
        vpcId = pVpcId_,
        subnetIds = Prelude.mempty,
        serviceRole = pServiceRole_,
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
createStudio_tags = Lens.lens (\CreateStudio' {tags} -> tags) (\s@CreateStudio' {} a -> s {tags = a} :: CreateStudio) Prelude.. Lens.mapping Lens.coerced

-- | The name that your identity provider (IdP) uses for its @RelayState@
-- parameter. For example, @RelayState@ or @TargetSource@. Specify this
-- value when you use IAM authentication and want to let federated users
-- log in to a Studio using the Studio URL. The @RelayState@ parameter
-- differs by IdP.
createStudio_idpRelayStateParameterName :: Lens.Lens' CreateStudio (Prelude.Maybe Prelude.Text)
createStudio_idpRelayStateParameterName = Lens.lens (\CreateStudio' {idpRelayStateParameterName} -> idpRelayStateParameterName) (\s@CreateStudio' {} a -> s {idpRelayStateParameterName = a} :: CreateStudio)

-- | The authentication endpoint of your identity provider (IdP). Specify
-- this value when you use IAM authentication and want to let federated
-- users log in to a Studio with the Studio URL and credentials from your
-- IdP. Amazon EMR Studio redirects users to this endpoint to enter
-- credentials.
createStudio_idpAuthUrl :: Lens.Lens' CreateStudio (Prelude.Maybe Prelude.Text)
createStudio_idpAuthUrl = Lens.lens (\CreateStudio' {idpAuthUrl} -> idpAuthUrl) (\s@CreateStudio' {} a -> s {idpAuthUrl = a} :: CreateStudio)

-- | A detailed description of the Amazon EMR Studio.
createStudio_description :: Lens.Lens' CreateStudio (Prelude.Maybe Prelude.Text)
createStudio_description = Lens.lens (\CreateStudio' {description} -> description) (\s@CreateStudio' {} a -> s {description = a} :: CreateStudio)

-- | The IAM user role that users and groups assume when logged in to an
-- Amazon EMR Studio. Only specify a @UserRole@ when you use Amazon Web
-- Services SSO authentication. The permissions attached to the @UserRole@
-- can be scoped down for each user or group using session policies.
createStudio_userRole :: Lens.Lens' CreateStudio (Prelude.Maybe Prelude.Text)
createStudio_userRole = Lens.lens (\CreateStudio' {userRole} -> userRole) (\s@CreateStudio' {} a -> s {userRole = a} :: CreateStudio)

-- | A descriptive name for the Amazon EMR Studio.
createStudio_name :: Lens.Lens' CreateStudio Prelude.Text
createStudio_name = Lens.lens (\CreateStudio' {name} -> name) (\s@CreateStudio' {} a -> s {name = a} :: CreateStudio)

-- | Specifies whether the Studio authenticates users using IAM or Amazon Web
-- Services SSO.
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
createStudio_subnetIds = Lens.lens (\CreateStudio' {subnetIds} -> subnetIds) (\s@CreateStudio' {} a -> s {subnetIds = a} :: CreateStudio) Prelude.. Lens.coerced

-- | The IAM role that the Amazon EMR Studio assumes. The service role
-- provides a way for Amazon EMR Studio to interoperate with other Amazon
-- Web Services services.
createStudio_serviceRole :: Lens.Lens' CreateStudio Prelude.Text
createStudio_serviceRole = Lens.lens (\CreateStudio' {serviceRole} -> serviceRole) (\s@CreateStudio' {} a -> s {serviceRole = a} :: CreateStudio)

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

-- | The Amazon S3 location to back up Amazon EMR Studio Workspaces and
-- notebook files.
createStudio_defaultS3Location :: Lens.Lens' CreateStudio Prelude.Text
createStudio_defaultS3Location = Lens.lens (\CreateStudio' {defaultS3Location} -> defaultS3Location) (\s@CreateStudio' {} a -> s {defaultS3Location = a} :: CreateStudio)

instance Core.AWSRequest CreateStudio where
  type AWSResponse CreateStudio = CreateStudioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStudioResponse'
            Prelude.<$> (x Data..?> "StudioId")
            Prelude.<*> (x Data..?> "Url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStudio where
  hashWithSalt _salt CreateStudio' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` idpRelayStateParameterName
      `Prelude.hashWithSalt` idpAuthUrl
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` userRole
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` authMode
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` workspaceSecurityGroupId
      `Prelude.hashWithSalt` engineSecurityGroupId
      `Prelude.hashWithSalt` defaultS3Location

instance Prelude.NFData CreateStudio where
  rnf CreateStudio' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf idpRelayStateParameterName
      `Prelude.seq` Prelude.rnf idpAuthUrl
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf userRole
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf authMode
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf workspaceSecurityGroupId
      `Prelude.seq` Prelude.rnf engineSecurityGroupId
      `Prelude.seq` Prelude.rnf defaultS3Location

instance Data.ToHeaders CreateStudio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.CreateStudio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStudio where
  toJSON CreateStudio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("IdpRelayStateParameterName" Data..=)
              Prelude.<$> idpRelayStateParameterName,
            ("IdpAuthUrl" Data..=) Prelude.<$> idpAuthUrl,
            ("Description" Data..=) Prelude.<$> description,
            ("UserRole" Data..=) Prelude.<$> userRole,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("AuthMode" Data..= authMode),
            Prelude.Just ("VpcId" Data..= vpcId),
            Prelude.Just ("SubnetIds" Data..= subnetIds),
            Prelude.Just ("ServiceRole" Data..= serviceRole),
            Prelude.Just
              ( "WorkspaceSecurityGroupId"
                  Data..= workspaceSecurityGroupId
              ),
            Prelude.Just
              ( "EngineSecurityGroupId"
                  Data..= engineSecurityGroupId
              ),
            Prelude.Just
              ("DefaultS3Location" Data..= defaultS3Location)
          ]
      )

instance Data.ToPath CreateStudio where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text,
    -- | The unique Studio access URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'createStudioResponse_studioId' - The ID of the Amazon EMR Studio.
--
-- 'url', 'createStudioResponse_url' - The unique Studio access URL.
--
-- 'httpStatus', 'createStudioResponse_httpStatus' - The response's http status code.
newCreateStudioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStudioResponse
newCreateStudioResponse pHttpStatus_ =
  CreateStudioResponse'
    { studioId = Prelude.Nothing,
      url = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Amazon EMR Studio.
createStudioResponse_studioId :: Lens.Lens' CreateStudioResponse (Prelude.Maybe Prelude.Text)
createStudioResponse_studioId = Lens.lens (\CreateStudioResponse' {studioId} -> studioId) (\s@CreateStudioResponse' {} a -> s {studioId = a} :: CreateStudioResponse)

-- | The unique Studio access URL.
createStudioResponse_url :: Lens.Lens' CreateStudioResponse (Prelude.Maybe Prelude.Text)
createStudioResponse_url = Lens.lens (\CreateStudioResponse' {url} -> url) (\s@CreateStudioResponse' {} a -> s {url = a} :: CreateStudioResponse)

-- | The response's http status code.
createStudioResponse_httpStatus :: Lens.Lens' CreateStudioResponse Prelude.Int
createStudioResponse_httpStatus = Lens.lens (\CreateStudioResponse' {httpStatus} -> httpStatus) (\s@CreateStudioResponse' {} a -> s {httpStatus = a} :: CreateStudioResponse)

instance Prelude.NFData CreateStudioResponse where
  rnf CreateStudioResponse' {..} =
    Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf httpStatus
