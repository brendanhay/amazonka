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
-- Module      : Amazonka.Grafana.UpdateWorkspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing Amazon Managed Grafana workspace. If you use this
-- operation and omit any optional parameters, the existing values of those
-- parameters are not changed.
--
-- To modify the user authentication methods that the workspace uses, such
-- as SAML or IAM Identity Center, use
-- <https://docs.aws.amazon.com/grafana/latest/APIReference/API_UpdateWorkspaceAuthentication.html UpdateWorkspaceAuthentication>.
--
-- To modify which users in the workspace have the @Admin@ and @Editor@
-- Grafana roles, use
-- <https://docs.aws.amazon.com/grafana/latest/APIReference/API_UpdatePermissions.html UpdatePermissions>.
module Amazonka.Grafana.UpdateWorkspace
  ( -- * Creating a Request
    UpdateWorkspace (..),
    newUpdateWorkspace,

    -- * Request Lenses
    updateWorkspace_accountAccessType,
    updateWorkspace_organizationRoleName,
    updateWorkspace_permissionType,
    updateWorkspace_removeVpcConfiguration,
    updateWorkspace_stackSetName,
    updateWorkspace_vpcConfiguration,
    updateWorkspace_workspaceDataSources,
    updateWorkspace_workspaceDescription,
    updateWorkspace_workspaceName,
    updateWorkspace_workspaceNotificationDestinations,
    updateWorkspace_workspaceOrganizationalUnits,
    updateWorkspace_workspaceRoleArn,
    updateWorkspace_workspaceId,

    -- * Destructuring the Response
    UpdateWorkspaceResponse (..),
    newUpdateWorkspaceResponse,

    -- * Response Lenses
    updateWorkspaceResponse_httpStatus,
    updateWorkspaceResponse_workspace,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkspace' smart constructor.
data UpdateWorkspace = UpdateWorkspace'
  { -- | Specifies whether the workspace can access Amazon Web Services resources
    -- in this Amazon Web Services account only, or whether it can also access
    -- Amazon Web Services resources in other accounts in the same
    -- organization. If you specify @ORGANIZATION@, you must specify which
    -- organizational units the workspace can access in the
    -- @workspaceOrganizationalUnits@ parameter.
    accountAccessType :: Prelude.Maybe AccountAccessType,
    -- | The name of an IAM role that already exists to use to access resources
    -- through Organizations.
    organizationRoleName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | If you specify @Service Managed@, Amazon Managed Grafana automatically
    -- creates the IAM roles and provisions the permissions that the workspace
    -- needs to use Amazon Web Services data sources and notification channels.
    --
    -- If you specify @CUSTOMER_MANAGED@, you will manage those roles and
    -- permissions yourself. If you are creating this workspace in a member
    -- account of an organization and that account is not a delegated
    -- administrator account, and you want the workspace to access data sources
    -- in other Amazon Web Services accounts in the organization, you must
    -- choose @CUSTOMER_MANAGED@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-manage-permissions.html Amazon Managed Grafana permissions and policies for Amazon Web Services data sources and notification channels>
    permissionType :: Prelude.Maybe PermissionType,
    -- | Whether to remove the VPC configuration from the workspace.
    --
    -- Setting this to @true@ and providing a @vpcConfiguration@ to set will
    -- return an error.
    removeVpcConfiguration :: Prelude.Maybe Prelude.Bool,
    -- | The name of the CloudFormation stack set to use to generate IAM roles to
    -- be used for this workspace.
    stackSetName :: Prelude.Maybe Prelude.Text,
    -- | The configuration settings for an Amazon VPC that contains data sources
    -- for your Grafana workspace to connect to.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | Specify the Amazon Web Services data sources that you want to be queried
    -- in this workspace. Specifying these data sources here enables Amazon
    -- Managed Grafana to create IAM roles and permissions that allow Amazon
    -- Managed Grafana to read data from these sources. You must still add them
    -- as data sources in the Grafana console in the workspace.
    --
    -- If you don\'t specify a data source here, you can still add it as a data
    -- source later in the workspace console. However, you will then have to
    -- manually configure permissions for it.
    workspaceDataSources :: Prelude.Maybe [DataSourceType],
    -- | A description for the workspace. This is used only to help you identify
    -- this workspace.
    workspaceDescription :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A new name for the workspace to update.
    workspaceName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specify the Amazon Web Services notification channels that you plan to
    -- use in this workspace. Specifying these data sources here enables Amazon
    -- Managed Grafana to create IAM roles and permissions that allow Amazon
    -- Managed Grafana to use these channels.
    workspaceNotificationDestinations :: Prelude.Maybe [NotificationDestinationType],
    -- | Specifies the organizational units that this workspace is allowed to use
    -- data sources from, if this workspace is in an account that is part of an
    -- organization.
    workspaceOrganizationalUnits :: Prelude.Maybe (Data.Sensitive [Prelude.Text]),
    -- | The workspace needs an IAM role that grants permissions to the Amazon
    -- Web Services resources that the workspace will view data from. If you
    -- already have a role that you want to use, specify it here. If you omit
    -- this field and you specify some Amazon Web Services resources in
    -- @workspaceDataSources@ or @workspaceNotificationDestinations@, a new IAM
    -- role with the necessary permissions is automatically created.
    workspaceRoleArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the workspace to update.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAccessType', 'updateWorkspace_accountAccessType' - Specifies whether the workspace can access Amazon Web Services resources
-- in this Amazon Web Services account only, or whether it can also access
-- Amazon Web Services resources in other accounts in the same
-- organization. If you specify @ORGANIZATION@, you must specify which
-- organizational units the workspace can access in the
-- @workspaceOrganizationalUnits@ parameter.
--
-- 'organizationRoleName', 'updateWorkspace_organizationRoleName' - The name of an IAM role that already exists to use to access resources
-- through Organizations.
--
-- 'permissionType', 'updateWorkspace_permissionType' - If you specify @Service Managed@, Amazon Managed Grafana automatically
-- creates the IAM roles and provisions the permissions that the workspace
-- needs to use Amazon Web Services data sources and notification channels.
--
-- If you specify @CUSTOMER_MANAGED@, you will manage those roles and
-- permissions yourself. If you are creating this workspace in a member
-- account of an organization and that account is not a delegated
-- administrator account, and you want the workspace to access data sources
-- in other Amazon Web Services accounts in the organization, you must
-- choose @CUSTOMER_MANAGED@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-manage-permissions.html Amazon Managed Grafana permissions and policies for Amazon Web Services data sources and notification channels>
--
-- 'removeVpcConfiguration', 'updateWorkspace_removeVpcConfiguration' - Whether to remove the VPC configuration from the workspace.
--
-- Setting this to @true@ and providing a @vpcConfiguration@ to set will
-- return an error.
--
-- 'stackSetName', 'updateWorkspace_stackSetName' - The name of the CloudFormation stack set to use to generate IAM roles to
-- be used for this workspace.
--
-- 'vpcConfiguration', 'updateWorkspace_vpcConfiguration' - The configuration settings for an Amazon VPC that contains data sources
-- for your Grafana workspace to connect to.
--
-- 'workspaceDataSources', 'updateWorkspace_workspaceDataSources' - Specify the Amazon Web Services data sources that you want to be queried
-- in this workspace. Specifying these data sources here enables Amazon
-- Managed Grafana to create IAM roles and permissions that allow Amazon
-- Managed Grafana to read data from these sources. You must still add them
-- as data sources in the Grafana console in the workspace.
--
-- If you don\'t specify a data source here, you can still add it as a data
-- source later in the workspace console. However, you will then have to
-- manually configure permissions for it.
--
-- 'workspaceDescription', 'updateWorkspace_workspaceDescription' - A description for the workspace. This is used only to help you identify
-- this workspace.
--
-- 'workspaceName', 'updateWorkspace_workspaceName' - A new name for the workspace to update.
--
-- 'workspaceNotificationDestinations', 'updateWorkspace_workspaceNotificationDestinations' - Specify the Amazon Web Services notification channels that you plan to
-- use in this workspace. Specifying these data sources here enables Amazon
-- Managed Grafana to create IAM roles and permissions that allow Amazon
-- Managed Grafana to use these channels.
--
-- 'workspaceOrganizationalUnits', 'updateWorkspace_workspaceOrganizationalUnits' - Specifies the organizational units that this workspace is allowed to use
-- data sources from, if this workspace is in an account that is part of an
-- organization.
--
-- 'workspaceRoleArn', 'updateWorkspace_workspaceRoleArn' - The workspace needs an IAM role that grants permissions to the Amazon
-- Web Services resources that the workspace will view data from. If you
-- already have a role that you want to use, specify it here. If you omit
-- this field and you specify some Amazon Web Services resources in
-- @workspaceDataSources@ or @workspaceNotificationDestinations@, a new IAM
-- role with the necessary permissions is automatically created.
--
-- 'workspaceId', 'updateWorkspace_workspaceId' - The ID of the workspace to update.
newUpdateWorkspace ::
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateWorkspace
newUpdateWorkspace pWorkspaceId_ =
  UpdateWorkspace'
    { accountAccessType =
        Prelude.Nothing,
      organizationRoleName = Prelude.Nothing,
      permissionType = Prelude.Nothing,
      removeVpcConfiguration = Prelude.Nothing,
      stackSetName = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      workspaceDataSources = Prelude.Nothing,
      workspaceDescription = Prelude.Nothing,
      workspaceName = Prelude.Nothing,
      workspaceNotificationDestinations = Prelude.Nothing,
      workspaceOrganizationalUnits = Prelude.Nothing,
      workspaceRoleArn = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | Specifies whether the workspace can access Amazon Web Services resources
-- in this Amazon Web Services account only, or whether it can also access
-- Amazon Web Services resources in other accounts in the same
-- organization. If you specify @ORGANIZATION@, you must specify which
-- organizational units the workspace can access in the
-- @workspaceOrganizationalUnits@ parameter.
updateWorkspace_accountAccessType :: Lens.Lens' UpdateWorkspace (Prelude.Maybe AccountAccessType)
updateWorkspace_accountAccessType = Lens.lens (\UpdateWorkspace' {accountAccessType} -> accountAccessType) (\s@UpdateWorkspace' {} a -> s {accountAccessType = a} :: UpdateWorkspace)

-- | The name of an IAM role that already exists to use to access resources
-- through Organizations.
updateWorkspace_organizationRoleName :: Lens.Lens' UpdateWorkspace (Prelude.Maybe Prelude.Text)
updateWorkspace_organizationRoleName = Lens.lens (\UpdateWorkspace' {organizationRoleName} -> organizationRoleName) (\s@UpdateWorkspace' {} a -> s {organizationRoleName = a} :: UpdateWorkspace) Prelude.. Lens.mapping Data._Sensitive

-- | If you specify @Service Managed@, Amazon Managed Grafana automatically
-- creates the IAM roles and provisions the permissions that the workspace
-- needs to use Amazon Web Services data sources and notification channels.
--
-- If you specify @CUSTOMER_MANAGED@, you will manage those roles and
-- permissions yourself. If you are creating this workspace in a member
-- account of an organization and that account is not a delegated
-- administrator account, and you want the workspace to access data sources
-- in other Amazon Web Services accounts in the organization, you must
-- choose @CUSTOMER_MANAGED@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-manage-permissions.html Amazon Managed Grafana permissions and policies for Amazon Web Services data sources and notification channels>
updateWorkspace_permissionType :: Lens.Lens' UpdateWorkspace (Prelude.Maybe PermissionType)
updateWorkspace_permissionType = Lens.lens (\UpdateWorkspace' {permissionType} -> permissionType) (\s@UpdateWorkspace' {} a -> s {permissionType = a} :: UpdateWorkspace)

-- | Whether to remove the VPC configuration from the workspace.
--
-- Setting this to @true@ and providing a @vpcConfiguration@ to set will
-- return an error.
updateWorkspace_removeVpcConfiguration :: Lens.Lens' UpdateWorkspace (Prelude.Maybe Prelude.Bool)
updateWorkspace_removeVpcConfiguration = Lens.lens (\UpdateWorkspace' {removeVpcConfiguration} -> removeVpcConfiguration) (\s@UpdateWorkspace' {} a -> s {removeVpcConfiguration = a} :: UpdateWorkspace)

-- | The name of the CloudFormation stack set to use to generate IAM roles to
-- be used for this workspace.
updateWorkspace_stackSetName :: Lens.Lens' UpdateWorkspace (Prelude.Maybe Prelude.Text)
updateWorkspace_stackSetName = Lens.lens (\UpdateWorkspace' {stackSetName} -> stackSetName) (\s@UpdateWorkspace' {} a -> s {stackSetName = a} :: UpdateWorkspace)

-- | The configuration settings for an Amazon VPC that contains data sources
-- for your Grafana workspace to connect to.
updateWorkspace_vpcConfiguration :: Lens.Lens' UpdateWorkspace (Prelude.Maybe VpcConfiguration)
updateWorkspace_vpcConfiguration = Lens.lens (\UpdateWorkspace' {vpcConfiguration} -> vpcConfiguration) (\s@UpdateWorkspace' {} a -> s {vpcConfiguration = a} :: UpdateWorkspace)

-- | Specify the Amazon Web Services data sources that you want to be queried
-- in this workspace. Specifying these data sources here enables Amazon
-- Managed Grafana to create IAM roles and permissions that allow Amazon
-- Managed Grafana to read data from these sources. You must still add them
-- as data sources in the Grafana console in the workspace.
--
-- If you don\'t specify a data source here, you can still add it as a data
-- source later in the workspace console. However, you will then have to
-- manually configure permissions for it.
updateWorkspace_workspaceDataSources :: Lens.Lens' UpdateWorkspace (Prelude.Maybe [DataSourceType])
updateWorkspace_workspaceDataSources = Lens.lens (\UpdateWorkspace' {workspaceDataSources} -> workspaceDataSources) (\s@UpdateWorkspace' {} a -> s {workspaceDataSources = a} :: UpdateWorkspace) Prelude.. Lens.mapping Lens.coerced

-- | A description for the workspace. This is used only to help you identify
-- this workspace.
updateWorkspace_workspaceDescription :: Lens.Lens' UpdateWorkspace (Prelude.Maybe Prelude.Text)
updateWorkspace_workspaceDescription = Lens.lens (\UpdateWorkspace' {workspaceDescription} -> workspaceDescription) (\s@UpdateWorkspace' {} a -> s {workspaceDescription = a} :: UpdateWorkspace) Prelude.. Lens.mapping Data._Sensitive

-- | A new name for the workspace to update.
updateWorkspace_workspaceName :: Lens.Lens' UpdateWorkspace (Prelude.Maybe Prelude.Text)
updateWorkspace_workspaceName = Lens.lens (\UpdateWorkspace' {workspaceName} -> workspaceName) (\s@UpdateWorkspace' {} a -> s {workspaceName = a} :: UpdateWorkspace) Prelude.. Lens.mapping Data._Sensitive

-- | Specify the Amazon Web Services notification channels that you plan to
-- use in this workspace. Specifying these data sources here enables Amazon
-- Managed Grafana to create IAM roles and permissions that allow Amazon
-- Managed Grafana to use these channels.
updateWorkspace_workspaceNotificationDestinations :: Lens.Lens' UpdateWorkspace (Prelude.Maybe [NotificationDestinationType])
updateWorkspace_workspaceNotificationDestinations = Lens.lens (\UpdateWorkspace' {workspaceNotificationDestinations} -> workspaceNotificationDestinations) (\s@UpdateWorkspace' {} a -> s {workspaceNotificationDestinations = a} :: UpdateWorkspace) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the organizational units that this workspace is allowed to use
-- data sources from, if this workspace is in an account that is part of an
-- organization.
updateWorkspace_workspaceOrganizationalUnits :: Lens.Lens' UpdateWorkspace (Prelude.Maybe [Prelude.Text])
updateWorkspace_workspaceOrganizationalUnits = Lens.lens (\UpdateWorkspace' {workspaceOrganizationalUnits} -> workspaceOrganizationalUnits) (\s@UpdateWorkspace' {} a -> s {workspaceOrganizationalUnits = a} :: UpdateWorkspace) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The workspace needs an IAM role that grants permissions to the Amazon
-- Web Services resources that the workspace will view data from. If you
-- already have a role that you want to use, specify it here. If you omit
-- this field and you specify some Amazon Web Services resources in
-- @workspaceDataSources@ or @workspaceNotificationDestinations@, a new IAM
-- role with the necessary permissions is automatically created.
updateWorkspace_workspaceRoleArn :: Lens.Lens' UpdateWorkspace (Prelude.Maybe Prelude.Text)
updateWorkspace_workspaceRoleArn = Lens.lens (\UpdateWorkspace' {workspaceRoleArn} -> workspaceRoleArn) (\s@UpdateWorkspace' {} a -> s {workspaceRoleArn = a} :: UpdateWorkspace) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the workspace to update.
updateWorkspace_workspaceId :: Lens.Lens' UpdateWorkspace Prelude.Text
updateWorkspace_workspaceId = Lens.lens (\UpdateWorkspace' {workspaceId} -> workspaceId) (\s@UpdateWorkspace' {} a -> s {workspaceId = a} :: UpdateWorkspace)

instance Core.AWSRequest UpdateWorkspace where
  type
    AWSResponse UpdateWorkspace =
      UpdateWorkspaceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workspace")
      )

instance Prelude.Hashable UpdateWorkspace where
  hashWithSalt _salt UpdateWorkspace' {..} =
    _salt
      `Prelude.hashWithSalt` accountAccessType
      `Prelude.hashWithSalt` organizationRoleName
      `Prelude.hashWithSalt` permissionType
      `Prelude.hashWithSalt` removeVpcConfiguration
      `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` workspaceDataSources
      `Prelude.hashWithSalt` workspaceDescription
      `Prelude.hashWithSalt` workspaceName
      `Prelude.hashWithSalt` workspaceNotificationDestinations
      `Prelude.hashWithSalt` workspaceOrganizationalUnits
      `Prelude.hashWithSalt` workspaceRoleArn
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData UpdateWorkspace where
  rnf UpdateWorkspace' {..} =
    Prelude.rnf accountAccessType
      `Prelude.seq` Prelude.rnf organizationRoleName
      `Prelude.seq` Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf removeVpcConfiguration
      `Prelude.seq` Prelude.rnf stackSetName
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf workspaceDataSources
      `Prelude.seq` Prelude.rnf workspaceDescription
      `Prelude.seq` Prelude.rnf workspaceName
      `Prelude.seq` Prelude.rnf workspaceNotificationDestinations
      `Prelude.seq` Prelude.rnf workspaceOrganizationalUnits
      `Prelude.seq` Prelude.rnf workspaceRoleArn
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders UpdateWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkspace where
  toJSON UpdateWorkspace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountAccessType" Data..=)
              Prelude.<$> accountAccessType,
            ("organizationRoleName" Data..=)
              Prelude.<$> organizationRoleName,
            ("permissionType" Data..=)
              Prelude.<$> permissionType,
            ("removeVpcConfiguration" Data..=)
              Prelude.<$> removeVpcConfiguration,
            ("stackSetName" Data..=) Prelude.<$> stackSetName,
            ("vpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            ("workspaceDataSources" Data..=)
              Prelude.<$> workspaceDataSources,
            ("workspaceDescription" Data..=)
              Prelude.<$> workspaceDescription,
            ("workspaceName" Data..=) Prelude.<$> workspaceName,
            ("workspaceNotificationDestinations" Data..=)
              Prelude.<$> workspaceNotificationDestinations,
            ("workspaceOrganizationalUnits" Data..=)
              Prelude.<$> workspaceOrganizationalUnits,
            ("workspaceRoleArn" Data..=)
              Prelude.<$> workspaceRoleArn
          ]
      )

instance Data.ToPath UpdateWorkspace where
  toPath UpdateWorkspace' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId]

instance Data.ToQuery UpdateWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkspaceResponse' smart constructor.
data UpdateWorkspaceResponse = UpdateWorkspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing data about the workspace that was created.
    workspace :: WorkspaceDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkspaceResponse_httpStatus' - The response's http status code.
--
-- 'workspace', 'updateWorkspaceResponse_workspace' - A structure containing data about the workspace that was created.
newUpdateWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workspace'
  WorkspaceDescription ->
  UpdateWorkspaceResponse
newUpdateWorkspaceResponse pHttpStatus_ pWorkspace_ =
  UpdateWorkspaceResponse'
    { httpStatus = pHttpStatus_,
      workspace = pWorkspace_
    }

-- | The response's http status code.
updateWorkspaceResponse_httpStatus :: Lens.Lens' UpdateWorkspaceResponse Prelude.Int
updateWorkspaceResponse_httpStatus = Lens.lens (\UpdateWorkspaceResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkspaceResponse' {} a -> s {httpStatus = a} :: UpdateWorkspaceResponse)

-- | A structure containing data about the workspace that was created.
updateWorkspaceResponse_workspace :: Lens.Lens' UpdateWorkspaceResponse WorkspaceDescription
updateWorkspaceResponse_workspace = Lens.lens (\UpdateWorkspaceResponse' {workspace} -> workspace) (\s@UpdateWorkspaceResponse' {} a -> s {workspace = a} :: UpdateWorkspaceResponse)

instance Prelude.NFData UpdateWorkspaceResponse where
  rnf UpdateWorkspaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workspace
