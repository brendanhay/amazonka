{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Grafana.Types.WorkspaceDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Grafana.Types.WorkspaceDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.Grafana.Types.AccountAccessType
import Network.AWS.Grafana.Types.AuthenticationSummary
import Network.AWS.Grafana.Types.DataSourceType
import Network.AWS.Grafana.Types.LicenseType
import Network.AWS.Grafana.Types.NotificationDestinationType
import Network.AWS.Grafana.Types.PermissionType
import Network.AWS.Grafana.Types.WorkspaceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing information about an Amazon Managed Grafana
-- workspace in your account.
--
-- /See:/ 'newWorkspaceDescription' smart constructor.
data WorkspaceDescription = WorkspaceDescription'
  { -- | The IAM role that grants permissions to the Amazon Web Services
    -- resources that the workspace will view data from. This role must already
    -- exist.
    workspaceRoleArn :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | If this workspace is currently in the free trial period for Grafana
    -- Enterprise, this value specifies when that free trial ends.
    freeTrialExpiration :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether this workspace has a full Grafana Enterprise license
    -- or a free trial license.
    licenseType :: Prelude.Maybe LicenseType,
    -- | If this is @Service Managed@, Amazon Managed Grafana automatically
    -- creates the IAM roles and provisions the permissions that the workspace
    -- needs to use Amazon Web Services data sources and notification channels.
    --
    -- If this is @CUSTOMER_MANAGED@, you manage those roles and permissions
    -- yourself. If you are creating this workspace in a member account of an
    -- organization and that account is not a delegated administrator account,
    -- and you want the workspace to access data sources in other Amazon Web
    -- Services accounts in the organization, you must choose
    -- @CUSTOMER_MANAGED@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-manage-permissions.html Amazon Managed Grafana permissions and policies for Amazon Web Services data sources and notification channels>
    permissionType :: Prelude.Maybe PermissionType,
    -- | The name of the workspace.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Web Services notification channels that Amazon Managed
    -- Grafana can automatically create IAM roles and permissions for, to allow
    -- Amazon Managed Grafana to use these channels.
    notificationDestinations :: Prelude.Maybe [NotificationDestinationType],
    -- | Specifies whether the workspace can access Amazon Web Services resources
    -- in this Amazon Web Services account only, or whether it can also access
    -- Amazon Web Services resources in other accounts in the same
    -- organization. If this is @ORGANIZATION@, the
    -- @workspaceOrganizationalUnits@ parameter specifies which organizational
    -- units the workspace can access.
    accountAccessType :: Prelude.Maybe AccountAccessType,
    -- | If this workspace has a full Grafana Enterprise license, this specifies
    -- when the license ends and will need to be renewed.
    licenseExpiration :: Prelude.Maybe Core.POSIX,
    -- | The name of the IAM role that is used to access resources through
    -- Organizations.
    organizationRoleName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The name of the CloudFormation stack set that is used to generate IAM
    -- roles to be used for this workspace.
    stackSetName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the organizational units that this workspace is allowed to use
    -- data sources from, if this workspace is in an account that is part of an
    -- organization.
    organizationalUnits :: Prelude.Maybe (Core.Sensitive [Prelude.Text]),
    -- | The user-defined description of the workspace.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Specifies whether this workspace has already fully used its free trial
    -- for Grafana Enterprise.
    freeTrialConsumed :: Prelude.Maybe Prelude.Bool,
    -- | A structure that describes whether the workspace uses SAML, Amazon Web
    -- Services SSO, or both methods for user authentication.
    authentication :: AuthenticationSummary,
    -- | The date that the workspace was created.
    created :: Core.POSIX,
    -- | Specifies the Amazon Web Services data sources that have been configured
    -- to have IAM roles and permissions created to allow Amazon Managed
    -- Grafana to read data from these sources.
    dataSources :: [DataSourceType],
    -- | The URL that users can use to access the Grafana console in the
    -- workspace.
    endpoint :: Prelude.Text,
    -- | The version of Grafana supported in this workspace.
    grafanaVersion :: Prelude.Text,
    -- | The unique ID of this workspace.
    id :: Prelude.Text,
    -- | The most recent date that the workspace was modified.
    modified :: Core.POSIX,
    -- | The current status of the workspace.
    status :: WorkspaceStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceRoleArn', 'workspaceDescription_workspaceRoleArn' - The IAM role that grants permissions to the Amazon Web Services
-- resources that the workspace will view data from. This role must already
-- exist.
--
-- 'freeTrialExpiration', 'workspaceDescription_freeTrialExpiration' - If this workspace is currently in the free trial period for Grafana
-- Enterprise, this value specifies when that free trial ends.
--
-- 'licenseType', 'workspaceDescription_licenseType' - Specifies whether this workspace has a full Grafana Enterprise license
-- or a free trial license.
--
-- 'permissionType', 'workspaceDescription_permissionType' - If this is @Service Managed@, Amazon Managed Grafana automatically
-- creates the IAM roles and provisions the permissions that the workspace
-- needs to use Amazon Web Services data sources and notification channels.
--
-- If this is @CUSTOMER_MANAGED@, you manage those roles and permissions
-- yourself. If you are creating this workspace in a member account of an
-- organization and that account is not a delegated administrator account,
-- and you want the workspace to access data sources in other Amazon Web
-- Services accounts in the organization, you must choose
-- @CUSTOMER_MANAGED@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-manage-permissions.html Amazon Managed Grafana permissions and policies for Amazon Web Services data sources and notification channels>
--
-- 'name', 'workspaceDescription_name' - The name of the workspace.
--
-- 'notificationDestinations', 'workspaceDescription_notificationDestinations' - The Amazon Web Services notification channels that Amazon Managed
-- Grafana can automatically create IAM roles and permissions for, to allow
-- Amazon Managed Grafana to use these channels.
--
-- 'accountAccessType', 'workspaceDescription_accountAccessType' - Specifies whether the workspace can access Amazon Web Services resources
-- in this Amazon Web Services account only, or whether it can also access
-- Amazon Web Services resources in other accounts in the same
-- organization. If this is @ORGANIZATION@, the
-- @workspaceOrganizationalUnits@ parameter specifies which organizational
-- units the workspace can access.
--
-- 'licenseExpiration', 'workspaceDescription_licenseExpiration' - If this workspace has a full Grafana Enterprise license, this specifies
-- when the license ends and will need to be renewed.
--
-- 'organizationRoleName', 'workspaceDescription_organizationRoleName' - The name of the IAM role that is used to access resources through
-- Organizations.
--
-- 'stackSetName', 'workspaceDescription_stackSetName' - The name of the CloudFormation stack set that is used to generate IAM
-- roles to be used for this workspace.
--
-- 'organizationalUnits', 'workspaceDescription_organizationalUnits' - Specifies the organizational units that this workspace is allowed to use
-- data sources from, if this workspace is in an account that is part of an
-- organization.
--
-- 'description', 'workspaceDescription_description' - The user-defined description of the workspace.
--
-- 'freeTrialConsumed', 'workspaceDescription_freeTrialConsumed' - Specifies whether this workspace has already fully used its free trial
-- for Grafana Enterprise.
--
-- 'authentication', 'workspaceDescription_authentication' - A structure that describes whether the workspace uses SAML, Amazon Web
-- Services SSO, or both methods for user authentication.
--
-- 'created', 'workspaceDescription_created' - The date that the workspace was created.
--
-- 'dataSources', 'workspaceDescription_dataSources' - Specifies the Amazon Web Services data sources that have been configured
-- to have IAM roles and permissions created to allow Amazon Managed
-- Grafana to read data from these sources.
--
-- 'endpoint', 'workspaceDescription_endpoint' - The URL that users can use to access the Grafana console in the
-- workspace.
--
-- 'grafanaVersion', 'workspaceDescription_grafanaVersion' - The version of Grafana supported in this workspace.
--
-- 'id', 'workspaceDescription_id' - The unique ID of this workspace.
--
-- 'modified', 'workspaceDescription_modified' - The most recent date that the workspace was modified.
--
-- 'status', 'workspaceDescription_status' - The current status of the workspace.
newWorkspaceDescription ::
  -- | 'authentication'
  AuthenticationSummary ->
  -- | 'created'
  Prelude.UTCTime ->
  -- | 'endpoint'
  Prelude.Text ->
  -- | 'grafanaVersion'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'modified'
  Prelude.UTCTime ->
  -- | 'status'
  WorkspaceStatus ->
  WorkspaceDescription
newWorkspaceDescription
  pAuthentication_
  pCreated_
  pEndpoint_
  pGrafanaVersion_
  pId_
  pModified_
  pStatus_ =
    WorkspaceDescription'
      { workspaceRoleArn =
          Prelude.Nothing,
        freeTrialExpiration = Prelude.Nothing,
        licenseType = Prelude.Nothing,
        permissionType = Prelude.Nothing,
        name = Prelude.Nothing,
        notificationDestinations = Prelude.Nothing,
        accountAccessType = Prelude.Nothing,
        licenseExpiration = Prelude.Nothing,
        organizationRoleName = Prelude.Nothing,
        stackSetName = Prelude.Nothing,
        organizationalUnits = Prelude.Nothing,
        description = Prelude.Nothing,
        freeTrialConsumed = Prelude.Nothing,
        authentication = pAuthentication_,
        created = Core._Time Lens.# pCreated_,
        dataSources = Prelude.mempty,
        endpoint = pEndpoint_,
        grafanaVersion = pGrafanaVersion_,
        id = pId_,
        modified = Core._Time Lens.# pModified_,
        status = pStatus_
      }

-- | The IAM role that grants permissions to the Amazon Web Services
-- resources that the workspace will view data from. This role must already
-- exist.
workspaceDescription_workspaceRoleArn :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_workspaceRoleArn = Lens.lens (\WorkspaceDescription' {workspaceRoleArn} -> workspaceRoleArn) (\s@WorkspaceDescription' {} a -> s {workspaceRoleArn = a} :: WorkspaceDescription) Prelude.. Lens.mapping Core._Sensitive

-- | If this workspace is currently in the free trial period for Grafana
-- Enterprise, this value specifies when that free trial ends.
workspaceDescription_freeTrialExpiration :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.UTCTime)
workspaceDescription_freeTrialExpiration = Lens.lens (\WorkspaceDescription' {freeTrialExpiration} -> freeTrialExpiration) (\s@WorkspaceDescription' {} a -> s {freeTrialExpiration = a} :: WorkspaceDescription) Prelude.. Lens.mapping Core._Time

-- | Specifies whether this workspace has a full Grafana Enterprise license
-- or a free trial license.
workspaceDescription_licenseType :: Lens.Lens' WorkspaceDescription (Prelude.Maybe LicenseType)
workspaceDescription_licenseType = Lens.lens (\WorkspaceDescription' {licenseType} -> licenseType) (\s@WorkspaceDescription' {} a -> s {licenseType = a} :: WorkspaceDescription)

-- | If this is @Service Managed@, Amazon Managed Grafana automatically
-- creates the IAM roles and provisions the permissions that the workspace
-- needs to use Amazon Web Services data sources and notification channels.
--
-- If this is @CUSTOMER_MANAGED@, you manage those roles and permissions
-- yourself. If you are creating this workspace in a member account of an
-- organization and that account is not a delegated administrator account,
-- and you want the workspace to access data sources in other Amazon Web
-- Services accounts in the organization, you must choose
-- @CUSTOMER_MANAGED@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-manage-permissions.html Amazon Managed Grafana permissions and policies for Amazon Web Services data sources and notification channels>
workspaceDescription_permissionType :: Lens.Lens' WorkspaceDescription (Prelude.Maybe PermissionType)
workspaceDescription_permissionType = Lens.lens (\WorkspaceDescription' {permissionType} -> permissionType) (\s@WorkspaceDescription' {} a -> s {permissionType = a} :: WorkspaceDescription)

-- | The name of the workspace.
workspaceDescription_name :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_name = Lens.lens (\WorkspaceDescription' {name} -> name) (\s@WorkspaceDescription' {} a -> s {name = a} :: WorkspaceDescription) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Web Services notification channels that Amazon Managed
-- Grafana can automatically create IAM roles and permissions for, to allow
-- Amazon Managed Grafana to use these channels.
workspaceDescription_notificationDestinations :: Lens.Lens' WorkspaceDescription (Prelude.Maybe [NotificationDestinationType])
workspaceDescription_notificationDestinations = Lens.lens (\WorkspaceDescription' {notificationDestinations} -> notificationDestinations) (\s@WorkspaceDescription' {} a -> s {notificationDestinations = a} :: WorkspaceDescription) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the workspace can access Amazon Web Services resources
-- in this Amazon Web Services account only, or whether it can also access
-- Amazon Web Services resources in other accounts in the same
-- organization. If this is @ORGANIZATION@, the
-- @workspaceOrganizationalUnits@ parameter specifies which organizational
-- units the workspace can access.
workspaceDescription_accountAccessType :: Lens.Lens' WorkspaceDescription (Prelude.Maybe AccountAccessType)
workspaceDescription_accountAccessType = Lens.lens (\WorkspaceDescription' {accountAccessType} -> accountAccessType) (\s@WorkspaceDescription' {} a -> s {accountAccessType = a} :: WorkspaceDescription)

-- | If this workspace has a full Grafana Enterprise license, this specifies
-- when the license ends and will need to be renewed.
workspaceDescription_licenseExpiration :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.UTCTime)
workspaceDescription_licenseExpiration = Lens.lens (\WorkspaceDescription' {licenseExpiration} -> licenseExpiration) (\s@WorkspaceDescription' {} a -> s {licenseExpiration = a} :: WorkspaceDescription) Prelude.. Lens.mapping Core._Time

-- | The name of the IAM role that is used to access resources through
-- Organizations.
workspaceDescription_organizationRoleName :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_organizationRoleName = Lens.lens (\WorkspaceDescription' {organizationRoleName} -> organizationRoleName) (\s@WorkspaceDescription' {} a -> s {organizationRoleName = a} :: WorkspaceDescription) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the CloudFormation stack set that is used to generate IAM
-- roles to be used for this workspace.
workspaceDescription_stackSetName :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_stackSetName = Lens.lens (\WorkspaceDescription' {stackSetName} -> stackSetName) (\s@WorkspaceDescription' {} a -> s {stackSetName = a} :: WorkspaceDescription)

-- | Specifies the organizational units that this workspace is allowed to use
-- data sources from, if this workspace is in an account that is part of an
-- organization.
workspaceDescription_organizationalUnits :: Lens.Lens' WorkspaceDescription (Prelude.Maybe [Prelude.Text])
workspaceDescription_organizationalUnits = Lens.lens (\WorkspaceDescription' {organizationalUnits} -> organizationalUnits) (\s@WorkspaceDescription' {} a -> s {organizationalUnits = a} :: WorkspaceDescription) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The user-defined description of the workspace.
workspaceDescription_description :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_description = Lens.lens (\WorkspaceDescription' {description} -> description) (\s@WorkspaceDescription' {} a -> s {description = a} :: WorkspaceDescription) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies whether this workspace has already fully used its free trial
-- for Grafana Enterprise.
workspaceDescription_freeTrialConsumed :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Bool)
workspaceDescription_freeTrialConsumed = Lens.lens (\WorkspaceDescription' {freeTrialConsumed} -> freeTrialConsumed) (\s@WorkspaceDescription' {} a -> s {freeTrialConsumed = a} :: WorkspaceDescription)

-- | A structure that describes whether the workspace uses SAML, Amazon Web
-- Services SSO, or both methods for user authentication.
workspaceDescription_authentication :: Lens.Lens' WorkspaceDescription AuthenticationSummary
workspaceDescription_authentication = Lens.lens (\WorkspaceDescription' {authentication} -> authentication) (\s@WorkspaceDescription' {} a -> s {authentication = a} :: WorkspaceDescription)

-- | The date that the workspace was created.
workspaceDescription_created :: Lens.Lens' WorkspaceDescription Prelude.UTCTime
workspaceDescription_created = Lens.lens (\WorkspaceDescription' {created} -> created) (\s@WorkspaceDescription' {} a -> s {created = a} :: WorkspaceDescription) Prelude.. Core._Time

-- | Specifies the Amazon Web Services data sources that have been configured
-- to have IAM roles and permissions created to allow Amazon Managed
-- Grafana to read data from these sources.
workspaceDescription_dataSources :: Lens.Lens' WorkspaceDescription [DataSourceType]
workspaceDescription_dataSources = Lens.lens (\WorkspaceDescription' {dataSources} -> dataSources) (\s@WorkspaceDescription' {} a -> s {dataSources = a} :: WorkspaceDescription) Prelude.. Lens.coerced

-- | The URL that users can use to access the Grafana console in the
-- workspace.
workspaceDescription_endpoint :: Lens.Lens' WorkspaceDescription Prelude.Text
workspaceDescription_endpoint = Lens.lens (\WorkspaceDescription' {endpoint} -> endpoint) (\s@WorkspaceDescription' {} a -> s {endpoint = a} :: WorkspaceDescription)

-- | The version of Grafana supported in this workspace.
workspaceDescription_grafanaVersion :: Lens.Lens' WorkspaceDescription Prelude.Text
workspaceDescription_grafanaVersion = Lens.lens (\WorkspaceDescription' {grafanaVersion} -> grafanaVersion) (\s@WorkspaceDescription' {} a -> s {grafanaVersion = a} :: WorkspaceDescription)

-- | The unique ID of this workspace.
workspaceDescription_id :: Lens.Lens' WorkspaceDescription Prelude.Text
workspaceDescription_id = Lens.lens (\WorkspaceDescription' {id} -> id) (\s@WorkspaceDescription' {} a -> s {id = a} :: WorkspaceDescription)

-- | The most recent date that the workspace was modified.
workspaceDescription_modified :: Lens.Lens' WorkspaceDescription Prelude.UTCTime
workspaceDescription_modified = Lens.lens (\WorkspaceDescription' {modified} -> modified) (\s@WorkspaceDescription' {} a -> s {modified = a} :: WorkspaceDescription) Prelude.. Core._Time

-- | The current status of the workspace.
workspaceDescription_status :: Lens.Lens' WorkspaceDescription WorkspaceStatus
workspaceDescription_status = Lens.lens (\WorkspaceDescription' {status} -> status) (\s@WorkspaceDescription' {} a -> s {status = a} :: WorkspaceDescription)

instance Core.FromJSON WorkspaceDescription where
  parseJSON =
    Core.withObject
      "WorkspaceDescription"
      ( \x ->
          WorkspaceDescription'
            Prelude.<$> (x Core..:? "workspaceRoleArn")
            Prelude.<*> (x Core..:? "freeTrialExpiration")
            Prelude.<*> (x Core..:? "licenseType")
            Prelude.<*> (x Core..:? "permissionType")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> ( x Core..:? "notificationDestinations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "accountAccessType")
            Prelude.<*> (x Core..:? "licenseExpiration")
            Prelude.<*> (x Core..:? "organizationRoleName")
            Prelude.<*> (x Core..:? "stackSetName")
            Prelude.<*> ( x Core..:? "organizationalUnits"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "freeTrialConsumed")
            Prelude.<*> (x Core..: "authentication")
            Prelude.<*> (x Core..: "created")
            Prelude.<*> (x Core..:? "dataSources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "endpoint")
            Prelude.<*> (x Core..: "grafanaVersion")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "modified")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable WorkspaceDescription

instance Prelude.NFData WorkspaceDescription
