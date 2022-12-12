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
-- Module      : Amazonka.Grafana.Types.WorkspaceDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.WorkspaceDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types.AccountAccessType
import Amazonka.Grafana.Types.AuthenticationSummary
import Amazonka.Grafana.Types.DataSourceType
import Amazonka.Grafana.Types.LicenseType
import Amazonka.Grafana.Types.NotificationDestinationType
import Amazonka.Grafana.Types.PermissionType
import Amazonka.Grafana.Types.VpcConfiguration
import Amazonka.Grafana.Types.WorkspaceStatus
import qualified Amazonka.Prelude as Prelude

-- | A structure containing information about an Amazon Managed Grafana
-- workspace in your account.
--
-- /See:/ 'newWorkspaceDescription' smart constructor.
data WorkspaceDescription = WorkspaceDescription'
  { -- | Specifies whether the workspace can access Amazon Web Services resources
    -- in this Amazon Web Services account only, or whether it can also access
    -- Amazon Web Services resources in other accounts in the same
    -- organization. If this is @ORGANIZATION@, the
    -- @workspaceOrganizationalUnits@ parameter specifies which organizational
    -- units the workspace can access.
    accountAccessType :: Prelude.Maybe AccountAccessType,
    -- | The user-defined description of the workspace.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies whether this workspace has already fully used its free trial
    -- for Grafana Enterprise.
    freeTrialConsumed :: Prelude.Maybe Prelude.Bool,
    -- | If this workspace is currently in the free trial period for Grafana
    -- Enterprise, this value specifies when that free trial ends.
    freeTrialExpiration :: Prelude.Maybe Data.POSIX,
    -- | If this workspace has a full Grafana Enterprise license, this specifies
    -- when the license ends and will need to be renewed.
    licenseExpiration :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether this workspace has a full Grafana Enterprise license
    -- or a free trial license.
    licenseType :: Prelude.Maybe LicenseType,
    -- | The name of the workspace.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Web Services notification channels that Amazon Managed
    -- Grafana can automatically create IAM roles and permissions for, to allow
    -- Amazon Managed Grafana to use these channels.
    notificationDestinations :: Prelude.Maybe [NotificationDestinationType],
    -- | The name of the IAM role that is used to access resources through
    -- Organizations.
    organizationRoleName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the organizational units that this workspace is allowed to use
    -- data sources from, if this workspace is in an account that is part of an
    -- organization.
    organizationalUnits :: Prelude.Maybe (Data.Sensitive [Prelude.Text]),
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
    -- | The name of the CloudFormation stack set that is used to generate IAM
    -- roles to be used for this workspace.
    stackSetName :: Prelude.Maybe Prelude.Text,
    -- | The list of tags associated with the workspace.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The configuration for connecting to data sources in a private VPC
    -- (Amazon Virtual Private Cloud).
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | The IAM role that grants permissions to the Amazon Web Services
    -- resources that the workspace will view data from. This role must already
    -- exist.
    workspaceRoleArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A structure that describes whether the workspace uses SAML, IAM Identity
    -- Center, or both methods for user authentication.
    authentication :: AuthenticationSummary,
    -- | The date that the workspace was created.
    created :: Data.POSIX,
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
    modified :: Data.POSIX,
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
-- 'accountAccessType', 'workspaceDescription_accountAccessType' - Specifies whether the workspace can access Amazon Web Services resources
-- in this Amazon Web Services account only, or whether it can also access
-- Amazon Web Services resources in other accounts in the same
-- organization. If this is @ORGANIZATION@, the
-- @workspaceOrganizationalUnits@ parameter specifies which organizational
-- units the workspace can access.
--
-- 'description', 'workspaceDescription_description' - The user-defined description of the workspace.
--
-- 'freeTrialConsumed', 'workspaceDescription_freeTrialConsumed' - Specifies whether this workspace has already fully used its free trial
-- for Grafana Enterprise.
--
-- 'freeTrialExpiration', 'workspaceDescription_freeTrialExpiration' - If this workspace is currently in the free trial period for Grafana
-- Enterprise, this value specifies when that free trial ends.
--
-- 'licenseExpiration', 'workspaceDescription_licenseExpiration' - If this workspace has a full Grafana Enterprise license, this specifies
-- when the license ends and will need to be renewed.
--
-- 'licenseType', 'workspaceDescription_licenseType' - Specifies whether this workspace has a full Grafana Enterprise license
-- or a free trial license.
--
-- 'name', 'workspaceDescription_name' - The name of the workspace.
--
-- 'notificationDestinations', 'workspaceDescription_notificationDestinations' - The Amazon Web Services notification channels that Amazon Managed
-- Grafana can automatically create IAM roles and permissions for, to allow
-- Amazon Managed Grafana to use these channels.
--
-- 'organizationRoleName', 'workspaceDescription_organizationRoleName' - The name of the IAM role that is used to access resources through
-- Organizations.
--
-- 'organizationalUnits', 'workspaceDescription_organizationalUnits' - Specifies the organizational units that this workspace is allowed to use
-- data sources from, if this workspace is in an account that is part of an
-- organization.
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
-- 'stackSetName', 'workspaceDescription_stackSetName' - The name of the CloudFormation stack set that is used to generate IAM
-- roles to be used for this workspace.
--
-- 'tags', 'workspaceDescription_tags' - The list of tags associated with the workspace.
--
-- 'vpcConfiguration', 'workspaceDescription_vpcConfiguration' - The configuration for connecting to data sources in a private VPC
-- (Amazon Virtual Private Cloud).
--
-- 'workspaceRoleArn', 'workspaceDescription_workspaceRoleArn' - The IAM role that grants permissions to the Amazon Web Services
-- resources that the workspace will view data from. This role must already
-- exist.
--
-- 'authentication', 'workspaceDescription_authentication' - A structure that describes whether the workspace uses SAML, IAM Identity
-- Center, or both methods for user authentication.
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
      { accountAccessType =
          Prelude.Nothing,
        description = Prelude.Nothing,
        freeTrialConsumed = Prelude.Nothing,
        freeTrialExpiration = Prelude.Nothing,
        licenseExpiration = Prelude.Nothing,
        licenseType = Prelude.Nothing,
        name = Prelude.Nothing,
        notificationDestinations = Prelude.Nothing,
        organizationRoleName = Prelude.Nothing,
        organizationalUnits = Prelude.Nothing,
        permissionType = Prelude.Nothing,
        stackSetName = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcConfiguration = Prelude.Nothing,
        workspaceRoleArn = Prelude.Nothing,
        authentication = pAuthentication_,
        created = Data._Time Lens.# pCreated_,
        dataSources = Prelude.mempty,
        endpoint = pEndpoint_,
        grafanaVersion = pGrafanaVersion_,
        id = pId_,
        modified = Data._Time Lens.# pModified_,
        status = pStatus_
      }

-- | Specifies whether the workspace can access Amazon Web Services resources
-- in this Amazon Web Services account only, or whether it can also access
-- Amazon Web Services resources in other accounts in the same
-- organization. If this is @ORGANIZATION@, the
-- @workspaceOrganizationalUnits@ parameter specifies which organizational
-- units the workspace can access.
workspaceDescription_accountAccessType :: Lens.Lens' WorkspaceDescription (Prelude.Maybe AccountAccessType)
workspaceDescription_accountAccessType = Lens.lens (\WorkspaceDescription' {accountAccessType} -> accountAccessType) (\s@WorkspaceDescription' {} a -> s {accountAccessType = a} :: WorkspaceDescription)

-- | The user-defined description of the workspace.
workspaceDescription_description :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_description = Lens.lens (\WorkspaceDescription' {description} -> description) (\s@WorkspaceDescription' {} a -> s {description = a} :: WorkspaceDescription) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies whether this workspace has already fully used its free trial
-- for Grafana Enterprise.
workspaceDescription_freeTrialConsumed :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Bool)
workspaceDescription_freeTrialConsumed = Lens.lens (\WorkspaceDescription' {freeTrialConsumed} -> freeTrialConsumed) (\s@WorkspaceDescription' {} a -> s {freeTrialConsumed = a} :: WorkspaceDescription)

-- | If this workspace is currently in the free trial period for Grafana
-- Enterprise, this value specifies when that free trial ends.
workspaceDescription_freeTrialExpiration :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.UTCTime)
workspaceDescription_freeTrialExpiration = Lens.lens (\WorkspaceDescription' {freeTrialExpiration} -> freeTrialExpiration) (\s@WorkspaceDescription' {} a -> s {freeTrialExpiration = a} :: WorkspaceDescription) Prelude.. Lens.mapping Data._Time

-- | If this workspace has a full Grafana Enterprise license, this specifies
-- when the license ends and will need to be renewed.
workspaceDescription_licenseExpiration :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.UTCTime)
workspaceDescription_licenseExpiration = Lens.lens (\WorkspaceDescription' {licenseExpiration} -> licenseExpiration) (\s@WorkspaceDescription' {} a -> s {licenseExpiration = a} :: WorkspaceDescription) Prelude.. Lens.mapping Data._Time

-- | Specifies whether this workspace has a full Grafana Enterprise license
-- or a free trial license.
workspaceDescription_licenseType :: Lens.Lens' WorkspaceDescription (Prelude.Maybe LicenseType)
workspaceDescription_licenseType = Lens.lens (\WorkspaceDescription' {licenseType} -> licenseType) (\s@WorkspaceDescription' {} a -> s {licenseType = a} :: WorkspaceDescription)

-- | The name of the workspace.
workspaceDescription_name :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_name = Lens.lens (\WorkspaceDescription' {name} -> name) (\s@WorkspaceDescription' {} a -> s {name = a} :: WorkspaceDescription) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Web Services notification channels that Amazon Managed
-- Grafana can automatically create IAM roles and permissions for, to allow
-- Amazon Managed Grafana to use these channels.
workspaceDescription_notificationDestinations :: Lens.Lens' WorkspaceDescription (Prelude.Maybe [NotificationDestinationType])
workspaceDescription_notificationDestinations = Lens.lens (\WorkspaceDescription' {notificationDestinations} -> notificationDestinations) (\s@WorkspaceDescription' {} a -> s {notificationDestinations = a} :: WorkspaceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the IAM role that is used to access resources through
-- Organizations.
workspaceDescription_organizationRoleName :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_organizationRoleName = Lens.lens (\WorkspaceDescription' {organizationRoleName} -> organizationRoleName) (\s@WorkspaceDescription' {} a -> s {organizationRoleName = a} :: WorkspaceDescription) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the organizational units that this workspace is allowed to use
-- data sources from, if this workspace is in an account that is part of an
-- organization.
workspaceDescription_organizationalUnits :: Lens.Lens' WorkspaceDescription (Prelude.Maybe [Prelude.Text])
workspaceDescription_organizationalUnits = Lens.lens (\WorkspaceDescription' {organizationalUnits} -> organizationalUnits) (\s@WorkspaceDescription' {} a -> s {organizationalUnits = a} :: WorkspaceDescription) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

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

-- | The name of the CloudFormation stack set that is used to generate IAM
-- roles to be used for this workspace.
workspaceDescription_stackSetName :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_stackSetName = Lens.lens (\WorkspaceDescription' {stackSetName} -> stackSetName) (\s@WorkspaceDescription' {} a -> s {stackSetName = a} :: WorkspaceDescription)

-- | The list of tags associated with the workspace.
workspaceDescription_tags :: Lens.Lens' WorkspaceDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workspaceDescription_tags = Lens.lens (\WorkspaceDescription' {tags} -> tags) (\s@WorkspaceDescription' {} a -> s {tags = a} :: WorkspaceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for connecting to data sources in a private VPC
-- (Amazon Virtual Private Cloud).
workspaceDescription_vpcConfiguration :: Lens.Lens' WorkspaceDescription (Prelude.Maybe VpcConfiguration)
workspaceDescription_vpcConfiguration = Lens.lens (\WorkspaceDescription' {vpcConfiguration} -> vpcConfiguration) (\s@WorkspaceDescription' {} a -> s {vpcConfiguration = a} :: WorkspaceDescription)

-- | The IAM role that grants permissions to the Amazon Web Services
-- resources that the workspace will view data from. This role must already
-- exist.
workspaceDescription_workspaceRoleArn :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_workspaceRoleArn = Lens.lens (\WorkspaceDescription' {workspaceRoleArn} -> workspaceRoleArn) (\s@WorkspaceDescription' {} a -> s {workspaceRoleArn = a} :: WorkspaceDescription) Prelude.. Lens.mapping Data._Sensitive

-- | A structure that describes whether the workspace uses SAML, IAM Identity
-- Center, or both methods for user authentication.
workspaceDescription_authentication :: Lens.Lens' WorkspaceDescription AuthenticationSummary
workspaceDescription_authentication = Lens.lens (\WorkspaceDescription' {authentication} -> authentication) (\s@WorkspaceDescription' {} a -> s {authentication = a} :: WorkspaceDescription)

-- | The date that the workspace was created.
workspaceDescription_created :: Lens.Lens' WorkspaceDescription Prelude.UTCTime
workspaceDescription_created = Lens.lens (\WorkspaceDescription' {created} -> created) (\s@WorkspaceDescription' {} a -> s {created = a} :: WorkspaceDescription) Prelude.. Data._Time

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
workspaceDescription_modified = Lens.lens (\WorkspaceDescription' {modified} -> modified) (\s@WorkspaceDescription' {} a -> s {modified = a} :: WorkspaceDescription) Prelude.. Data._Time

-- | The current status of the workspace.
workspaceDescription_status :: Lens.Lens' WorkspaceDescription WorkspaceStatus
workspaceDescription_status = Lens.lens (\WorkspaceDescription' {status} -> status) (\s@WorkspaceDescription' {} a -> s {status = a} :: WorkspaceDescription)

instance Data.FromJSON WorkspaceDescription where
  parseJSON =
    Data.withObject
      "WorkspaceDescription"
      ( \x ->
          WorkspaceDescription'
            Prelude.<$> (x Data..:? "accountAccessType")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "freeTrialConsumed")
            Prelude.<*> (x Data..:? "freeTrialExpiration")
            Prelude.<*> (x Data..:? "licenseExpiration")
            Prelude.<*> (x Data..:? "licenseType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> ( x Data..:? "notificationDestinations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "organizationRoleName")
            Prelude.<*> ( x Data..:? "organizationalUnits"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "permissionType")
            Prelude.<*> (x Data..:? "stackSetName")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcConfiguration")
            Prelude.<*> (x Data..:? "workspaceRoleArn")
            Prelude.<*> (x Data..: "authentication")
            Prelude.<*> (x Data..: "created")
            Prelude.<*> (x Data..:? "dataSources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "endpoint")
            Prelude.<*> (x Data..: "grafanaVersion")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "modified")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable WorkspaceDescription where
  hashWithSalt _salt WorkspaceDescription' {..} =
    _salt `Prelude.hashWithSalt` accountAccessType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` freeTrialConsumed
      `Prelude.hashWithSalt` freeTrialExpiration
      `Prelude.hashWithSalt` licenseExpiration
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notificationDestinations
      `Prelude.hashWithSalt` organizationRoleName
      `Prelude.hashWithSalt` organizationalUnits
      `Prelude.hashWithSalt` permissionType
      `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` workspaceRoleArn
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` grafanaVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` modified
      `Prelude.hashWithSalt` status

instance Prelude.NFData WorkspaceDescription where
  rnf WorkspaceDescription' {..} =
    Prelude.rnf accountAccessType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf freeTrialConsumed
      `Prelude.seq` Prelude.rnf freeTrialExpiration
      `Prelude.seq` Prelude.rnf licenseExpiration
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notificationDestinations
      `Prelude.seq` Prelude.rnf organizationRoleName
      `Prelude.seq` Prelude.rnf organizationalUnits
      `Prelude.seq` Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf stackSetName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf workspaceRoleArn
      `Prelude.seq` Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf grafanaVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf modified
      `Prelude.seq` Prelude.rnf status
