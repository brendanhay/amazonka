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
-- Module      : Amazonka.WorkSpaces.Types.DefaultWorkspaceCreationProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.DefaultWorkspaceCreationProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the default values that are used to create WorkSpaces. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces>.
--
-- /See:/ 'newDefaultWorkspaceCreationProperties' smart constructor.
data DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties'
  { -- | Specifies whether WorkSpace users are local administrators on their
    -- WorkSpaces.
    userEnabledAsLocalAdministrator :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to automatically assign an Elastic public IP address
    -- to WorkSpaces in this directory by default. If enabled, the Elastic
    -- public IP address allows outbound internet access from your WorkSpaces
    -- when you’re using an internet gateway in the Amazon VPC in which your
    -- WorkSpaces are located. If you\'re using a Network Address Translation
    -- (NAT) gateway for outbound internet access from your VPC, or if your
    -- WorkSpaces are in public subnets and you manually assign them Elastic IP
    -- addresses, you should disable this setting. This setting applies to new
    -- WorkSpaces that you launch or to existing WorkSpaces that you rebuild.
    -- For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces>.
    enableInternetAccess :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the directory is enabled for Amazon WorkDocs.
    enableWorkDocs :: Prelude.Maybe Prelude.Bool,
    -- | The organizational unit (OU) in the directory for the WorkSpace machine
    -- accounts.
    defaultOu :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether maintenance mode is enabled for WorkSpaces. For more
    -- information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance>.
    enableMaintenanceMode :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the default security group to apply to WorkSpaces when
    -- they are created. For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces>.
    customSecurityGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultWorkspaceCreationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userEnabledAsLocalAdministrator', 'defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator' - Specifies whether WorkSpace users are local administrators on their
-- WorkSpaces.
--
-- 'enableInternetAccess', 'defaultWorkspaceCreationProperties_enableInternetAccess' - Specifies whether to automatically assign an Elastic public IP address
-- to WorkSpaces in this directory by default. If enabled, the Elastic
-- public IP address allows outbound internet access from your WorkSpaces
-- when you’re using an internet gateway in the Amazon VPC in which your
-- WorkSpaces are located. If you\'re using a Network Address Translation
-- (NAT) gateway for outbound internet access from your VPC, or if your
-- WorkSpaces are in public subnets and you manually assign them Elastic IP
-- addresses, you should disable this setting. This setting applies to new
-- WorkSpaces that you launch or to existing WorkSpaces that you rebuild.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces>.
--
-- 'enableWorkDocs', 'defaultWorkspaceCreationProperties_enableWorkDocs' - Specifies whether the directory is enabled for Amazon WorkDocs.
--
-- 'defaultOu', 'defaultWorkspaceCreationProperties_defaultOu' - The organizational unit (OU) in the directory for the WorkSpace machine
-- accounts.
--
-- 'enableMaintenanceMode', 'defaultWorkspaceCreationProperties_enableMaintenanceMode' - Specifies whether maintenance mode is enabled for WorkSpaces. For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance>.
--
-- 'customSecurityGroupId', 'defaultWorkspaceCreationProperties_customSecurityGroupId' - The identifier of the default security group to apply to WorkSpaces when
-- they are created. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces>.
newDefaultWorkspaceCreationProperties ::
  DefaultWorkspaceCreationProperties
newDefaultWorkspaceCreationProperties =
  DefaultWorkspaceCreationProperties'
    { userEnabledAsLocalAdministrator =
        Prelude.Nothing,
      enableInternetAccess = Prelude.Nothing,
      enableWorkDocs = Prelude.Nothing,
      defaultOu = Prelude.Nothing,
      enableMaintenanceMode = Prelude.Nothing,
      customSecurityGroupId = Prelude.Nothing
    }

-- | Specifies whether WorkSpace users are local administrators on their
-- WorkSpaces.
defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator :: Lens.Lens' DefaultWorkspaceCreationProperties (Prelude.Maybe Prelude.Bool)
defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator = Lens.lens (\DefaultWorkspaceCreationProperties' {userEnabledAsLocalAdministrator} -> userEnabledAsLocalAdministrator) (\s@DefaultWorkspaceCreationProperties' {} a -> s {userEnabledAsLocalAdministrator = a} :: DefaultWorkspaceCreationProperties)

-- | Specifies whether to automatically assign an Elastic public IP address
-- to WorkSpaces in this directory by default. If enabled, the Elastic
-- public IP address allows outbound internet access from your WorkSpaces
-- when you’re using an internet gateway in the Amazon VPC in which your
-- WorkSpaces are located. If you\'re using a Network Address Translation
-- (NAT) gateway for outbound internet access from your VPC, or if your
-- WorkSpaces are in public subnets and you manually assign them Elastic IP
-- addresses, you should disable this setting. This setting applies to new
-- WorkSpaces that you launch or to existing WorkSpaces that you rebuild.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces>.
defaultWorkspaceCreationProperties_enableInternetAccess :: Lens.Lens' DefaultWorkspaceCreationProperties (Prelude.Maybe Prelude.Bool)
defaultWorkspaceCreationProperties_enableInternetAccess = Lens.lens (\DefaultWorkspaceCreationProperties' {enableInternetAccess} -> enableInternetAccess) (\s@DefaultWorkspaceCreationProperties' {} a -> s {enableInternetAccess = a} :: DefaultWorkspaceCreationProperties)

-- | Specifies whether the directory is enabled for Amazon WorkDocs.
defaultWorkspaceCreationProperties_enableWorkDocs :: Lens.Lens' DefaultWorkspaceCreationProperties (Prelude.Maybe Prelude.Bool)
defaultWorkspaceCreationProperties_enableWorkDocs = Lens.lens (\DefaultWorkspaceCreationProperties' {enableWorkDocs} -> enableWorkDocs) (\s@DefaultWorkspaceCreationProperties' {} a -> s {enableWorkDocs = a} :: DefaultWorkspaceCreationProperties)

-- | The organizational unit (OU) in the directory for the WorkSpace machine
-- accounts.
defaultWorkspaceCreationProperties_defaultOu :: Lens.Lens' DefaultWorkspaceCreationProperties (Prelude.Maybe Prelude.Text)
defaultWorkspaceCreationProperties_defaultOu = Lens.lens (\DefaultWorkspaceCreationProperties' {defaultOu} -> defaultOu) (\s@DefaultWorkspaceCreationProperties' {} a -> s {defaultOu = a} :: DefaultWorkspaceCreationProperties)

-- | Specifies whether maintenance mode is enabled for WorkSpaces. For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance>.
defaultWorkspaceCreationProperties_enableMaintenanceMode :: Lens.Lens' DefaultWorkspaceCreationProperties (Prelude.Maybe Prelude.Bool)
defaultWorkspaceCreationProperties_enableMaintenanceMode = Lens.lens (\DefaultWorkspaceCreationProperties' {enableMaintenanceMode} -> enableMaintenanceMode) (\s@DefaultWorkspaceCreationProperties' {} a -> s {enableMaintenanceMode = a} :: DefaultWorkspaceCreationProperties)

-- | The identifier of the default security group to apply to WorkSpaces when
-- they are created. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces>.
defaultWorkspaceCreationProperties_customSecurityGroupId :: Lens.Lens' DefaultWorkspaceCreationProperties (Prelude.Maybe Prelude.Text)
defaultWorkspaceCreationProperties_customSecurityGroupId = Lens.lens (\DefaultWorkspaceCreationProperties' {customSecurityGroupId} -> customSecurityGroupId) (\s@DefaultWorkspaceCreationProperties' {} a -> s {customSecurityGroupId = a} :: DefaultWorkspaceCreationProperties)

instance
  Data.FromJSON
    DefaultWorkspaceCreationProperties
  where
  parseJSON =
    Data.withObject
      "DefaultWorkspaceCreationProperties"
      ( \x ->
          DefaultWorkspaceCreationProperties'
            Prelude.<$> (x Data..:? "UserEnabledAsLocalAdministrator")
            Prelude.<*> (x Data..:? "EnableInternetAccess")
            Prelude.<*> (x Data..:? "EnableWorkDocs")
            Prelude.<*> (x Data..:? "DefaultOu")
            Prelude.<*> (x Data..:? "EnableMaintenanceMode")
            Prelude.<*> (x Data..:? "CustomSecurityGroupId")
      )

instance
  Prelude.Hashable
    DefaultWorkspaceCreationProperties
  where
  hashWithSalt
    _salt
    DefaultWorkspaceCreationProperties' {..} =
      _salt
        `Prelude.hashWithSalt` userEnabledAsLocalAdministrator
        `Prelude.hashWithSalt` enableInternetAccess
        `Prelude.hashWithSalt` enableWorkDocs
        `Prelude.hashWithSalt` defaultOu
        `Prelude.hashWithSalt` enableMaintenanceMode
        `Prelude.hashWithSalt` customSecurityGroupId

instance
  Prelude.NFData
    DefaultWorkspaceCreationProperties
  where
  rnf DefaultWorkspaceCreationProperties' {..} =
    Prelude.rnf userEnabledAsLocalAdministrator
      `Prelude.seq` Prelude.rnf enableInternetAccess
      `Prelude.seq` Prelude.rnf enableWorkDocs
      `Prelude.seq` Prelude.rnf defaultOu
      `Prelude.seq` Prelude.rnf enableMaintenanceMode
      `Prelude.seq` Prelude.rnf customSecurityGroupId
