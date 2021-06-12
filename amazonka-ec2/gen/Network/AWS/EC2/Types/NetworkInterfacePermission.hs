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
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePermission where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InterfacePermissionType
import Network.AWS.EC2.Types.NetworkInterfacePermissionState
import qualified Network.AWS.Lens as Lens

-- | Describes a permission for a network interface.
--
-- /See:/ 'newNetworkInterfacePermission' smart constructor.
data NetworkInterfacePermission = NetworkInterfacePermission'
  { -- | The AWS account ID.
    awsAccountId :: Core.Maybe Core.Text,
    -- | Information about the state of the permission.
    permissionState :: Core.Maybe NetworkInterfacePermissionState,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The type of permission.
    permission :: Core.Maybe InterfacePermissionType,
    -- | The AWS service.
    awsService :: Core.Maybe Core.Text,
    -- | The ID of the network interface permission.
    networkInterfacePermissionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkInterfacePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'networkInterfacePermission_awsAccountId' - The AWS account ID.
--
-- 'permissionState', 'networkInterfacePermission_permissionState' - Information about the state of the permission.
--
-- 'networkInterfaceId', 'networkInterfacePermission_networkInterfaceId' - The ID of the network interface.
--
-- 'permission', 'networkInterfacePermission_permission' - The type of permission.
--
-- 'awsService', 'networkInterfacePermission_awsService' - The AWS service.
--
-- 'networkInterfacePermissionId', 'networkInterfacePermission_networkInterfacePermissionId' - The ID of the network interface permission.
newNetworkInterfacePermission ::
  NetworkInterfacePermission
newNetworkInterfacePermission =
  NetworkInterfacePermission'
    { awsAccountId =
        Core.Nothing,
      permissionState = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      permission = Core.Nothing,
      awsService = Core.Nothing,
      networkInterfacePermissionId = Core.Nothing
    }

-- | The AWS account ID.
networkInterfacePermission_awsAccountId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Core.Text)
networkInterfacePermission_awsAccountId = Lens.lens (\NetworkInterfacePermission' {awsAccountId} -> awsAccountId) (\s@NetworkInterfacePermission' {} a -> s {awsAccountId = a} :: NetworkInterfacePermission)

-- | Information about the state of the permission.
networkInterfacePermission_permissionState :: Lens.Lens' NetworkInterfacePermission (Core.Maybe NetworkInterfacePermissionState)
networkInterfacePermission_permissionState = Lens.lens (\NetworkInterfacePermission' {permissionState} -> permissionState) (\s@NetworkInterfacePermission' {} a -> s {permissionState = a} :: NetworkInterfacePermission)

-- | The ID of the network interface.
networkInterfacePermission_networkInterfaceId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Core.Text)
networkInterfacePermission_networkInterfaceId = Lens.lens (\NetworkInterfacePermission' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterfacePermission' {} a -> s {networkInterfaceId = a} :: NetworkInterfacePermission)

-- | The type of permission.
networkInterfacePermission_permission :: Lens.Lens' NetworkInterfacePermission (Core.Maybe InterfacePermissionType)
networkInterfacePermission_permission = Lens.lens (\NetworkInterfacePermission' {permission} -> permission) (\s@NetworkInterfacePermission' {} a -> s {permission = a} :: NetworkInterfacePermission)

-- | The AWS service.
networkInterfacePermission_awsService :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Core.Text)
networkInterfacePermission_awsService = Lens.lens (\NetworkInterfacePermission' {awsService} -> awsService) (\s@NetworkInterfacePermission' {} a -> s {awsService = a} :: NetworkInterfacePermission)

-- | The ID of the network interface permission.
networkInterfacePermission_networkInterfacePermissionId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Core.Text)
networkInterfacePermission_networkInterfacePermissionId = Lens.lens (\NetworkInterfacePermission' {networkInterfacePermissionId} -> networkInterfacePermissionId) (\s@NetworkInterfacePermission' {} a -> s {networkInterfacePermissionId = a} :: NetworkInterfacePermission)

instance Core.FromXML NetworkInterfacePermission where
  parseXML x =
    NetworkInterfacePermission'
      Core.<$> (x Core..@? "awsAccountId")
      Core.<*> (x Core..@? "permissionState")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "permission")
      Core.<*> (x Core..@? "awsService")
      Core.<*> (x Core..@? "networkInterfacePermissionId")

instance Core.Hashable NetworkInterfacePermission

instance Core.NFData NetworkInterfacePermission
