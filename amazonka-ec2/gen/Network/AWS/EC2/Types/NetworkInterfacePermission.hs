{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InterfacePermissionType
import Network.AWS.EC2.Types.NetworkInterfacePermissionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a permission for a network interface.
--
-- /See:/ 'newNetworkInterfacePermission' smart constructor.
data NetworkInterfacePermission = NetworkInterfacePermission'
  { -- | The AWS account ID.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | Information about the state of the permission.
    permissionState :: Prelude.Maybe NetworkInterfacePermissionState,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The type of permission.
    permission :: Prelude.Maybe InterfacePermissionType,
    -- | The AWS service.
    awsService :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface permission.
    networkInterfacePermissionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      permissionState = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      permission = Prelude.Nothing,
      awsService = Prelude.Nothing,
      networkInterfacePermissionId = Prelude.Nothing
    }

-- | The AWS account ID.
networkInterfacePermission_awsAccountId :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe Prelude.Text)
networkInterfacePermission_awsAccountId = Lens.lens (\NetworkInterfacePermission' {awsAccountId} -> awsAccountId) (\s@NetworkInterfacePermission' {} a -> s {awsAccountId = a} :: NetworkInterfacePermission)

-- | Information about the state of the permission.
networkInterfacePermission_permissionState :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe NetworkInterfacePermissionState)
networkInterfacePermission_permissionState = Lens.lens (\NetworkInterfacePermission' {permissionState} -> permissionState) (\s@NetworkInterfacePermission' {} a -> s {permissionState = a} :: NetworkInterfacePermission)

-- | The ID of the network interface.
networkInterfacePermission_networkInterfaceId :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe Prelude.Text)
networkInterfacePermission_networkInterfaceId = Lens.lens (\NetworkInterfacePermission' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterfacePermission' {} a -> s {networkInterfaceId = a} :: NetworkInterfacePermission)

-- | The type of permission.
networkInterfacePermission_permission :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe InterfacePermissionType)
networkInterfacePermission_permission = Lens.lens (\NetworkInterfacePermission' {permission} -> permission) (\s@NetworkInterfacePermission' {} a -> s {permission = a} :: NetworkInterfacePermission)

-- | The AWS service.
networkInterfacePermission_awsService :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe Prelude.Text)
networkInterfacePermission_awsService = Lens.lens (\NetworkInterfacePermission' {awsService} -> awsService) (\s@NetworkInterfacePermission' {} a -> s {awsService = a} :: NetworkInterfacePermission)

-- | The ID of the network interface permission.
networkInterfacePermission_networkInterfacePermissionId :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe Prelude.Text)
networkInterfacePermission_networkInterfacePermissionId = Lens.lens (\NetworkInterfacePermission' {networkInterfacePermissionId} -> networkInterfacePermissionId) (\s@NetworkInterfacePermission' {} a -> s {networkInterfacePermissionId = a} :: NetworkInterfacePermission)

instance Prelude.FromXML NetworkInterfacePermission where
  parseXML x =
    NetworkInterfacePermission'
      Prelude.<$> (x Prelude..@? "awsAccountId")
      Prelude.<*> (x Prelude..@? "permissionState")
      Prelude.<*> (x Prelude..@? "networkInterfaceId")
      Prelude.<*> (x Prelude..@? "permission")
      Prelude.<*> (x Prelude..@? "awsService")
      Prelude.<*> (x Prelude..@? "networkInterfacePermissionId")

instance Prelude.Hashable NetworkInterfacePermission

instance Prelude.NFData NetworkInterfacePermission
