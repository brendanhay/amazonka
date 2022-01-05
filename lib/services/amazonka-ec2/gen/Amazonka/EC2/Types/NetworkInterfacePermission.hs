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
-- Module      : Amazonka.EC2.Types.NetworkInterfacePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfacePermission where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InterfacePermissionType
import Amazonka.EC2.Types.NetworkInterfacePermissionState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a permission for a network interface.
--
-- /See:/ 'newNetworkInterfacePermission' smart constructor.
data NetworkInterfacePermission = NetworkInterfacePermission'
  { -- | Information about the state of the permission.
    permissionState :: Prelude.Maybe NetworkInterfacePermissionState,
    -- | The ID of the network interface permission.
    networkInterfacePermissionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Service.
    awsService :: Prelude.Maybe Prelude.Text,
    -- | The type of permission.
    permission :: Prelude.Maybe InterfacePermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfacePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionState', 'networkInterfacePermission_permissionState' - Information about the state of the permission.
--
-- 'networkInterfacePermissionId', 'networkInterfacePermission_networkInterfacePermissionId' - The ID of the network interface permission.
--
-- 'networkInterfaceId', 'networkInterfacePermission_networkInterfaceId' - The ID of the network interface.
--
-- 'awsAccountId', 'networkInterfacePermission_awsAccountId' - The Amazon Web Services account ID.
--
-- 'awsService', 'networkInterfacePermission_awsService' - The Amazon Web Service.
--
-- 'permission', 'networkInterfacePermission_permission' - The type of permission.
newNetworkInterfacePermission ::
  NetworkInterfacePermission
newNetworkInterfacePermission =
  NetworkInterfacePermission'
    { permissionState =
        Prelude.Nothing,
      networkInterfacePermissionId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      awsService = Prelude.Nothing,
      permission = Prelude.Nothing
    }

-- | Information about the state of the permission.
networkInterfacePermission_permissionState :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe NetworkInterfacePermissionState)
networkInterfacePermission_permissionState = Lens.lens (\NetworkInterfacePermission' {permissionState} -> permissionState) (\s@NetworkInterfacePermission' {} a -> s {permissionState = a} :: NetworkInterfacePermission)

-- | The ID of the network interface permission.
networkInterfacePermission_networkInterfacePermissionId :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe Prelude.Text)
networkInterfacePermission_networkInterfacePermissionId = Lens.lens (\NetworkInterfacePermission' {networkInterfacePermissionId} -> networkInterfacePermissionId) (\s@NetworkInterfacePermission' {} a -> s {networkInterfacePermissionId = a} :: NetworkInterfacePermission)

-- | The ID of the network interface.
networkInterfacePermission_networkInterfaceId :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe Prelude.Text)
networkInterfacePermission_networkInterfaceId = Lens.lens (\NetworkInterfacePermission' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterfacePermission' {} a -> s {networkInterfaceId = a} :: NetworkInterfacePermission)

-- | The Amazon Web Services account ID.
networkInterfacePermission_awsAccountId :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe Prelude.Text)
networkInterfacePermission_awsAccountId = Lens.lens (\NetworkInterfacePermission' {awsAccountId} -> awsAccountId) (\s@NetworkInterfacePermission' {} a -> s {awsAccountId = a} :: NetworkInterfacePermission)

-- | The Amazon Web Service.
networkInterfacePermission_awsService :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe Prelude.Text)
networkInterfacePermission_awsService = Lens.lens (\NetworkInterfacePermission' {awsService} -> awsService) (\s@NetworkInterfacePermission' {} a -> s {awsService = a} :: NetworkInterfacePermission)

-- | The type of permission.
networkInterfacePermission_permission :: Lens.Lens' NetworkInterfacePermission (Prelude.Maybe InterfacePermissionType)
networkInterfacePermission_permission = Lens.lens (\NetworkInterfacePermission' {permission} -> permission) (\s@NetworkInterfacePermission' {} a -> s {permission = a} :: NetworkInterfacePermission)

instance Core.FromXML NetworkInterfacePermission where
  parseXML x =
    NetworkInterfacePermission'
      Prelude.<$> (x Core..@? "permissionState")
      Prelude.<*> (x Core..@? "networkInterfacePermissionId")
      Prelude.<*> (x Core..@? "networkInterfaceId")
      Prelude.<*> (x Core..@? "awsAccountId")
      Prelude.<*> (x Core..@? "awsService")
      Prelude.<*> (x Core..@? "permission")

instance Prelude.Hashable NetworkInterfacePermission where
  hashWithSalt _salt NetworkInterfacePermission' {..} =
    _salt `Prelude.hashWithSalt` permissionState
      `Prelude.hashWithSalt` networkInterfacePermissionId
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsService
      `Prelude.hashWithSalt` permission

instance Prelude.NFData NetworkInterfacePermission where
  rnf NetworkInterfacePermission' {..} =
    Prelude.rnf permissionState
      `Prelude.seq` Prelude.rnf networkInterfacePermissionId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf awsService
      `Prelude.seq` Prelude.rnf permission
