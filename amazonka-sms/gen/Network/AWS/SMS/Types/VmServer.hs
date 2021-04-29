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
-- Module      : Network.AWS.SMS.Types.VmServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.VmServer where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.VmManagerType
import Network.AWS.SMS.Types.VmServerAddress

-- | Represents a VM server.
--
-- /See:/ 'newVmServer' smart constructor.
data VmServer = VmServer'
  { -- | The VM folder path in the vCenter Server virtual machine inventory tree.
    vmPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the VM manager.
    vmManagerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the VM.
    vmName :: Prelude.Maybe Prelude.Text,
    -- | The VM server location.
    vmServerAddress :: Prelude.Maybe VmServerAddress,
    -- | The type of VM management product.
    vmManagerType :: Prelude.Maybe VmManagerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VmServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vmPath', 'vmServer_vmPath' - The VM folder path in the vCenter Server virtual machine inventory tree.
--
-- 'vmManagerName', 'vmServer_vmManagerName' - The name of the VM manager.
--
-- 'vmName', 'vmServer_vmName' - The name of the VM.
--
-- 'vmServerAddress', 'vmServer_vmServerAddress' - The VM server location.
--
-- 'vmManagerType', 'vmServer_vmManagerType' - The type of VM management product.
newVmServer ::
  VmServer
newVmServer =
  VmServer'
    { vmPath = Prelude.Nothing,
      vmManagerName = Prelude.Nothing,
      vmName = Prelude.Nothing,
      vmServerAddress = Prelude.Nothing,
      vmManagerType = Prelude.Nothing
    }

-- | The VM folder path in the vCenter Server virtual machine inventory tree.
vmServer_vmPath :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmPath = Lens.lens (\VmServer' {vmPath} -> vmPath) (\s@VmServer' {} a -> s {vmPath = a} :: VmServer)

-- | The name of the VM manager.
vmServer_vmManagerName :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmManagerName = Lens.lens (\VmServer' {vmManagerName} -> vmManagerName) (\s@VmServer' {} a -> s {vmManagerName = a} :: VmServer)

-- | The name of the VM.
vmServer_vmName :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmName = Lens.lens (\VmServer' {vmName} -> vmName) (\s@VmServer' {} a -> s {vmName = a} :: VmServer)

-- | The VM server location.
vmServer_vmServerAddress :: Lens.Lens' VmServer (Prelude.Maybe VmServerAddress)
vmServer_vmServerAddress = Lens.lens (\VmServer' {vmServerAddress} -> vmServerAddress) (\s@VmServer' {} a -> s {vmServerAddress = a} :: VmServer)

-- | The type of VM management product.
vmServer_vmManagerType :: Lens.Lens' VmServer (Prelude.Maybe VmManagerType)
vmServer_vmManagerType = Lens.lens (\VmServer' {vmManagerType} -> vmManagerType) (\s@VmServer' {} a -> s {vmManagerType = a} :: VmServer)

instance Prelude.FromJSON VmServer where
  parseJSON =
    Prelude.withObject
      "VmServer"
      ( \x ->
          VmServer'
            Prelude.<$> (x Prelude..:? "vmPath")
            Prelude.<*> (x Prelude..:? "vmManagerName")
            Prelude.<*> (x Prelude..:? "vmName")
            Prelude.<*> (x Prelude..:? "vmServerAddress")
            Prelude.<*> (x Prelude..:? "vmManagerType")
      )

instance Prelude.Hashable VmServer

instance Prelude.NFData VmServer

instance Prelude.ToJSON VmServer where
  toJSON VmServer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("vmPath" Prelude..=) Prelude.<$> vmPath,
            ("vmManagerName" Prelude..=)
              Prelude.<$> vmManagerName,
            ("vmName" Prelude..=) Prelude.<$> vmName,
            ("vmServerAddress" Prelude..=)
              Prelude.<$> vmServerAddress,
            ("vmManagerType" Prelude..=)
              Prelude.<$> vmManagerType
          ]
      )
