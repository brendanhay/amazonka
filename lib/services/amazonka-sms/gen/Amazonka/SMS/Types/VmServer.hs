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
-- Module      : Amazonka.SMS.Types.VmServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.VmServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.VmManagerType
import Amazonka.SMS.Types.VmServerAddress

-- | Represents a VM server.
--
-- /See:/ 'newVmServer' smart constructor.
data VmServer = VmServer'
  { -- | The name of the VM manager.
    vmManagerName :: Prelude.Maybe Prelude.Text,
    -- | The type of VM management product.
    vmManagerType :: Prelude.Maybe VmManagerType,
    -- | The VM server location.
    vmServerAddress :: Prelude.Maybe VmServerAddress,
    -- | The name of the VM.
    vmName :: Prelude.Maybe Prelude.Text,
    -- | The VM folder path in the vCenter Server virtual machine inventory tree.
    vmPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VmServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vmManagerName', 'vmServer_vmManagerName' - The name of the VM manager.
--
-- 'vmManagerType', 'vmServer_vmManagerType' - The type of VM management product.
--
-- 'vmServerAddress', 'vmServer_vmServerAddress' - The VM server location.
--
-- 'vmName', 'vmServer_vmName' - The name of the VM.
--
-- 'vmPath', 'vmServer_vmPath' - The VM folder path in the vCenter Server virtual machine inventory tree.
newVmServer ::
  VmServer
newVmServer =
  VmServer'
    { vmManagerName = Prelude.Nothing,
      vmManagerType = Prelude.Nothing,
      vmServerAddress = Prelude.Nothing,
      vmName = Prelude.Nothing,
      vmPath = Prelude.Nothing
    }

-- | The name of the VM manager.
vmServer_vmManagerName :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmManagerName = Lens.lens (\VmServer' {vmManagerName} -> vmManagerName) (\s@VmServer' {} a -> s {vmManagerName = a} :: VmServer)

-- | The type of VM management product.
vmServer_vmManagerType :: Lens.Lens' VmServer (Prelude.Maybe VmManagerType)
vmServer_vmManagerType = Lens.lens (\VmServer' {vmManagerType} -> vmManagerType) (\s@VmServer' {} a -> s {vmManagerType = a} :: VmServer)

-- | The VM server location.
vmServer_vmServerAddress :: Lens.Lens' VmServer (Prelude.Maybe VmServerAddress)
vmServer_vmServerAddress = Lens.lens (\VmServer' {vmServerAddress} -> vmServerAddress) (\s@VmServer' {} a -> s {vmServerAddress = a} :: VmServer)

-- | The name of the VM.
vmServer_vmName :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmName = Lens.lens (\VmServer' {vmName} -> vmName) (\s@VmServer' {} a -> s {vmName = a} :: VmServer)

-- | The VM folder path in the vCenter Server virtual machine inventory tree.
vmServer_vmPath :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmPath = Lens.lens (\VmServer' {vmPath} -> vmPath) (\s@VmServer' {} a -> s {vmPath = a} :: VmServer)

instance Core.FromJSON VmServer where
  parseJSON =
    Core.withObject
      "VmServer"
      ( \x ->
          VmServer'
            Prelude.<$> (x Core..:? "vmManagerName")
            Prelude.<*> (x Core..:? "vmManagerType")
            Prelude.<*> (x Core..:? "vmServerAddress")
            Prelude.<*> (x Core..:? "vmName")
            Prelude.<*> (x Core..:? "vmPath")
      )

instance Prelude.Hashable VmServer

instance Prelude.NFData VmServer

instance Core.ToJSON VmServer where
  toJSON VmServer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("vmManagerName" Core..=) Prelude.<$> vmManagerName,
            ("vmManagerType" Core..=) Prelude.<$> vmManagerType,
            ("vmServerAddress" Core..=)
              Prelude.<$> vmServerAddress,
            ("vmName" Core..=) Prelude.<$> vmName,
            ("vmPath" Core..=) Prelude.<$> vmPath
          ]
      )
