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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.VmServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.VmManagerType
import Amazonka.SMS.Types.VmServerAddress

-- | Represents a VM server.
--
-- /See:/ 'newVmServer' smart constructor.
data VmServer = VmServer'
  { -- | The VM server location.
    vmServerAddress :: Prelude.Maybe VmServerAddress,
    -- | The name of the VM manager.
    vmManagerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the VM.
    vmName :: Prelude.Maybe Prelude.Text,
    -- | The type of VM management product.
    vmManagerType :: Prelude.Maybe VmManagerType,
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
-- 'vmServerAddress', 'vmServer_vmServerAddress' - The VM server location.
--
-- 'vmManagerName', 'vmServer_vmManagerName' - The name of the VM manager.
--
-- 'vmName', 'vmServer_vmName' - The name of the VM.
--
-- 'vmManagerType', 'vmServer_vmManagerType' - The type of VM management product.
--
-- 'vmPath', 'vmServer_vmPath' - The VM folder path in the vCenter Server virtual machine inventory tree.
newVmServer ::
  VmServer
newVmServer =
  VmServer'
    { vmServerAddress = Prelude.Nothing,
      vmManagerName = Prelude.Nothing,
      vmName = Prelude.Nothing,
      vmManagerType = Prelude.Nothing,
      vmPath = Prelude.Nothing
    }

-- | The VM server location.
vmServer_vmServerAddress :: Lens.Lens' VmServer (Prelude.Maybe VmServerAddress)
vmServer_vmServerAddress = Lens.lens (\VmServer' {vmServerAddress} -> vmServerAddress) (\s@VmServer' {} a -> s {vmServerAddress = a} :: VmServer)

-- | The name of the VM manager.
vmServer_vmManagerName :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmManagerName = Lens.lens (\VmServer' {vmManagerName} -> vmManagerName) (\s@VmServer' {} a -> s {vmManagerName = a} :: VmServer)

-- | The name of the VM.
vmServer_vmName :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmName = Lens.lens (\VmServer' {vmName} -> vmName) (\s@VmServer' {} a -> s {vmName = a} :: VmServer)

-- | The type of VM management product.
vmServer_vmManagerType :: Lens.Lens' VmServer (Prelude.Maybe VmManagerType)
vmServer_vmManagerType = Lens.lens (\VmServer' {vmManagerType} -> vmManagerType) (\s@VmServer' {} a -> s {vmManagerType = a} :: VmServer)

-- | The VM folder path in the vCenter Server virtual machine inventory tree.
vmServer_vmPath :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmPath = Lens.lens (\VmServer' {vmPath} -> vmPath) (\s@VmServer' {} a -> s {vmPath = a} :: VmServer)

instance Core.FromJSON VmServer where
  parseJSON =
    Core.withObject
      "VmServer"
      ( \x ->
          VmServer'
            Prelude.<$> (x Core..:? "vmServerAddress")
            Prelude.<*> (x Core..:? "vmManagerName")
            Prelude.<*> (x Core..:? "vmName")
            Prelude.<*> (x Core..:? "vmManagerType")
            Prelude.<*> (x Core..:? "vmPath")
      )

instance Prelude.Hashable VmServer where
  hashWithSalt _salt VmServer' {..} =
    _salt `Prelude.hashWithSalt` vmServerAddress
      `Prelude.hashWithSalt` vmManagerName
      `Prelude.hashWithSalt` vmName
      `Prelude.hashWithSalt` vmManagerType
      `Prelude.hashWithSalt` vmPath

instance Prelude.NFData VmServer where
  rnf VmServer' {..} =
    Prelude.rnf vmServerAddress
      `Prelude.seq` Prelude.rnf vmManagerName
      `Prelude.seq` Prelude.rnf vmName
      `Prelude.seq` Prelude.rnf vmManagerType
      `Prelude.seq` Prelude.rnf vmPath

instance Core.ToJSON VmServer where
  toJSON VmServer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("vmServerAddress" Core..=)
              Prelude.<$> vmServerAddress,
            ("vmManagerName" Core..=) Prelude.<$> vmManagerName,
            ("vmName" Core..=) Prelude.<$> vmName,
            ("vmManagerType" Core..=) Prelude.<$> vmManagerType,
            ("vmPath" Core..=) Prelude.<$> vmPath
          ]
      )
