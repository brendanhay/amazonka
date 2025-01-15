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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.VmServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | The name of the VM.
    vmName :: Prelude.Maybe Prelude.Text,
    -- | The VM folder path in the vCenter Server virtual machine inventory tree.
    vmPath :: Prelude.Maybe Prelude.Text,
    -- | The VM server location.
    vmServerAddress :: Prelude.Maybe VmServerAddress
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
-- 'vmName', 'vmServer_vmName' - The name of the VM.
--
-- 'vmPath', 'vmServer_vmPath' - The VM folder path in the vCenter Server virtual machine inventory tree.
--
-- 'vmServerAddress', 'vmServer_vmServerAddress' - The VM server location.
newVmServer ::
  VmServer
newVmServer =
  VmServer'
    { vmManagerName = Prelude.Nothing,
      vmManagerType = Prelude.Nothing,
      vmName = Prelude.Nothing,
      vmPath = Prelude.Nothing,
      vmServerAddress = Prelude.Nothing
    }

-- | The name of the VM manager.
vmServer_vmManagerName :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmManagerName = Lens.lens (\VmServer' {vmManagerName} -> vmManagerName) (\s@VmServer' {} a -> s {vmManagerName = a} :: VmServer)

-- | The type of VM management product.
vmServer_vmManagerType :: Lens.Lens' VmServer (Prelude.Maybe VmManagerType)
vmServer_vmManagerType = Lens.lens (\VmServer' {vmManagerType} -> vmManagerType) (\s@VmServer' {} a -> s {vmManagerType = a} :: VmServer)

-- | The name of the VM.
vmServer_vmName :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmName = Lens.lens (\VmServer' {vmName} -> vmName) (\s@VmServer' {} a -> s {vmName = a} :: VmServer)

-- | The VM folder path in the vCenter Server virtual machine inventory tree.
vmServer_vmPath :: Lens.Lens' VmServer (Prelude.Maybe Prelude.Text)
vmServer_vmPath = Lens.lens (\VmServer' {vmPath} -> vmPath) (\s@VmServer' {} a -> s {vmPath = a} :: VmServer)

-- | The VM server location.
vmServer_vmServerAddress :: Lens.Lens' VmServer (Prelude.Maybe VmServerAddress)
vmServer_vmServerAddress = Lens.lens (\VmServer' {vmServerAddress} -> vmServerAddress) (\s@VmServer' {} a -> s {vmServerAddress = a} :: VmServer)

instance Data.FromJSON VmServer where
  parseJSON =
    Data.withObject
      "VmServer"
      ( \x ->
          VmServer'
            Prelude.<$> (x Data..:? "vmManagerName")
            Prelude.<*> (x Data..:? "vmManagerType")
            Prelude.<*> (x Data..:? "vmName")
            Prelude.<*> (x Data..:? "vmPath")
            Prelude.<*> (x Data..:? "vmServerAddress")
      )

instance Prelude.Hashable VmServer where
  hashWithSalt _salt VmServer' {..} =
    _salt
      `Prelude.hashWithSalt` vmManagerName
      `Prelude.hashWithSalt` vmManagerType
      `Prelude.hashWithSalt` vmName
      `Prelude.hashWithSalt` vmPath
      `Prelude.hashWithSalt` vmServerAddress

instance Prelude.NFData VmServer where
  rnf VmServer' {..} =
    Prelude.rnf vmManagerName `Prelude.seq`
      Prelude.rnf vmManagerType `Prelude.seq`
        Prelude.rnf vmName `Prelude.seq`
          Prelude.rnf vmPath `Prelude.seq`
            Prelude.rnf vmServerAddress

instance Data.ToJSON VmServer where
  toJSON VmServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("vmManagerName" Data..=) Prelude.<$> vmManagerName,
            ("vmManagerType" Data..=) Prelude.<$> vmManagerType,
            ("vmName" Data..=) Prelude.<$> vmName,
            ("vmPath" Data..=) Prelude.<$> vmPath,
            ("vmServerAddress" Data..=)
              Prelude.<$> vmServerAddress
          ]
      )
