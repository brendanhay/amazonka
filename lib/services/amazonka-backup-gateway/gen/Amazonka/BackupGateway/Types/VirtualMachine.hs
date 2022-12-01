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
-- Module      : Amazonka.BackupGateway.Types.VirtualMachine
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.VirtualMachine where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A virtual machine that is on a hypervisor.
--
-- /See:/ 'newVirtualMachine' smart constructor.
data VirtualMachine = VirtualMachine'
  { -- | The name of the virtual machine.
    name :: Prelude.Maybe Prelude.Text,
    -- | The path of the virtual machine.
    path :: Prelude.Maybe Prelude.Text,
    -- | The host name of the virtual machine.
    hostName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual machine. For example,
    -- @arn:aws:backup-gateway:us-west-1:0000000000000:vm\/vm-0000ABCDEFGIJKL@.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual machine\'s hypervisor.
    hypervisorId :: Prelude.Maybe Prelude.Text,
    -- | The most recent date a virtual machine was backed up, in Unix format and
    -- UTC time.
    lastBackupDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'virtualMachine_name' - The name of the virtual machine.
--
-- 'path', 'virtualMachine_path' - The path of the virtual machine.
--
-- 'hostName', 'virtualMachine_hostName' - The host name of the virtual machine.
--
-- 'resourceArn', 'virtualMachine_resourceArn' - The Amazon Resource Name (ARN) of the virtual machine. For example,
-- @arn:aws:backup-gateway:us-west-1:0000000000000:vm\/vm-0000ABCDEFGIJKL@.
--
-- 'hypervisorId', 'virtualMachine_hypervisorId' - The ID of the virtual machine\'s hypervisor.
--
-- 'lastBackupDate', 'virtualMachine_lastBackupDate' - The most recent date a virtual machine was backed up, in Unix format and
-- UTC time.
newVirtualMachine ::
  VirtualMachine
newVirtualMachine =
  VirtualMachine'
    { name = Prelude.Nothing,
      path = Prelude.Nothing,
      hostName = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      hypervisorId = Prelude.Nothing,
      lastBackupDate = Prelude.Nothing
    }

-- | The name of the virtual machine.
virtualMachine_name :: Lens.Lens' VirtualMachine (Prelude.Maybe Prelude.Text)
virtualMachine_name = Lens.lens (\VirtualMachine' {name} -> name) (\s@VirtualMachine' {} a -> s {name = a} :: VirtualMachine)

-- | The path of the virtual machine.
virtualMachine_path :: Lens.Lens' VirtualMachine (Prelude.Maybe Prelude.Text)
virtualMachine_path = Lens.lens (\VirtualMachine' {path} -> path) (\s@VirtualMachine' {} a -> s {path = a} :: VirtualMachine)

-- | The host name of the virtual machine.
virtualMachine_hostName :: Lens.Lens' VirtualMachine (Prelude.Maybe Prelude.Text)
virtualMachine_hostName = Lens.lens (\VirtualMachine' {hostName} -> hostName) (\s@VirtualMachine' {} a -> s {hostName = a} :: VirtualMachine)

-- | The Amazon Resource Name (ARN) of the virtual machine. For example,
-- @arn:aws:backup-gateway:us-west-1:0000000000000:vm\/vm-0000ABCDEFGIJKL@.
virtualMachine_resourceArn :: Lens.Lens' VirtualMachine (Prelude.Maybe Prelude.Text)
virtualMachine_resourceArn = Lens.lens (\VirtualMachine' {resourceArn} -> resourceArn) (\s@VirtualMachine' {} a -> s {resourceArn = a} :: VirtualMachine)

-- | The ID of the virtual machine\'s hypervisor.
virtualMachine_hypervisorId :: Lens.Lens' VirtualMachine (Prelude.Maybe Prelude.Text)
virtualMachine_hypervisorId = Lens.lens (\VirtualMachine' {hypervisorId} -> hypervisorId) (\s@VirtualMachine' {} a -> s {hypervisorId = a} :: VirtualMachine)

-- | The most recent date a virtual machine was backed up, in Unix format and
-- UTC time.
virtualMachine_lastBackupDate :: Lens.Lens' VirtualMachine (Prelude.Maybe Prelude.UTCTime)
virtualMachine_lastBackupDate = Lens.lens (\VirtualMachine' {lastBackupDate} -> lastBackupDate) (\s@VirtualMachine' {} a -> s {lastBackupDate = a} :: VirtualMachine) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON VirtualMachine where
  parseJSON =
    Core.withObject
      "VirtualMachine"
      ( \x ->
          VirtualMachine'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Path")
            Prelude.<*> (x Core..:? "HostName")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "HypervisorId")
            Prelude.<*> (x Core..:? "LastBackupDate")
      )

instance Prelude.Hashable VirtualMachine where
  hashWithSalt _salt VirtualMachine' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` hostName
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` hypervisorId
      `Prelude.hashWithSalt` lastBackupDate

instance Prelude.NFData VirtualMachine where
  rnf VirtualMachine' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf hostName
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf hypervisorId
      `Prelude.seq` Prelude.rnf lastBackupDate
