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
-- Module      : Amazonka.BackupGateway.Types.VirtualMachineDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.VirtualMachineDetails where

import Amazonka.BackupGateway.Types.VmwareTag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Your @VirtualMachine@ objects, ordered by their Amazon Resource Names
-- (ARNs).
--
-- /See:/ 'newVirtualMachineDetails' smart constructor.
data VirtualMachineDetails = VirtualMachineDetails'
  { -- | The host name of the virtual machine.
    hostName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual machine\'s hypervisor.
    hypervisorId :: Prelude.Maybe Prelude.Text,
    -- | The most recent date a virtual machine was backed up, in Unix format and
    -- UTC time.
    lastBackupDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the virtual machine.
    name :: Prelude.Maybe Prelude.Text,
    -- | The path of the virtual machine.
    path :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual machine. For example,
    -- @arn:aws:backup-gateway:us-west-1:0000000000000:vm\/vm-0000ABCDEFGIJKL@.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | These are the details of the VMware tags associated with the specified
    -- virtual machine.
    vmwareTags :: Prelude.Maybe [VmwareTag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualMachineDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostName', 'virtualMachineDetails_hostName' - The host name of the virtual machine.
--
-- 'hypervisorId', 'virtualMachineDetails_hypervisorId' - The ID of the virtual machine\'s hypervisor.
--
-- 'lastBackupDate', 'virtualMachineDetails_lastBackupDate' - The most recent date a virtual machine was backed up, in Unix format and
-- UTC time.
--
-- 'name', 'virtualMachineDetails_name' - The name of the virtual machine.
--
-- 'path', 'virtualMachineDetails_path' - The path of the virtual machine.
--
-- 'resourceArn', 'virtualMachineDetails_resourceArn' - The Amazon Resource Name (ARN) of the virtual machine. For example,
-- @arn:aws:backup-gateway:us-west-1:0000000000000:vm\/vm-0000ABCDEFGIJKL@.
--
-- 'vmwareTags', 'virtualMachineDetails_vmwareTags' - These are the details of the VMware tags associated with the specified
-- virtual machine.
newVirtualMachineDetails ::
  VirtualMachineDetails
newVirtualMachineDetails =
  VirtualMachineDetails'
    { hostName = Prelude.Nothing,
      hypervisorId = Prelude.Nothing,
      lastBackupDate = Prelude.Nothing,
      name = Prelude.Nothing,
      path = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      vmwareTags = Prelude.Nothing
    }

-- | The host name of the virtual machine.
virtualMachineDetails_hostName :: Lens.Lens' VirtualMachineDetails (Prelude.Maybe Prelude.Text)
virtualMachineDetails_hostName = Lens.lens (\VirtualMachineDetails' {hostName} -> hostName) (\s@VirtualMachineDetails' {} a -> s {hostName = a} :: VirtualMachineDetails)

-- | The ID of the virtual machine\'s hypervisor.
virtualMachineDetails_hypervisorId :: Lens.Lens' VirtualMachineDetails (Prelude.Maybe Prelude.Text)
virtualMachineDetails_hypervisorId = Lens.lens (\VirtualMachineDetails' {hypervisorId} -> hypervisorId) (\s@VirtualMachineDetails' {} a -> s {hypervisorId = a} :: VirtualMachineDetails)

-- | The most recent date a virtual machine was backed up, in Unix format and
-- UTC time.
virtualMachineDetails_lastBackupDate :: Lens.Lens' VirtualMachineDetails (Prelude.Maybe Prelude.UTCTime)
virtualMachineDetails_lastBackupDate = Lens.lens (\VirtualMachineDetails' {lastBackupDate} -> lastBackupDate) (\s@VirtualMachineDetails' {} a -> s {lastBackupDate = a} :: VirtualMachineDetails) Prelude.. Lens.mapping Data._Time

-- | The name of the virtual machine.
virtualMachineDetails_name :: Lens.Lens' VirtualMachineDetails (Prelude.Maybe Prelude.Text)
virtualMachineDetails_name = Lens.lens (\VirtualMachineDetails' {name} -> name) (\s@VirtualMachineDetails' {} a -> s {name = a} :: VirtualMachineDetails)

-- | The path of the virtual machine.
virtualMachineDetails_path :: Lens.Lens' VirtualMachineDetails (Prelude.Maybe Prelude.Text)
virtualMachineDetails_path = Lens.lens (\VirtualMachineDetails' {path} -> path) (\s@VirtualMachineDetails' {} a -> s {path = a} :: VirtualMachineDetails)

-- | The Amazon Resource Name (ARN) of the virtual machine. For example,
-- @arn:aws:backup-gateway:us-west-1:0000000000000:vm\/vm-0000ABCDEFGIJKL@.
virtualMachineDetails_resourceArn :: Lens.Lens' VirtualMachineDetails (Prelude.Maybe Prelude.Text)
virtualMachineDetails_resourceArn = Lens.lens (\VirtualMachineDetails' {resourceArn} -> resourceArn) (\s@VirtualMachineDetails' {} a -> s {resourceArn = a} :: VirtualMachineDetails)

-- | These are the details of the VMware tags associated with the specified
-- virtual machine.
virtualMachineDetails_vmwareTags :: Lens.Lens' VirtualMachineDetails (Prelude.Maybe [VmwareTag])
virtualMachineDetails_vmwareTags = Lens.lens (\VirtualMachineDetails' {vmwareTags} -> vmwareTags) (\s@VirtualMachineDetails' {} a -> s {vmwareTags = a} :: VirtualMachineDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VirtualMachineDetails where
  parseJSON =
    Data.withObject
      "VirtualMachineDetails"
      ( \x ->
          VirtualMachineDetails'
            Prelude.<$> (x Data..:? "HostName")
            Prelude.<*> (x Data..:? "HypervisorId")
            Prelude.<*> (x Data..:? "LastBackupDate")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "VmwareTags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable VirtualMachineDetails where
  hashWithSalt _salt VirtualMachineDetails' {..} =
    _salt
      `Prelude.hashWithSalt` hostName
      `Prelude.hashWithSalt` hypervisorId
      `Prelude.hashWithSalt` lastBackupDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` vmwareTags

instance Prelude.NFData VirtualMachineDetails where
  rnf VirtualMachineDetails' {..} =
    Prelude.rnf hostName
      `Prelude.seq` Prelude.rnf hypervisorId
      `Prelude.seq` Prelude.rnf lastBackupDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf vmwareTags
