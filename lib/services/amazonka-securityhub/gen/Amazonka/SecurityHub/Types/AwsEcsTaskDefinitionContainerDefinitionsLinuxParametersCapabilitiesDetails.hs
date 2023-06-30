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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails = AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails'
  { -- | The Linux capabilities for the container that are added to the default
    -- configuration provided by Docker. Valid values are as follows:
    --
    -- Valid values: @\"ALL\"@ | @\"AUDIT_CONTROL\"@ |@ \"AUDIT_WRITE\"@ |
    -- @\"BLOCK_SUSPEND\"@ | @\"CHOWN\"@ | @\"DAC_OVERRIDE\"@ |
    -- @\"DAC_READ_SEARCH\"@ | @\"FOWNER\"@ | @\"FSETID\"@ | @\"IPC_LOCK\"@ |
    -- @\"IPC_OWNER\"@ | @\"KILL\"@ | @\"LEASE\"@ | @\"LINUX_IMMUTABLE\"@ |
    -- @\"MAC_ADMIN\"@ |@ \"MAC_OVERRIDE\"@ | @\"MKNOD\"@ | @\"NET_ADMIN\"@ |
    -- @\"NET_BIND_SERVICE\"@ | @\"NET_BROADCAST\"@ | @\"NET_RAW\"@ |
    -- @\"SETFCAP\"@ | @\"SETGID\"@ | @\"SETPCAP\"@ | @\"SETUID\"@ |
    -- @\"SYS_ADMIN\"@ | @\"SYS_BOOT\"@ | @\"SYS_CHROOT\"@ | @\"SYS_MODULE\"@ |
    -- @\"SYS_NICE\"@ | @\"SYS_PACCT\"@ | @\"SYS_PTRACE\"@ | @\"SYS_RAWIO\"@ |
    -- @\"SYS_RESOURCE\"@ | @\"SYS_TIME\"@ | @\"SYS_TTY_CONFIG\"@ |
    -- @\"SYSLOG\"@ | @\"WAKE_ALARM\"@
    add :: Prelude.Maybe [Prelude.Text],
    -- | The Linux capabilities for the container that are dropped from the
    -- default configuration provided by Docker.
    --
    -- Valid values: @\"ALL\"@ | @\"AUDIT_CONTROL\"@ |@ \"AUDIT_WRITE\"@ |
    -- @\"BLOCK_SUSPEND\"@ | @\"CHOWN\"@ | @\"DAC_OVERRIDE\"@ |
    -- @\"DAC_READ_SEARCH\"@ | @\"FOWNER\"@ | @\"FSETID\"@ | @\"IPC_LOCK\"@ |
    -- @\"IPC_OWNER\"@ | @\"KILL\"@ | @\"LEASE\"@ | @\"LINUX_IMMUTABLE\"@ |
    -- @\"MAC_ADMIN\"@ |@ \"MAC_OVERRIDE\"@ | @\"MKNOD\"@ | @\"NET_ADMIN\"@ |
    -- @\"NET_BIND_SERVICE\"@ | @\"NET_BROADCAST\"@ | @\"NET_RAW\"@ |
    -- @\"SETFCAP\"@ | @\"SETGID\"@ | @\"SETPCAP\"@ | @\"SETUID\"@ |
    -- @\"SYS_ADMIN\"@ | @\"SYS_BOOT\"@ | @\"SYS_CHROOT\"@ | @\"SYS_MODULE\"@ |
    -- @\"SYS_NICE\"@ | @\"SYS_PACCT\"@ | @\"SYS_PTRACE\"@ | @\"SYS_RAWIO\"@ |
    -- @\"SYS_RESOURCE\"@ | @\"SYS_TIME\"@ | @\"SYS_TTY_CONFIG\"@ |
    -- @\"SYSLOG\"@ | @\"WAKE_ALARM\"@
    drop :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'add', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add' - The Linux capabilities for the container that are added to the default
-- configuration provided by Docker. Valid values are as follows:
--
-- Valid values: @\"ALL\"@ | @\"AUDIT_CONTROL\"@ |@ \"AUDIT_WRITE\"@ |
-- @\"BLOCK_SUSPEND\"@ | @\"CHOWN\"@ | @\"DAC_OVERRIDE\"@ |
-- @\"DAC_READ_SEARCH\"@ | @\"FOWNER\"@ | @\"FSETID\"@ | @\"IPC_LOCK\"@ |
-- @\"IPC_OWNER\"@ | @\"KILL\"@ | @\"LEASE\"@ | @\"LINUX_IMMUTABLE\"@ |
-- @\"MAC_ADMIN\"@ |@ \"MAC_OVERRIDE\"@ | @\"MKNOD\"@ | @\"NET_ADMIN\"@ |
-- @\"NET_BIND_SERVICE\"@ | @\"NET_BROADCAST\"@ | @\"NET_RAW\"@ |
-- @\"SETFCAP\"@ | @\"SETGID\"@ | @\"SETPCAP\"@ | @\"SETUID\"@ |
-- @\"SYS_ADMIN\"@ | @\"SYS_BOOT\"@ | @\"SYS_CHROOT\"@ | @\"SYS_MODULE\"@ |
-- @\"SYS_NICE\"@ | @\"SYS_PACCT\"@ | @\"SYS_PTRACE\"@ | @\"SYS_RAWIO\"@ |
-- @\"SYS_RESOURCE\"@ | @\"SYS_TIME\"@ | @\"SYS_TTY_CONFIG\"@ |
-- @\"SYSLOG\"@ | @\"WAKE_ALARM\"@
--
-- 'drop', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop' - The Linux capabilities for the container that are dropped from the
-- default configuration provided by Docker.
--
-- Valid values: @\"ALL\"@ | @\"AUDIT_CONTROL\"@ |@ \"AUDIT_WRITE\"@ |
-- @\"BLOCK_SUSPEND\"@ | @\"CHOWN\"@ | @\"DAC_OVERRIDE\"@ |
-- @\"DAC_READ_SEARCH\"@ | @\"FOWNER\"@ | @\"FSETID\"@ | @\"IPC_LOCK\"@ |
-- @\"IPC_OWNER\"@ | @\"KILL\"@ | @\"LEASE\"@ | @\"LINUX_IMMUTABLE\"@ |
-- @\"MAC_ADMIN\"@ |@ \"MAC_OVERRIDE\"@ | @\"MKNOD\"@ | @\"NET_ADMIN\"@ |
-- @\"NET_BIND_SERVICE\"@ | @\"NET_BROADCAST\"@ | @\"NET_RAW\"@ |
-- @\"SETFCAP\"@ | @\"SETGID\"@ | @\"SETPCAP\"@ | @\"SETUID\"@ |
-- @\"SYS_ADMIN\"@ | @\"SYS_BOOT\"@ | @\"SYS_CHROOT\"@ | @\"SYS_MODULE\"@ |
-- @\"SYS_NICE\"@ | @\"SYS_PACCT\"@ | @\"SYS_PTRACE\"@ | @\"SYS_RAWIO\"@ |
-- @\"SYS_RESOURCE\"@ | @\"SYS_TIME\"@ | @\"SYS_TTY_CONFIG\"@ |
-- @\"SYSLOG\"@ | @\"WAKE_ALARM\"@
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails =
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails'
    { add =
        Prelude.Nothing,
      drop =
        Prelude.Nothing
    }

-- | The Linux capabilities for the container that are added to the default
-- configuration provided by Docker. Valid values are as follows:
--
-- Valid values: @\"ALL\"@ | @\"AUDIT_CONTROL\"@ |@ \"AUDIT_WRITE\"@ |
-- @\"BLOCK_SUSPEND\"@ | @\"CHOWN\"@ | @\"DAC_OVERRIDE\"@ |
-- @\"DAC_READ_SEARCH\"@ | @\"FOWNER\"@ | @\"FSETID\"@ | @\"IPC_LOCK\"@ |
-- @\"IPC_OWNER\"@ | @\"KILL\"@ | @\"LEASE\"@ | @\"LINUX_IMMUTABLE\"@ |
-- @\"MAC_ADMIN\"@ |@ \"MAC_OVERRIDE\"@ | @\"MKNOD\"@ | @\"NET_ADMIN\"@ |
-- @\"NET_BIND_SERVICE\"@ | @\"NET_BROADCAST\"@ | @\"NET_RAW\"@ |
-- @\"SETFCAP\"@ | @\"SETGID\"@ | @\"SETPCAP\"@ | @\"SETUID\"@ |
-- @\"SYS_ADMIN\"@ | @\"SYS_BOOT\"@ | @\"SYS_CHROOT\"@ | @\"SYS_MODULE\"@ |
-- @\"SYS_NICE\"@ | @\"SYS_PACCT\"@ | @\"SYS_PTRACE\"@ | @\"SYS_RAWIO\"@ |
-- @\"SYS_RESOURCE\"@ | @\"SYS_TIME\"@ | @\"SYS_TTY_CONFIG\"@ |
-- @\"SYSLOG\"@ | @\"WAKE_ALARM\"@
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {add} -> add) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {} a -> s {add = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Linux capabilities for the container that are dropped from the
-- default configuration provided by Docker.
--
-- Valid values: @\"ALL\"@ | @\"AUDIT_CONTROL\"@ |@ \"AUDIT_WRITE\"@ |
-- @\"BLOCK_SUSPEND\"@ | @\"CHOWN\"@ | @\"DAC_OVERRIDE\"@ |
-- @\"DAC_READ_SEARCH\"@ | @\"FOWNER\"@ | @\"FSETID\"@ | @\"IPC_LOCK\"@ |
-- @\"IPC_OWNER\"@ | @\"KILL\"@ | @\"LEASE\"@ | @\"LINUX_IMMUTABLE\"@ |
-- @\"MAC_ADMIN\"@ |@ \"MAC_OVERRIDE\"@ | @\"MKNOD\"@ | @\"NET_ADMIN\"@ |
-- @\"NET_BIND_SERVICE\"@ | @\"NET_BROADCAST\"@ | @\"NET_RAW\"@ |
-- @\"SETFCAP\"@ | @\"SETGID\"@ | @\"SETPCAP\"@ | @\"SETUID\"@ |
-- @\"SYS_ADMIN\"@ | @\"SYS_BOOT\"@ | @\"SYS_CHROOT\"@ | @\"SYS_MODULE\"@ |
-- @\"SYS_NICE\"@ | @\"SYS_PACCT\"@ | @\"SYS_PTRACE\"@ | @\"SYS_RAWIO\"@ |
-- @\"SYS_RESOURCE\"@ | @\"SYS_TIME\"@ | @\"SYS_TTY_CONFIG\"@ |
-- @\"SYSLOG\"@ | @\"WAKE_ALARM\"@
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {drop} -> drop) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {} a -> s {drop = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails'
            Prelude.<$> (x Data..:? "Add" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Drop" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {..} =
      _salt
        `Prelude.hashWithSalt` add
        `Prelude.hashWithSalt` drop

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {..} =
      Prelude.rnf add `Prelude.seq` Prelude.rnf drop

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Add" Data..=) Prelude.<$> add,
              ("Drop" Data..=) Prelude.<$> drop
            ]
        )
