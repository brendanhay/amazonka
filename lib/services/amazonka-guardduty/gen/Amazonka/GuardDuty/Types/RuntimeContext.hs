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
-- Module      : Amazonka.GuardDuty.Types.RuntimeContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.RuntimeContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ProcessDetails
import qualified Amazonka.Prelude as Prelude

-- | Additional information about the suspicious activity.
--
-- /See:/ 'newRuntimeContext' smart constructor.
data RuntimeContext = RuntimeContext'
  { -- | Represents the communication protocol associated with the address. For
    -- example, the address family @AF_INET@ is used for IP version of 4
    -- protocol.
    addressFamily :: Prelude.Maybe Prelude.Text,
    -- | Represents the type of mounted fileSystem.
    fileSystemType :: Prelude.Maybe Prelude.Text,
    -- | Represents options that control the behavior of a runtime operation or
    -- action. For example, a filesystem mount operation may contain a
    -- read-only flag.
    flags :: Prelude.Maybe [Prelude.Text],
    -- | Specifies a particular protocol within the address family. Usually there
    -- is a single protocol in address families. For example, the address
    -- family @AF_INET@ only has the IP protocol.
    ianaProtocolNumber :: Prelude.Maybe Prelude.Int,
    -- | The value of the LD_PRELOAD environment variable.
    ldPreloadValue :: Prelude.Maybe Prelude.Text,
    -- | The path to the new library that was loaded.
    libraryPath :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Region of a process\'s address space such as stack and
    -- heap.
    memoryRegions :: Prelude.Maybe [Prelude.Text],
    -- | The timestamp at which the process modified the current process. The
    -- timestamp is in UTC date string format.
    modifiedAt :: Prelude.Maybe Data.POSIX,
    -- | Information about the process that modified the current process. This is
    -- available for multiple finding types.
    modifyingProcess :: Prelude.Maybe ProcessDetails,
    -- | The path to the module loaded into the kernel.
    moduleFilePath :: Prelude.Maybe Prelude.Text,
    -- | The name of the module loaded into the kernel.
    moduleName :: Prelude.Maybe Prelude.Text,
    -- | The @SHA256@ hash of the module.
    moduleSha256 :: Prelude.Maybe Prelude.Text,
    -- | The path on the host that is mounted by the container.
    mountSource :: Prelude.Maybe Prelude.Text,
    -- | The path in the container that is mapped to the host directory.
    mountTarget :: Prelude.Maybe Prelude.Text,
    -- | The path in the container that modified the release agent file.
    releaseAgentPath :: Prelude.Maybe Prelude.Text,
    -- | The path to the leveraged @runc@ implementation.
    runcBinaryPath :: Prelude.Maybe Prelude.Text,
    -- | The path to the script that was executed.
    scriptPath :: Prelude.Maybe Prelude.Text,
    -- | The path to the modified shell history file.
    shellHistoryFilePath :: Prelude.Maybe Prelude.Text,
    -- | The path to the docket socket that was accessed.
    socketPath :: Prelude.Maybe Prelude.Text,
    -- | Information about the process that had its memory overwritten by the
    -- current process.
    targetProcess :: Prelude.Maybe ProcessDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimeContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressFamily', 'runtimeContext_addressFamily' - Represents the communication protocol associated with the address. For
-- example, the address family @AF_INET@ is used for IP version of 4
-- protocol.
--
-- 'fileSystemType', 'runtimeContext_fileSystemType' - Represents the type of mounted fileSystem.
--
-- 'flags', 'runtimeContext_flags' - Represents options that control the behavior of a runtime operation or
-- action. For example, a filesystem mount operation may contain a
-- read-only flag.
--
-- 'ianaProtocolNumber', 'runtimeContext_ianaProtocolNumber' - Specifies a particular protocol within the address family. Usually there
-- is a single protocol in address families. For example, the address
-- family @AF_INET@ only has the IP protocol.
--
-- 'ldPreloadValue', 'runtimeContext_ldPreloadValue' - The value of the LD_PRELOAD environment variable.
--
-- 'libraryPath', 'runtimeContext_libraryPath' - The path to the new library that was loaded.
--
-- 'memoryRegions', 'runtimeContext_memoryRegions' - Specifies the Region of a process\'s address space such as stack and
-- heap.
--
-- 'modifiedAt', 'runtimeContext_modifiedAt' - The timestamp at which the process modified the current process. The
-- timestamp is in UTC date string format.
--
-- 'modifyingProcess', 'runtimeContext_modifyingProcess' - Information about the process that modified the current process. This is
-- available for multiple finding types.
--
-- 'moduleFilePath', 'runtimeContext_moduleFilePath' - The path to the module loaded into the kernel.
--
-- 'moduleName', 'runtimeContext_moduleName' - The name of the module loaded into the kernel.
--
-- 'moduleSha256', 'runtimeContext_moduleSha256' - The @SHA256@ hash of the module.
--
-- 'mountSource', 'runtimeContext_mountSource' - The path on the host that is mounted by the container.
--
-- 'mountTarget', 'runtimeContext_mountTarget' - The path in the container that is mapped to the host directory.
--
-- 'releaseAgentPath', 'runtimeContext_releaseAgentPath' - The path in the container that modified the release agent file.
--
-- 'runcBinaryPath', 'runtimeContext_runcBinaryPath' - The path to the leveraged @runc@ implementation.
--
-- 'scriptPath', 'runtimeContext_scriptPath' - The path to the script that was executed.
--
-- 'shellHistoryFilePath', 'runtimeContext_shellHistoryFilePath' - The path to the modified shell history file.
--
-- 'socketPath', 'runtimeContext_socketPath' - The path to the docket socket that was accessed.
--
-- 'targetProcess', 'runtimeContext_targetProcess' - Information about the process that had its memory overwritten by the
-- current process.
newRuntimeContext ::
  RuntimeContext
newRuntimeContext =
  RuntimeContext'
    { addressFamily = Prelude.Nothing,
      fileSystemType = Prelude.Nothing,
      flags = Prelude.Nothing,
      ianaProtocolNumber = Prelude.Nothing,
      ldPreloadValue = Prelude.Nothing,
      libraryPath = Prelude.Nothing,
      memoryRegions = Prelude.Nothing,
      modifiedAt = Prelude.Nothing,
      modifyingProcess = Prelude.Nothing,
      moduleFilePath = Prelude.Nothing,
      moduleName = Prelude.Nothing,
      moduleSha256 = Prelude.Nothing,
      mountSource = Prelude.Nothing,
      mountTarget = Prelude.Nothing,
      releaseAgentPath = Prelude.Nothing,
      runcBinaryPath = Prelude.Nothing,
      scriptPath = Prelude.Nothing,
      shellHistoryFilePath = Prelude.Nothing,
      socketPath = Prelude.Nothing,
      targetProcess = Prelude.Nothing
    }

-- | Represents the communication protocol associated with the address. For
-- example, the address family @AF_INET@ is used for IP version of 4
-- protocol.
runtimeContext_addressFamily :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_addressFamily = Lens.lens (\RuntimeContext' {addressFamily} -> addressFamily) (\s@RuntimeContext' {} a -> s {addressFamily = a} :: RuntimeContext)

-- | Represents the type of mounted fileSystem.
runtimeContext_fileSystemType :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_fileSystemType = Lens.lens (\RuntimeContext' {fileSystemType} -> fileSystemType) (\s@RuntimeContext' {} a -> s {fileSystemType = a} :: RuntimeContext)

-- | Represents options that control the behavior of a runtime operation or
-- action. For example, a filesystem mount operation may contain a
-- read-only flag.
runtimeContext_flags :: Lens.Lens' RuntimeContext (Prelude.Maybe [Prelude.Text])
runtimeContext_flags = Lens.lens (\RuntimeContext' {flags} -> flags) (\s@RuntimeContext' {} a -> s {flags = a} :: RuntimeContext) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a particular protocol within the address family. Usually there
-- is a single protocol in address families. For example, the address
-- family @AF_INET@ only has the IP protocol.
runtimeContext_ianaProtocolNumber :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Int)
runtimeContext_ianaProtocolNumber = Lens.lens (\RuntimeContext' {ianaProtocolNumber} -> ianaProtocolNumber) (\s@RuntimeContext' {} a -> s {ianaProtocolNumber = a} :: RuntimeContext)

-- | The value of the LD_PRELOAD environment variable.
runtimeContext_ldPreloadValue :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_ldPreloadValue = Lens.lens (\RuntimeContext' {ldPreloadValue} -> ldPreloadValue) (\s@RuntimeContext' {} a -> s {ldPreloadValue = a} :: RuntimeContext)

-- | The path to the new library that was loaded.
runtimeContext_libraryPath :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_libraryPath = Lens.lens (\RuntimeContext' {libraryPath} -> libraryPath) (\s@RuntimeContext' {} a -> s {libraryPath = a} :: RuntimeContext)

-- | Specifies the Region of a process\'s address space such as stack and
-- heap.
runtimeContext_memoryRegions :: Lens.Lens' RuntimeContext (Prelude.Maybe [Prelude.Text])
runtimeContext_memoryRegions = Lens.lens (\RuntimeContext' {memoryRegions} -> memoryRegions) (\s@RuntimeContext' {} a -> s {memoryRegions = a} :: RuntimeContext) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp at which the process modified the current process. The
-- timestamp is in UTC date string format.
runtimeContext_modifiedAt :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.UTCTime)
runtimeContext_modifiedAt = Lens.lens (\RuntimeContext' {modifiedAt} -> modifiedAt) (\s@RuntimeContext' {} a -> s {modifiedAt = a} :: RuntimeContext) Prelude.. Lens.mapping Data._Time

-- | Information about the process that modified the current process. This is
-- available for multiple finding types.
runtimeContext_modifyingProcess :: Lens.Lens' RuntimeContext (Prelude.Maybe ProcessDetails)
runtimeContext_modifyingProcess = Lens.lens (\RuntimeContext' {modifyingProcess} -> modifyingProcess) (\s@RuntimeContext' {} a -> s {modifyingProcess = a} :: RuntimeContext)

-- | The path to the module loaded into the kernel.
runtimeContext_moduleFilePath :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_moduleFilePath = Lens.lens (\RuntimeContext' {moduleFilePath} -> moduleFilePath) (\s@RuntimeContext' {} a -> s {moduleFilePath = a} :: RuntimeContext)

-- | The name of the module loaded into the kernel.
runtimeContext_moduleName :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_moduleName = Lens.lens (\RuntimeContext' {moduleName} -> moduleName) (\s@RuntimeContext' {} a -> s {moduleName = a} :: RuntimeContext)

-- | The @SHA256@ hash of the module.
runtimeContext_moduleSha256 :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_moduleSha256 = Lens.lens (\RuntimeContext' {moduleSha256} -> moduleSha256) (\s@RuntimeContext' {} a -> s {moduleSha256 = a} :: RuntimeContext)

-- | The path on the host that is mounted by the container.
runtimeContext_mountSource :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_mountSource = Lens.lens (\RuntimeContext' {mountSource} -> mountSource) (\s@RuntimeContext' {} a -> s {mountSource = a} :: RuntimeContext)

-- | The path in the container that is mapped to the host directory.
runtimeContext_mountTarget :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_mountTarget = Lens.lens (\RuntimeContext' {mountTarget} -> mountTarget) (\s@RuntimeContext' {} a -> s {mountTarget = a} :: RuntimeContext)

-- | The path in the container that modified the release agent file.
runtimeContext_releaseAgentPath :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_releaseAgentPath = Lens.lens (\RuntimeContext' {releaseAgentPath} -> releaseAgentPath) (\s@RuntimeContext' {} a -> s {releaseAgentPath = a} :: RuntimeContext)

-- | The path to the leveraged @runc@ implementation.
runtimeContext_runcBinaryPath :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_runcBinaryPath = Lens.lens (\RuntimeContext' {runcBinaryPath} -> runcBinaryPath) (\s@RuntimeContext' {} a -> s {runcBinaryPath = a} :: RuntimeContext)

-- | The path to the script that was executed.
runtimeContext_scriptPath :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_scriptPath = Lens.lens (\RuntimeContext' {scriptPath} -> scriptPath) (\s@RuntimeContext' {} a -> s {scriptPath = a} :: RuntimeContext)

-- | The path to the modified shell history file.
runtimeContext_shellHistoryFilePath :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_shellHistoryFilePath = Lens.lens (\RuntimeContext' {shellHistoryFilePath} -> shellHistoryFilePath) (\s@RuntimeContext' {} a -> s {shellHistoryFilePath = a} :: RuntimeContext)

-- | The path to the docket socket that was accessed.
runtimeContext_socketPath :: Lens.Lens' RuntimeContext (Prelude.Maybe Prelude.Text)
runtimeContext_socketPath = Lens.lens (\RuntimeContext' {socketPath} -> socketPath) (\s@RuntimeContext' {} a -> s {socketPath = a} :: RuntimeContext)

-- | Information about the process that had its memory overwritten by the
-- current process.
runtimeContext_targetProcess :: Lens.Lens' RuntimeContext (Prelude.Maybe ProcessDetails)
runtimeContext_targetProcess = Lens.lens (\RuntimeContext' {targetProcess} -> targetProcess) (\s@RuntimeContext' {} a -> s {targetProcess = a} :: RuntimeContext)

instance Data.FromJSON RuntimeContext where
  parseJSON =
    Data.withObject
      "RuntimeContext"
      ( \x ->
          RuntimeContext'
            Prelude.<$> (x Data..:? "addressFamily")
            Prelude.<*> (x Data..:? "fileSystemType")
            Prelude.<*> (x Data..:? "flags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ianaProtocolNumber")
            Prelude.<*> (x Data..:? "ldPreloadValue")
            Prelude.<*> (x Data..:? "libraryPath")
            Prelude.<*> (x Data..:? "memoryRegions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "modifiedAt")
            Prelude.<*> (x Data..:? "modifyingProcess")
            Prelude.<*> (x Data..:? "moduleFilePath")
            Prelude.<*> (x Data..:? "moduleName")
            Prelude.<*> (x Data..:? "moduleSha256")
            Prelude.<*> (x Data..:? "mountSource")
            Prelude.<*> (x Data..:? "mountTarget")
            Prelude.<*> (x Data..:? "releaseAgentPath")
            Prelude.<*> (x Data..:? "runcBinaryPath")
            Prelude.<*> (x Data..:? "scriptPath")
            Prelude.<*> (x Data..:? "shellHistoryFilePath")
            Prelude.<*> (x Data..:? "socketPath")
            Prelude.<*> (x Data..:? "targetProcess")
      )

instance Prelude.Hashable RuntimeContext where
  hashWithSalt _salt RuntimeContext' {..} =
    _salt
      `Prelude.hashWithSalt` addressFamily
      `Prelude.hashWithSalt` fileSystemType
      `Prelude.hashWithSalt` flags
      `Prelude.hashWithSalt` ianaProtocolNumber
      `Prelude.hashWithSalt` ldPreloadValue
      `Prelude.hashWithSalt` libraryPath
      `Prelude.hashWithSalt` memoryRegions
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` modifyingProcess
      `Prelude.hashWithSalt` moduleFilePath
      `Prelude.hashWithSalt` moduleName
      `Prelude.hashWithSalt` moduleSha256
      `Prelude.hashWithSalt` mountSource
      `Prelude.hashWithSalt` mountTarget
      `Prelude.hashWithSalt` releaseAgentPath
      `Prelude.hashWithSalt` runcBinaryPath
      `Prelude.hashWithSalt` scriptPath
      `Prelude.hashWithSalt` shellHistoryFilePath
      `Prelude.hashWithSalt` socketPath
      `Prelude.hashWithSalt` targetProcess

instance Prelude.NFData RuntimeContext where
  rnf RuntimeContext' {..} =
    Prelude.rnf addressFamily
      `Prelude.seq` Prelude.rnf fileSystemType
      `Prelude.seq` Prelude.rnf flags
      `Prelude.seq` Prelude.rnf ianaProtocolNumber
      `Prelude.seq` Prelude.rnf ldPreloadValue
      `Prelude.seq` Prelude.rnf libraryPath
      `Prelude.seq` Prelude.rnf memoryRegions
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf modifyingProcess
      `Prelude.seq` Prelude.rnf moduleFilePath
      `Prelude.seq` Prelude.rnf moduleName
      `Prelude.seq` Prelude.rnf moduleSha256
      `Prelude.seq` Prelude.rnf mountSource
      `Prelude.seq` Prelude.rnf mountTarget
      `Prelude.seq` Prelude.rnf releaseAgentPath
      `Prelude.seq` Prelude.rnf runcBinaryPath
      `Prelude.seq` Prelude.rnf scriptPath
      `Prelude.seq` Prelude.rnf shellHistoryFilePath
      `Prelude.seq` Prelude.rnf socketPath
      `Prelude.seq` Prelude.rnf targetProcess
