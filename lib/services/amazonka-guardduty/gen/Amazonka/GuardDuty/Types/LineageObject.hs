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
-- Module      : Amazonka.GuardDuty.Types.LineageObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.LineageObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the runtime process details.
--
-- /See:/ 'newLineageObject' smart constructor.
data LineageObject = LineageObject'
  { -- | The effective user ID that was used to execute the process.
    euid :: Prelude.Maybe Prelude.Int,
    -- | The absolute path of the process executable file.
    executablePath :: Prelude.Maybe Prelude.Text,
    -- | The name of the process.
    name :: Prelude.Maybe Prelude.Text,
    -- | The process ID of the child process.
    namespacePid :: Prelude.Maybe Prelude.Int,
    -- | The unique ID of the parent process. This ID is assigned to the parent
    -- process by GuardDuty.
    parentUuid :: Prelude.Maybe Prelude.Text,
    -- | The ID of the process.
    pid :: Prelude.Maybe Prelude.Int,
    -- | The time when the process started. This is in UTC format.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The user ID of the user that executed the process.
    userId :: Prelude.Maybe Prelude.Int,
    -- | The unique ID assigned to the process by GuardDuty.
    uuid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineageObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'euid', 'lineageObject_euid' - The effective user ID that was used to execute the process.
--
-- 'executablePath', 'lineageObject_executablePath' - The absolute path of the process executable file.
--
-- 'name', 'lineageObject_name' - The name of the process.
--
-- 'namespacePid', 'lineageObject_namespacePid' - The process ID of the child process.
--
-- 'parentUuid', 'lineageObject_parentUuid' - The unique ID of the parent process. This ID is assigned to the parent
-- process by GuardDuty.
--
-- 'pid', 'lineageObject_pid' - The ID of the process.
--
-- 'startTime', 'lineageObject_startTime' - The time when the process started. This is in UTC format.
--
-- 'userId', 'lineageObject_userId' - The user ID of the user that executed the process.
--
-- 'uuid', 'lineageObject_uuid' - The unique ID assigned to the process by GuardDuty.
newLineageObject ::
  LineageObject
newLineageObject =
  LineageObject'
    { euid = Prelude.Nothing,
      executablePath = Prelude.Nothing,
      name = Prelude.Nothing,
      namespacePid = Prelude.Nothing,
      parentUuid = Prelude.Nothing,
      pid = Prelude.Nothing,
      startTime = Prelude.Nothing,
      userId = Prelude.Nothing,
      uuid = Prelude.Nothing
    }

-- | The effective user ID that was used to execute the process.
lineageObject_euid :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.Int)
lineageObject_euid = Lens.lens (\LineageObject' {euid} -> euid) (\s@LineageObject' {} a -> s {euid = a} :: LineageObject)

-- | The absolute path of the process executable file.
lineageObject_executablePath :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.Text)
lineageObject_executablePath = Lens.lens (\LineageObject' {executablePath} -> executablePath) (\s@LineageObject' {} a -> s {executablePath = a} :: LineageObject)

-- | The name of the process.
lineageObject_name :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.Text)
lineageObject_name = Lens.lens (\LineageObject' {name} -> name) (\s@LineageObject' {} a -> s {name = a} :: LineageObject)

-- | The process ID of the child process.
lineageObject_namespacePid :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.Int)
lineageObject_namespacePid = Lens.lens (\LineageObject' {namespacePid} -> namespacePid) (\s@LineageObject' {} a -> s {namespacePid = a} :: LineageObject)

-- | The unique ID of the parent process. This ID is assigned to the parent
-- process by GuardDuty.
lineageObject_parentUuid :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.Text)
lineageObject_parentUuid = Lens.lens (\LineageObject' {parentUuid} -> parentUuid) (\s@LineageObject' {} a -> s {parentUuid = a} :: LineageObject)

-- | The ID of the process.
lineageObject_pid :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.Int)
lineageObject_pid = Lens.lens (\LineageObject' {pid} -> pid) (\s@LineageObject' {} a -> s {pid = a} :: LineageObject)

-- | The time when the process started. This is in UTC format.
lineageObject_startTime :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.UTCTime)
lineageObject_startTime = Lens.lens (\LineageObject' {startTime} -> startTime) (\s@LineageObject' {} a -> s {startTime = a} :: LineageObject) Prelude.. Lens.mapping Data._Time

-- | The user ID of the user that executed the process.
lineageObject_userId :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.Int)
lineageObject_userId = Lens.lens (\LineageObject' {userId} -> userId) (\s@LineageObject' {} a -> s {userId = a} :: LineageObject)

-- | The unique ID assigned to the process by GuardDuty.
lineageObject_uuid :: Lens.Lens' LineageObject (Prelude.Maybe Prelude.Text)
lineageObject_uuid = Lens.lens (\LineageObject' {uuid} -> uuid) (\s@LineageObject' {} a -> s {uuid = a} :: LineageObject)

instance Data.FromJSON LineageObject where
  parseJSON =
    Data.withObject
      "LineageObject"
      ( \x ->
          LineageObject'
            Prelude.<$> (x Data..:? "euid")
            Prelude.<*> (x Data..:? "executablePath")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "namespacePid")
            Prelude.<*> (x Data..:? "parentUuid")
            Prelude.<*> (x Data..:? "pid")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "userId")
            Prelude.<*> (x Data..:? "uuid")
      )

instance Prelude.Hashable LineageObject where
  hashWithSalt _salt LineageObject' {..} =
    _salt
      `Prelude.hashWithSalt` euid
      `Prelude.hashWithSalt` executablePath
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespacePid
      `Prelude.hashWithSalt` parentUuid
      `Prelude.hashWithSalt` pid
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` uuid

instance Prelude.NFData LineageObject where
  rnf LineageObject' {..} =
    Prelude.rnf euid
      `Prelude.seq` Prelude.rnf executablePath
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespacePid
      `Prelude.seq` Prelude.rnf parentUuid
      `Prelude.seq` Prelude.rnf pid
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf uuid
