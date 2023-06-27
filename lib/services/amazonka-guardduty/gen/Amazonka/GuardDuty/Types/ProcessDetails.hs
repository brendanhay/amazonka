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
-- Module      : Amazonka.GuardDuty.Types.ProcessDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ProcessDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.LineageObject
import qualified Amazonka.Prelude as Prelude

-- | Information about the observed process.
--
-- /See:/ 'newProcessDetails' smart constructor.
data ProcessDetails = ProcessDetails'
  { -- | The effective user ID of the user that executed the process.
    euid :: Prelude.Maybe Prelude.Int,
    -- | The absolute path of the process executable file.
    executablePath :: Prelude.Maybe Prelude.Text,
    -- | The @SHA256@ hash of the process executable.
    executableSha256 :: Prelude.Maybe Prelude.Text,
    -- | Information about the process\'s lineage.
    lineage :: Prelude.Maybe [LineageObject],
    -- | The name of the process.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the child process.
    namespacePid :: Prelude.Maybe Prelude.Int,
    -- | The unique ID of the parent process. This ID is assigned to the parent
    -- process by GuardDuty.
    parentUuid :: Prelude.Maybe Prelude.Text,
    -- | The ID of the process.
    pid :: Prelude.Maybe Prelude.Int,
    -- | The present working directory of the process.
    pwd :: Prelude.Maybe Prelude.Text,
    -- | The time when the process started. This is in UTC format.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The user that executed the process.
    user :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the user that executed the process.
    userId :: Prelude.Maybe Prelude.Int,
    -- | The unique ID assigned to the process by GuardDuty.
    uuid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'euid', 'processDetails_euid' - The effective user ID of the user that executed the process.
--
-- 'executablePath', 'processDetails_executablePath' - The absolute path of the process executable file.
--
-- 'executableSha256', 'processDetails_executableSha256' - The @SHA256@ hash of the process executable.
--
-- 'lineage', 'processDetails_lineage' - Information about the process\'s lineage.
--
-- 'name', 'processDetails_name' - The name of the process.
--
-- 'namespacePid', 'processDetails_namespacePid' - The ID of the child process.
--
-- 'parentUuid', 'processDetails_parentUuid' - The unique ID of the parent process. This ID is assigned to the parent
-- process by GuardDuty.
--
-- 'pid', 'processDetails_pid' - The ID of the process.
--
-- 'pwd', 'processDetails_pwd' - The present working directory of the process.
--
-- 'startTime', 'processDetails_startTime' - The time when the process started. This is in UTC format.
--
-- 'user', 'processDetails_user' - The user that executed the process.
--
-- 'userId', 'processDetails_userId' - The unique ID of the user that executed the process.
--
-- 'uuid', 'processDetails_uuid' - The unique ID assigned to the process by GuardDuty.
newProcessDetails ::
  ProcessDetails
newProcessDetails =
  ProcessDetails'
    { euid = Prelude.Nothing,
      executablePath = Prelude.Nothing,
      executableSha256 = Prelude.Nothing,
      lineage = Prelude.Nothing,
      name = Prelude.Nothing,
      namespacePid = Prelude.Nothing,
      parentUuid = Prelude.Nothing,
      pid = Prelude.Nothing,
      pwd = Prelude.Nothing,
      startTime = Prelude.Nothing,
      user = Prelude.Nothing,
      userId = Prelude.Nothing,
      uuid = Prelude.Nothing
    }

-- | The effective user ID of the user that executed the process.
processDetails_euid :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Int)
processDetails_euid = Lens.lens (\ProcessDetails' {euid} -> euid) (\s@ProcessDetails' {} a -> s {euid = a} :: ProcessDetails)

-- | The absolute path of the process executable file.
processDetails_executablePath :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_executablePath = Lens.lens (\ProcessDetails' {executablePath} -> executablePath) (\s@ProcessDetails' {} a -> s {executablePath = a} :: ProcessDetails)

-- | The @SHA256@ hash of the process executable.
processDetails_executableSha256 :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_executableSha256 = Lens.lens (\ProcessDetails' {executableSha256} -> executableSha256) (\s@ProcessDetails' {} a -> s {executableSha256 = a} :: ProcessDetails)

-- | Information about the process\'s lineage.
processDetails_lineage :: Lens.Lens' ProcessDetails (Prelude.Maybe [LineageObject])
processDetails_lineage = Lens.lens (\ProcessDetails' {lineage} -> lineage) (\s@ProcessDetails' {} a -> s {lineage = a} :: ProcessDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the process.
processDetails_name :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_name = Lens.lens (\ProcessDetails' {name} -> name) (\s@ProcessDetails' {} a -> s {name = a} :: ProcessDetails)

-- | The ID of the child process.
processDetails_namespacePid :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Int)
processDetails_namespacePid = Lens.lens (\ProcessDetails' {namespacePid} -> namespacePid) (\s@ProcessDetails' {} a -> s {namespacePid = a} :: ProcessDetails)

-- | The unique ID of the parent process. This ID is assigned to the parent
-- process by GuardDuty.
processDetails_parentUuid :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_parentUuid = Lens.lens (\ProcessDetails' {parentUuid} -> parentUuid) (\s@ProcessDetails' {} a -> s {parentUuid = a} :: ProcessDetails)

-- | The ID of the process.
processDetails_pid :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Int)
processDetails_pid = Lens.lens (\ProcessDetails' {pid} -> pid) (\s@ProcessDetails' {} a -> s {pid = a} :: ProcessDetails)

-- | The present working directory of the process.
processDetails_pwd :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_pwd = Lens.lens (\ProcessDetails' {pwd} -> pwd) (\s@ProcessDetails' {} a -> s {pwd = a} :: ProcessDetails)

-- | The time when the process started. This is in UTC format.
processDetails_startTime :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.UTCTime)
processDetails_startTime = Lens.lens (\ProcessDetails' {startTime} -> startTime) (\s@ProcessDetails' {} a -> s {startTime = a} :: ProcessDetails) Prelude.. Lens.mapping Data._Time

-- | The user that executed the process.
processDetails_user :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_user = Lens.lens (\ProcessDetails' {user} -> user) (\s@ProcessDetails' {} a -> s {user = a} :: ProcessDetails)

-- | The unique ID of the user that executed the process.
processDetails_userId :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Int)
processDetails_userId = Lens.lens (\ProcessDetails' {userId} -> userId) (\s@ProcessDetails' {} a -> s {userId = a} :: ProcessDetails)

-- | The unique ID assigned to the process by GuardDuty.
processDetails_uuid :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_uuid = Lens.lens (\ProcessDetails' {uuid} -> uuid) (\s@ProcessDetails' {} a -> s {uuid = a} :: ProcessDetails)

instance Data.FromJSON ProcessDetails where
  parseJSON =
    Data.withObject
      "ProcessDetails"
      ( \x ->
          ProcessDetails'
            Prelude.<$> (x Data..:? "euid")
            Prelude.<*> (x Data..:? "executablePath")
            Prelude.<*> (x Data..:? "executableSha256")
            Prelude.<*> (x Data..:? "lineage" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "namespacePid")
            Prelude.<*> (x Data..:? "parentUuid")
            Prelude.<*> (x Data..:? "pid")
            Prelude.<*> (x Data..:? "pwd")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "user")
            Prelude.<*> (x Data..:? "userId")
            Prelude.<*> (x Data..:? "uuid")
      )

instance Prelude.Hashable ProcessDetails where
  hashWithSalt _salt ProcessDetails' {..} =
    _salt
      `Prelude.hashWithSalt` euid
      `Prelude.hashWithSalt` executablePath
      `Prelude.hashWithSalt` executableSha256
      `Prelude.hashWithSalt` lineage
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespacePid
      `Prelude.hashWithSalt` parentUuid
      `Prelude.hashWithSalt` pid
      `Prelude.hashWithSalt` pwd
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` uuid

instance Prelude.NFData ProcessDetails where
  rnf ProcessDetails' {..} =
    Prelude.rnf euid
      `Prelude.seq` Prelude.rnf executablePath
      `Prelude.seq` Prelude.rnf executableSha256
      `Prelude.seq` Prelude.rnf lineage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespacePid
      `Prelude.seq` Prelude.rnf parentUuid
      `Prelude.seq` Prelude.rnf pid
      `Prelude.seq` Prelude.rnf pwd
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf uuid
