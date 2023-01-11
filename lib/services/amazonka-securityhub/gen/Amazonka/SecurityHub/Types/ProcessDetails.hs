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
-- Module      : Amazonka.SecurityHub.Types.ProcessDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ProcessDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of process-related information about a finding.
--
-- /See:/ 'newProcessDetails' smart constructor.
data ProcessDetails = ProcessDetails'
  { -- | Indicates when the process was launched.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    launchedAt :: Prelude.Maybe Prelude.Text,
    -- | The name of the process.
    name :: Prelude.Maybe Prelude.Text,
    -- | The parent process ID.
    parentPid :: Prelude.Maybe Prelude.Int,
    -- | The path to the process executable.
    path :: Prelude.Maybe Prelude.Text,
    -- | The process ID.
    pid :: Prelude.Maybe Prelude.Int,
    -- | Indicates when the process was terminated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    terminatedAt :: Prelude.Maybe Prelude.Text
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
-- 'launchedAt', 'processDetails_launchedAt' - Indicates when the process was launched.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'name', 'processDetails_name' - The name of the process.
--
-- 'parentPid', 'processDetails_parentPid' - The parent process ID.
--
-- 'path', 'processDetails_path' - The path to the process executable.
--
-- 'pid', 'processDetails_pid' - The process ID.
--
-- 'terminatedAt', 'processDetails_terminatedAt' - Indicates when the process was terminated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newProcessDetails ::
  ProcessDetails
newProcessDetails =
  ProcessDetails'
    { launchedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      parentPid = Prelude.Nothing,
      path = Prelude.Nothing,
      pid = Prelude.Nothing,
      terminatedAt = Prelude.Nothing
    }

-- | Indicates when the process was launched.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
processDetails_launchedAt :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_launchedAt = Lens.lens (\ProcessDetails' {launchedAt} -> launchedAt) (\s@ProcessDetails' {} a -> s {launchedAt = a} :: ProcessDetails)

-- | The name of the process.
processDetails_name :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_name = Lens.lens (\ProcessDetails' {name} -> name) (\s@ProcessDetails' {} a -> s {name = a} :: ProcessDetails)

-- | The parent process ID.
processDetails_parentPid :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Int)
processDetails_parentPid = Lens.lens (\ProcessDetails' {parentPid} -> parentPid) (\s@ProcessDetails' {} a -> s {parentPid = a} :: ProcessDetails)

-- | The path to the process executable.
processDetails_path :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_path = Lens.lens (\ProcessDetails' {path} -> path) (\s@ProcessDetails' {} a -> s {path = a} :: ProcessDetails)

-- | The process ID.
processDetails_pid :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Int)
processDetails_pid = Lens.lens (\ProcessDetails' {pid} -> pid) (\s@ProcessDetails' {} a -> s {pid = a} :: ProcessDetails)

-- | Indicates when the process was terminated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
processDetails_terminatedAt :: Lens.Lens' ProcessDetails (Prelude.Maybe Prelude.Text)
processDetails_terminatedAt = Lens.lens (\ProcessDetails' {terminatedAt} -> terminatedAt) (\s@ProcessDetails' {} a -> s {terminatedAt = a} :: ProcessDetails)

instance Data.FromJSON ProcessDetails where
  parseJSON =
    Data.withObject
      "ProcessDetails"
      ( \x ->
          ProcessDetails'
            Prelude.<$> (x Data..:? "LaunchedAt")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ParentPid")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "Pid")
            Prelude.<*> (x Data..:? "TerminatedAt")
      )

instance Prelude.Hashable ProcessDetails where
  hashWithSalt _salt ProcessDetails' {..} =
    _salt `Prelude.hashWithSalt` launchedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parentPid
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` pid
      `Prelude.hashWithSalt` terminatedAt

instance Prelude.NFData ProcessDetails where
  rnf ProcessDetails' {..} =
    Prelude.rnf launchedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parentPid
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf pid
      `Prelude.seq` Prelude.rnf terminatedAt

instance Data.ToJSON ProcessDetails where
  toJSON ProcessDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LaunchedAt" Data..=) Prelude.<$> launchedAt,
            ("Name" Data..=) Prelude.<$> name,
            ("ParentPid" Data..=) Prelude.<$> parentPid,
            ("Path" Data..=) Prelude.<$> path,
            ("Pid" Data..=) Prelude.<$> pid,
            ("TerminatedAt" Data..=) Prelude.<$> terminatedAt
          ]
      )
