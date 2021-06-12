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
-- Module      : Network.AWS.Glue.Types.JobCommand
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobCommand where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies code executed when a job is run.
--
-- /See:/ 'newJobCommand' smart constructor.
data JobCommand = JobCommand'
  { -- | The Python version being used to execute a Python shell job. Allowed
    -- values are 2 or 3.
    pythonVersion :: Core.Maybe Core.Text,
    -- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
    -- that executes a job.
    scriptLocation :: Core.Maybe Core.Text,
    -- | The name of the job command. For an Apache Spark ETL job, this must be
    -- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
    -- Apache Spark streaming ETL job, this must be @gluestreaming@.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pythonVersion', 'jobCommand_pythonVersion' - The Python version being used to execute a Python shell job. Allowed
-- values are 2 or 3.
--
-- 'scriptLocation', 'jobCommand_scriptLocation' - Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
-- that executes a job.
--
-- 'name', 'jobCommand_name' - The name of the job command. For an Apache Spark ETL job, this must be
-- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
-- Apache Spark streaming ETL job, this must be @gluestreaming@.
newJobCommand ::
  JobCommand
newJobCommand =
  JobCommand'
    { pythonVersion = Core.Nothing,
      scriptLocation = Core.Nothing,
      name = Core.Nothing
    }

-- | The Python version being used to execute a Python shell job. Allowed
-- values are 2 or 3.
jobCommand_pythonVersion :: Lens.Lens' JobCommand (Core.Maybe Core.Text)
jobCommand_pythonVersion = Lens.lens (\JobCommand' {pythonVersion} -> pythonVersion) (\s@JobCommand' {} a -> s {pythonVersion = a} :: JobCommand)

-- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
-- that executes a job.
jobCommand_scriptLocation :: Lens.Lens' JobCommand (Core.Maybe Core.Text)
jobCommand_scriptLocation = Lens.lens (\JobCommand' {scriptLocation} -> scriptLocation) (\s@JobCommand' {} a -> s {scriptLocation = a} :: JobCommand)

-- | The name of the job command. For an Apache Spark ETL job, this must be
-- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
-- Apache Spark streaming ETL job, this must be @gluestreaming@.
jobCommand_name :: Lens.Lens' JobCommand (Core.Maybe Core.Text)
jobCommand_name = Lens.lens (\JobCommand' {name} -> name) (\s@JobCommand' {} a -> s {name = a} :: JobCommand)

instance Core.FromJSON JobCommand where
  parseJSON =
    Core.withObject
      "JobCommand"
      ( \x ->
          JobCommand'
            Core.<$> (x Core..:? "PythonVersion")
            Core.<*> (x Core..:? "ScriptLocation")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable JobCommand

instance Core.NFData JobCommand

instance Core.ToJSON JobCommand where
  toJSON JobCommand' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PythonVersion" Core..=) Core.<$> pythonVersion,
            ("ScriptLocation" Core..=) Core.<$> scriptLocation,
            ("Name" Core..=) Core.<$> name
          ]
      )
