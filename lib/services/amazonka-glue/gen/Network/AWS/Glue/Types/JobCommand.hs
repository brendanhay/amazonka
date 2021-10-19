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
import qualified Network.AWS.Prelude as Prelude

-- | Specifies code that runs when a job is run.
--
-- /See:/ 'newJobCommand' smart constructor.
data JobCommand = JobCommand'
  { -- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
    -- that runs a job.
    scriptLocation :: Prelude.Maybe Prelude.Text,
    -- | The Python version being used to run a Python shell job. Allowed values
    -- are 2 or 3.
    pythonVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the job command. For an Apache Spark ETL job, this must be
    -- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
    -- Apache Spark streaming ETL job, this must be @gluestreaming@.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scriptLocation', 'jobCommand_scriptLocation' - Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
-- that runs a job.
--
-- 'pythonVersion', 'jobCommand_pythonVersion' - The Python version being used to run a Python shell job. Allowed values
-- are 2 or 3.
--
-- 'name', 'jobCommand_name' - The name of the job command. For an Apache Spark ETL job, this must be
-- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
-- Apache Spark streaming ETL job, this must be @gluestreaming@.
newJobCommand ::
  JobCommand
newJobCommand =
  JobCommand'
    { scriptLocation = Prelude.Nothing,
      pythonVersion = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
-- that runs a job.
jobCommand_scriptLocation :: Lens.Lens' JobCommand (Prelude.Maybe Prelude.Text)
jobCommand_scriptLocation = Lens.lens (\JobCommand' {scriptLocation} -> scriptLocation) (\s@JobCommand' {} a -> s {scriptLocation = a} :: JobCommand)

-- | The Python version being used to run a Python shell job. Allowed values
-- are 2 or 3.
jobCommand_pythonVersion :: Lens.Lens' JobCommand (Prelude.Maybe Prelude.Text)
jobCommand_pythonVersion = Lens.lens (\JobCommand' {pythonVersion} -> pythonVersion) (\s@JobCommand' {} a -> s {pythonVersion = a} :: JobCommand)

-- | The name of the job command. For an Apache Spark ETL job, this must be
-- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
-- Apache Spark streaming ETL job, this must be @gluestreaming@.
jobCommand_name :: Lens.Lens' JobCommand (Prelude.Maybe Prelude.Text)
jobCommand_name = Lens.lens (\JobCommand' {name} -> name) (\s@JobCommand' {} a -> s {name = a} :: JobCommand)

instance Core.FromJSON JobCommand where
  parseJSON =
    Core.withObject
      "JobCommand"
      ( \x ->
          JobCommand'
            Prelude.<$> (x Core..:? "ScriptLocation")
            Prelude.<*> (x Core..:? "PythonVersion")
            Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable JobCommand

instance Prelude.NFData JobCommand

instance Core.ToJSON JobCommand where
  toJSON JobCommand' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ScriptLocation" Core..=)
              Prelude.<$> scriptLocation,
            ("PythonVersion" Core..=) Prelude.<$> pythonVersion,
            ("Name" Core..=) Prelude.<$> name
          ]
      )
