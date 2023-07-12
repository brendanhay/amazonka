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
-- Module      : Amazonka.Glue.Types.JobCommand
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JobCommand where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies code that runs when a job is run.
--
-- /See:/ 'newJobCommand' smart constructor.
data JobCommand = JobCommand'
  { -- | The name of the job command. For an Apache Spark ETL job, this must be
    -- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
    -- Apache Spark streaming ETL job, this must be @gluestreaming@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Python version being used to run a Python shell job. Allowed values
    -- are 2 or 3.
    pythonVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
    -- that runs a job.
    scriptLocation :: Prelude.Maybe Prelude.Text
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
-- 'name', 'jobCommand_name' - The name of the job command. For an Apache Spark ETL job, this must be
-- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
-- Apache Spark streaming ETL job, this must be @gluestreaming@.
--
-- 'pythonVersion', 'jobCommand_pythonVersion' - The Python version being used to run a Python shell job. Allowed values
-- are 2 or 3.
--
-- 'scriptLocation', 'jobCommand_scriptLocation' - Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
-- that runs a job.
newJobCommand ::
  JobCommand
newJobCommand =
  JobCommand'
    { name = Prelude.Nothing,
      pythonVersion = Prelude.Nothing,
      scriptLocation = Prelude.Nothing
    }

-- | The name of the job command. For an Apache Spark ETL job, this must be
-- @glueetl@. For a Python shell job, it must be @pythonshell@. For an
-- Apache Spark streaming ETL job, this must be @gluestreaming@.
jobCommand_name :: Lens.Lens' JobCommand (Prelude.Maybe Prelude.Text)
jobCommand_name = Lens.lens (\JobCommand' {name} -> name) (\s@JobCommand' {} a -> s {name = a} :: JobCommand)

-- | The Python version being used to run a Python shell job. Allowed values
-- are 2 or 3.
jobCommand_pythonVersion :: Lens.Lens' JobCommand (Prelude.Maybe Prelude.Text)
jobCommand_pythonVersion = Lens.lens (\JobCommand' {pythonVersion} -> pythonVersion) (\s@JobCommand' {} a -> s {pythonVersion = a} :: JobCommand)

-- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script
-- that runs a job.
jobCommand_scriptLocation :: Lens.Lens' JobCommand (Prelude.Maybe Prelude.Text)
jobCommand_scriptLocation = Lens.lens (\JobCommand' {scriptLocation} -> scriptLocation) (\s@JobCommand' {} a -> s {scriptLocation = a} :: JobCommand)

instance Data.FromJSON JobCommand where
  parseJSON =
    Data.withObject
      "JobCommand"
      ( \x ->
          JobCommand'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PythonVersion")
            Prelude.<*> (x Data..:? "ScriptLocation")
      )

instance Prelude.Hashable JobCommand where
  hashWithSalt _salt JobCommand' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pythonVersion
      `Prelude.hashWithSalt` scriptLocation

instance Prelude.NFData JobCommand where
  rnf JobCommand' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf pythonVersion
      `Prelude.seq` Prelude.rnf scriptLocation

instance Data.ToJSON JobCommand where
  toJSON JobCommand' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("PythonVersion" Data..=) Prelude.<$> pythonVersion,
            ("ScriptLocation" Data..=)
              Prelude.<$> scriptLocation
          ]
      )
