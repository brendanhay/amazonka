-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobCommand
  ( JobCommand (..),

    -- * Smart constructor
    mkJobCommand,

    -- * Lenses
    jobScriptLocation,
    jobPythonVersion,
    jobName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies code executed when a job is run.
--
-- /See:/ 'mkJobCommand' smart constructor.
data JobCommand = JobCommand'
  { scriptLocation ::
      Lude.Maybe Lude.Text,
    pythonVersion :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobCommand' with the minimum fields required to make a request.
--
-- * 'name' - The name of the job command. For an Apache Spark ETL job, this must be @glueetl@ . For a Python shell job, it must be @pythonshell@ . For an Apache Spark streaming ETL job, this must be @gluestreaming@ .
-- * 'pythonVersion' - The Python version being used to execute a Python shell job. Allowed values are 2 or 3.
-- * 'scriptLocation' - Specifies the Amazon Simple Storage Service (Amazon S3) path to a script that executes a job.
mkJobCommand ::
  JobCommand
mkJobCommand =
  JobCommand'
    { scriptLocation = Lude.Nothing,
      pythonVersion = Lude.Nothing,
      name = Lude.Nothing
    }

-- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script that executes a job.
--
-- /Note:/ Consider using 'scriptLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobScriptLocation :: Lens.Lens' JobCommand (Lude.Maybe Lude.Text)
jobScriptLocation = Lens.lens (scriptLocation :: JobCommand -> Lude.Maybe Lude.Text) (\s a -> s {scriptLocation = a} :: JobCommand)
{-# DEPRECATED jobScriptLocation "Use generic-lens or generic-optics with 'scriptLocation' instead." #-}

-- | The Python version being used to execute a Python shell job. Allowed values are 2 or 3.
--
-- /Note:/ Consider using 'pythonVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobPythonVersion :: Lens.Lens' JobCommand (Lude.Maybe Lude.Text)
jobPythonVersion = Lens.lens (pythonVersion :: JobCommand -> Lude.Maybe Lude.Text) (\s a -> s {pythonVersion = a} :: JobCommand)
{-# DEPRECATED jobPythonVersion "Use generic-lens or generic-optics with 'pythonVersion' instead." #-}

-- | The name of the job command. For an Apache Spark ETL job, this must be @glueetl@ . For a Python shell job, it must be @pythonshell@ . For an Apache Spark streaming ETL job, this must be @gluestreaming@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobName :: Lens.Lens' JobCommand (Lude.Maybe Lude.Text)
jobName = Lens.lens (name :: JobCommand -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: JobCommand)
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON JobCommand where
  parseJSON =
    Lude.withObject
      "JobCommand"
      ( \x ->
          JobCommand'
            Lude.<$> (x Lude..:? "ScriptLocation")
            Lude.<*> (x Lude..:? "PythonVersion")
            Lude.<*> (x Lude..:? "Name")
      )

instance Lude.ToJSON JobCommand where
  toJSON JobCommand' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ScriptLocation" Lude..=) Lude.<$> scriptLocation,
            ("PythonVersion" Lude..=) Lude.<$> pythonVersion,
            ("Name" Lude..=) Lude.<$> name
          ]
      )
