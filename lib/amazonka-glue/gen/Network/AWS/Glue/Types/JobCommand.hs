{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    jcfName,
    jcfPythonVersion,
    jcfScriptLocation,
  )
where

import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.PythonVersion as Types
import qualified Network.AWS.Glue.Types.ScriptLocationString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies code executed when a job is run.
--
-- /See:/ 'mkJobCommand' smart constructor.
data JobCommand = JobCommand'
  { -- | The name of the job command. For an Apache Spark ETL job, this must be @glueetl@ . For a Python shell job, it must be @pythonshell@ . For an Apache Spark streaming ETL job, this must be @gluestreaming@ .
    name :: Core.Maybe Types.Name,
    -- | The Python version being used to execute a Python shell job. Allowed values are 2 or 3.
    pythonVersion :: Core.Maybe Types.PythonVersion,
    -- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script that executes a job.
    scriptLocation :: Core.Maybe Types.ScriptLocationString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobCommand' value with any optional fields omitted.
mkJobCommand ::
  JobCommand
mkJobCommand =
  JobCommand'
    { name = Core.Nothing,
      pythonVersion = Core.Nothing,
      scriptLocation = Core.Nothing
    }

-- | The name of the job command. For an Apache Spark ETL job, this must be @glueetl@ . For a Python shell job, it must be @pythonshell@ . For an Apache Spark streaming ETL job, this must be @gluestreaming@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcfName :: Lens.Lens' JobCommand (Core.Maybe Types.Name)
jcfName = Lens.field @"name"
{-# DEPRECATED jcfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Python version being used to execute a Python shell job. Allowed values are 2 or 3.
--
-- /Note:/ Consider using 'pythonVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcfPythonVersion :: Lens.Lens' JobCommand (Core.Maybe Types.PythonVersion)
jcfPythonVersion = Lens.field @"pythonVersion"
{-# DEPRECATED jcfPythonVersion "Use generic-lens or generic-optics with 'pythonVersion' instead." #-}

-- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script that executes a job.
--
-- /Note:/ Consider using 'scriptLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcfScriptLocation :: Lens.Lens' JobCommand (Core.Maybe Types.ScriptLocationString)
jcfScriptLocation = Lens.field @"scriptLocation"
{-# DEPRECATED jcfScriptLocation "Use generic-lens or generic-optics with 'scriptLocation' instead." #-}

instance Core.FromJSON JobCommand where
  toJSON JobCommand {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("PythonVersion" Core..=) Core.<$> pythonVersion,
            ("ScriptLocation" Core..=) Core.<$> scriptLocation
          ]
      )

instance Core.FromJSON JobCommand where
  parseJSON =
    Core.withObject "JobCommand" Core.$
      \x ->
        JobCommand'
          Core.<$> (x Core..:? "Name")
          Core.<*> (x Core..:? "PythonVersion")
          Core.<*> (x Core..:? "ScriptLocation")
