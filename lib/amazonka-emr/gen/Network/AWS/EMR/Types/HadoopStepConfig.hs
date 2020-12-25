{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.HadoopStepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.HadoopStepConfig
  ( HadoopStepConfig (..),

    -- * Smart constructor
    mkHadoopStepConfig,

    -- * Lenses
    hscArgs,
    hscJar,
    hscMainClass,
    hscProperties,
  )
where

import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A cluster step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
-- /See:/ 'mkHadoopStepConfig' smart constructor.
data HadoopStepConfig = HadoopStepConfig'
  { -- | The list of command line arguments to pass to the JAR file's main function for execution.
    args :: Core.Maybe [Types.String],
    -- | The path to the JAR file that runs during the step.
    jar :: Core.Maybe Types.String,
    -- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
    mainClass :: Core.Maybe Types.String,
    -- | The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
    properties :: Core.Maybe (Core.HashMap Types.String Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HadoopStepConfig' value with any optional fields omitted.
mkHadoopStepConfig ::
  HadoopStepConfig
mkHadoopStepConfig =
  HadoopStepConfig'
    { args = Core.Nothing,
      jar = Core.Nothing,
      mainClass = Core.Nothing,
      properties = Core.Nothing
    }

-- | The list of command line arguments to pass to the JAR file's main function for execution.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscArgs :: Lens.Lens' HadoopStepConfig (Core.Maybe [Types.String])
hscArgs = Lens.field @"args"
{-# DEPRECATED hscArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The path to the JAR file that runs during the step.
--
-- /Note:/ Consider using 'jar' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscJar :: Lens.Lens' HadoopStepConfig (Core.Maybe Types.String)
hscJar = Lens.field @"jar"
{-# DEPRECATED hscJar "Use generic-lens or generic-optics with 'jar' instead." #-}

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
--
-- /Note:/ Consider using 'mainClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscMainClass :: Lens.Lens' HadoopStepConfig (Core.Maybe Types.String)
hscMainClass = Lens.field @"mainClass"
{-# DEPRECATED hscMainClass "Use generic-lens or generic-optics with 'mainClass' instead." #-}

-- | The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscProperties :: Lens.Lens' HadoopStepConfig (Core.Maybe (Core.HashMap Types.String Types.String))
hscProperties = Lens.field @"properties"
{-# DEPRECATED hscProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

instance Core.FromJSON HadoopStepConfig where
  parseJSON =
    Core.withObject "HadoopStepConfig" Core.$
      \x ->
        HadoopStepConfig'
          Core.<$> (x Core..:? "Args")
          Core.<*> (x Core..:? "Jar")
          Core.<*> (x Core..:? "MainClass")
          Core.<*> (x Core..:? "Properties")
