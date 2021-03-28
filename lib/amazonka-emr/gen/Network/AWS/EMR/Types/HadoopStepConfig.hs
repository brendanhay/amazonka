{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.HadoopStepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.HadoopStepConfig
  ( HadoopStepConfig (..)
  -- * Smart constructor
  , mkHadoopStepConfig
  -- * Lenses
  , hscArgs
  , hscJar
  , hscMainClass
  , hscProperties
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A cluster step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
-- /See:/ 'mkHadoopStepConfig' smart constructor.
data HadoopStepConfig = HadoopStepConfig'
  { args :: Core.Maybe [Core.Text]
    -- ^ The list of command line arguments to pass to the JAR file's main function for execution.
  , jar :: Core.Maybe Core.Text
    -- ^ The path to the JAR file that runs during the step.
  , mainClass :: Core.Maybe Core.Text
    -- ^ The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
  , properties :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HadoopStepConfig' value with any optional fields omitted.
mkHadoopStepConfig
    :: HadoopStepConfig
mkHadoopStepConfig
  = HadoopStepConfig'{args = Core.Nothing, jar = Core.Nothing,
                      mainClass = Core.Nothing, properties = Core.Nothing}

-- | The list of command line arguments to pass to the JAR file's main function for execution.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscArgs :: Lens.Lens' HadoopStepConfig (Core.Maybe [Core.Text])
hscArgs = Lens.field @"args"
{-# INLINEABLE hscArgs #-}
{-# DEPRECATED args "Use generic-lens or generic-optics with 'args' instead"  #-}

-- | The path to the JAR file that runs during the step.
--
-- /Note:/ Consider using 'jar' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscJar :: Lens.Lens' HadoopStepConfig (Core.Maybe Core.Text)
hscJar = Lens.field @"jar"
{-# INLINEABLE hscJar #-}
{-# DEPRECATED jar "Use generic-lens or generic-optics with 'jar' instead"  #-}

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
--
-- /Note:/ Consider using 'mainClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscMainClass :: Lens.Lens' HadoopStepConfig (Core.Maybe Core.Text)
hscMainClass = Lens.field @"mainClass"
{-# INLINEABLE hscMainClass #-}
{-# DEPRECATED mainClass "Use generic-lens or generic-optics with 'mainClass' instead"  #-}

-- | The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscProperties :: Lens.Lens' HadoopStepConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
hscProperties = Lens.field @"properties"
{-# INLINEABLE hscProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

instance Core.FromJSON HadoopStepConfig where
        parseJSON
          = Core.withObject "HadoopStepConfig" Core.$
              \ x ->
                HadoopStepConfig' Core.<$>
                  (x Core..:? "Args") Core.<*> x Core..:? "Jar" Core.<*>
                    x Core..:? "MainClass"
                    Core.<*> x Core..:? "Properties"
