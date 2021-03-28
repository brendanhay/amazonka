{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.HadoopJarStepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.HadoopJarStepConfig
  ( HadoopJarStepConfig (..)
  -- * Smart constructor
  , mkHadoopJarStepConfig
  -- * Lenses
  , hjscJar
  , hjscArgs
  , hjscMainClass
  , hjscProperties
  ) where

import qualified Network.AWS.EMR.Types.KeyValue as Types
import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A job flow step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
-- /See:/ 'mkHadoopJarStepConfig' smart constructor.
data HadoopJarStepConfig = HadoopJarStepConfig'
  { jar :: Types.XmlString
    -- ^ A path to a JAR file run during the step.
  , args :: Core.Maybe [Types.XmlString]
    -- ^ A list of command line arguments passed to the JAR file's main function when executed.
  , mainClass :: Core.Maybe Types.XmlString
    -- ^ The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
  , properties :: Core.Maybe [Types.KeyValue]
    -- ^ A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HadoopJarStepConfig' value with any optional fields omitted.
mkHadoopJarStepConfig
    :: Types.XmlString -- ^ 'jar'
    -> HadoopJarStepConfig
mkHadoopJarStepConfig jar
  = HadoopJarStepConfig'{jar, args = Core.Nothing,
                         mainClass = Core.Nothing, properties = Core.Nothing}

-- | A path to a JAR file run during the step.
--
-- /Note:/ Consider using 'jar' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjscJar :: Lens.Lens' HadoopJarStepConfig Types.XmlString
hjscJar = Lens.field @"jar"
{-# INLINEABLE hjscJar #-}
{-# DEPRECATED jar "Use generic-lens or generic-optics with 'jar' instead"  #-}

-- | A list of command line arguments passed to the JAR file's main function when executed.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjscArgs :: Lens.Lens' HadoopJarStepConfig (Core.Maybe [Types.XmlString])
hjscArgs = Lens.field @"args"
{-# INLINEABLE hjscArgs #-}
{-# DEPRECATED args "Use generic-lens or generic-optics with 'args' instead"  #-}

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
--
-- /Note:/ Consider using 'mainClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjscMainClass :: Lens.Lens' HadoopJarStepConfig (Core.Maybe Types.XmlString)
hjscMainClass = Lens.field @"mainClass"
{-# INLINEABLE hjscMainClass #-}
{-# DEPRECATED mainClass "Use generic-lens or generic-optics with 'mainClass' instead"  #-}

-- | A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjscProperties :: Lens.Lens' HadoopJarStepConfig (Core.Maybe [Types.KeyValue])
hjscProperties = Lens.field @"properties"
{-# INLINEABLE hjscProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

instance Core.FromJSON HadoopJarStepConfig where
        toJSON HadoopJarStepConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Jar" Core..= jar), ("Args" Core..=) Core.<$> args,
                  ("MainClass" Core..=) Core.<$> mainClass,
                  ("Properties" Core..=) Core.<$> properties])
