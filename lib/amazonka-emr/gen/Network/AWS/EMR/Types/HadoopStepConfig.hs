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
    hscJAR,
    hscMainClass,
    hscProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A cluster step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
-- /See:/ 'mkHadoopStepConfig' smart constructor.
data HadoopStepConfig = HadoopStepConfig'
  { -- | The list of command line arguments to pass to the JAR file's main function for execution.
    args :: Lude.Maybe [Lude.Text],
    -- | The path to the JAR file that runs during the step.
    jar :: Lude.Maybe Lude.Text,
    -- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
    mainClass :: Lude.Maybe Lude.Text,
    -- | The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
    properties :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HadoopStepConfig' with the minimum fields required to make a request.
--
-- * 'args' - The list of command line arguments to pass to the JAR file's main function for execution.
-- * 'jar' - The path to the JAR file that runs during the step.
-- * 'mainClass' - The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
-- * 'properties' - The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
mkHadoopStepConfig ::
  HadoopStepConfig
mkHadoopStepConfig =
  HadoopStepConfig'
    { args = Lude.Nothing,
      jar = Lude.Nothing,
      mainClass = Lude.Nothing,
      properties = Lude.Nothing
    }

-- | The list of command line arguments to pass to the JAR file's main function for execution.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscArgs :: Lens.Lens' HadoopStepConfig (Lude.Maybe [Lude.Text])
hscArgs = Lens.lens (args :: HadoopStepConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {args = a} :: HadoopStepConfig)
{-# DEPRECATED hscArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The path to the JAR file that runs during the step.
--
-- /Note:/ Consider using 'jar' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscJAR :: Lens.Lens' HadoopStepConfig (Lude.Maybe Lude.Text)
hscJAR = Lens.lens (jar :: HadoopStepConfig -> Lude.Maybe Lude.Text) (\s a -> s {jar = a} :: HadoopStepConfig)
{-# DEPRECATED hscJAR "Use generic-lens or generic-optics with 'jar' instead." #-}

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
--
-- /Note:/ Consider using 'mainClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscMainClass :: Lens.Lens' HadoopStepConfig (Lude.Maybe Lude.Text)
hscMainClass = Lens.lens (mainClass :: HadoopStepConfig -> Lude.Maybe Lude.Text) (\s a -> s {mainClass = a} :: HadoopStepConfig)
{-# DEPRECATED hscMainClass "Use generic-lens or generic-optics with 'mainClass' instead." #-}

-- | The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hscProperties :: Lens.Lens' HadoopStepConfig (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
hscProperties = Lens.lens (properties :: HadoopStepConfig -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {properties = a} :: HadoopStepConfig)
{-# DEPRECATED hscProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

instance Lude.FromJSON HadoopStepConfig where
  parseJSON =
    Lude.withObject
      "HadoopStepConfig"
      ( \x ->
          HadoopStepConfig'
            Lude.<$> (x Lude..:? "Args" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Jar")
            Lude.<*> (x Lude..:? "MainClass")
            Lude.<*> (x Lude..:? "Properties" Lude..!= Lude.mempty)
      )
