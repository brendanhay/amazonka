-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.HadoopJARStepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.HadoopJARStepConfig
  ( HadoopJARStepConfig (..),

    -- * Smart constructor
    mkHadoopJARStepConfig,

    -- * Lenses
    hjscArgs,
    hjscMainClass,
    hjscProperties,
    hjscJAR,
  )
where

import Network.AWS.EMR.Types.KeyValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A job flow step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
-- /See:/ 'mkHadoopJARStepConfig' smart constructor.
data HadoopJARStepConfig = HadoopJARStepConfig'
  { args ::
      Lude.Maybe [Lude.Text],
    mainClass :: Lude.Maybe Lude.Text,
    properties :: Lude.Maybe [KeyValue],
    jar :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HadoopJARStepConfig' with the minimum fields required to make a request.
--
-- * 'args' - A list of command line arguments passed to the JAR file's main function when executed.
-- * 'jar' - A path to a JAR file run during the step.
-- * 'mainClass' - The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
-- * 'properties' - A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
mkHadoopJARStepConfig ::
  -- | 'jar'
  Lude.Text ->
  HadoopJARStepConfig
mkHadoopJARStepConfig pJAR_ =
  HadoopJARStepConfig'
    { args = Lude.Nothing,
      mainClass = Lude.Nothing,
      properties = Lude.Nothing,
      jar = pJAR_
    }

-- | A list of command line arguments passed to the JAR file's main function when executed.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjscArgs :: Lens.Lens' HadoopJARStepConfig (Lude.Maybe [Lude.Text])
hjscArgs = Lens.lens (args :: HadoopJARStepConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {args = a} :: HadoopJARStepConfig)
{-# DEPRECATED hjscArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
--
-- /Note:/ Consider using 'mainClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjscMainClass :: Lens.Lens' HadoopJARStepConfig (Lude.Maybe Lude.Text)
hjscMainClass = Lens.lens (mainClass :: HadoopJARStepConfig -> Lude.Maybe Lude.Text) (\s a -> s {mainClass = a} :: HadoopJARStepConfig)
{-# DEPRECATED hjscMainClass "Use generic-lens or generic-optics with 'mainClass' instead." #-}

-- | A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjscProperties :: Lens.Lens' HadoopJARStepConfig (Lude.Maybe [KeyValue])
hjscProperties = Lens.lens (properties :: HadoopJARStepConfig -> Lude.Maybe [KeyValue]) (\s a -> s {properties = a} :: HadoopJARStepConfig)
{-# DEPRECATED hjscProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

-- | A path to a JAR file run during the step.
--
-- /Note:/ Consider using 'jar' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjscJAR :: Lens.Lens' HadoopJARStepConfig Lude.Text
hjscJAR = Lens.lens (jar :: HadoopJARStepConfig -> Lude.Text) (\s a -> s {jar = a} :: HadoopJARStepConfig)
{-# DEPRECATED hjscJAR "Use generic-lens or generic-optics with 'jar' instead." #-}

instance Lude.ToJSON HadoopJARStepConfig where
  toJSON HadoopJARStepConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Args" Lude..=) Lude.<$> args,
            ("MainClass" Lude..=) Lude.<$> mainClass,
            ("Properties" Lude..=) Lude.<$> properties,
            Lude.Just ("Jar" Lude..= jar)
          ]
      )
