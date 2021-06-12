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
-- Module      : Network.AWS.EMR.Types.HadoopStepConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.HadoopStepConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A cluster step consisting of a JAR file whose main function will be
-- executed. The main function submits a job for Hadoop to execute and
-- waits for the job to finish or fail.
--
-- /See:/ 'newHadoopStepConfig' smart constructor.
data HadoopStepConfig = HadoopStepConfig'
  { -- | The list of command line arguments to pass to the JAR file\'s main
    -- function for execution.
    args :: Core.Maybe [Core.Text],
    -- | The path to the JAR file that runs during the step.
    jar :: Core.Maybe Core.Text,
    -- | The list of Java properties that are set when the step runs. You can use
    -- these properties to pass key-value pairs to your main function.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the main class in the specified Java file. If not specified,
    -- the JAR file should specify a main class in its manifest file.
    mainClass :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HadoopStepConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'hadoopStepConfig_args' - The list of command line arguments to pass to the JAR file\'s main
-- function for execution.
--
-- 'jar', 'hadoopStepConfig_jar' - The path to the JAR file that runs during the step.
--
-- 'properties', 'hadoopStepConfig_properties' - The list of Java properties that are set when the step runs. You can use
-- these properties to pass key-value pairs to your main function.
--
-- 'mainClass', 'hadoopStepConfig_mainClass' - The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a main class in its manifest file.
newHadoopStepConfig ::
  HadoopStepConfig
newHadoopStepConfig =
  HadoopStepConfig'
    { args = Core.Nothing,
      jar = Core.Nothing,
      properties = Core.Nothing,
      mainClass = Core.Nothing
    }

-- | The list of command line arguments to pass to the JAR file\'s main
-- function for execution.
hadoopStepConfig_args :: Lens.Lens' HadoopStepConfig (Core.Maybe [Core.Text])
hadoopStepConfig_args = Lens.lens (\HadoopStepConfig' {args} -> args) (\s@HadoopStepConfig' {} a -> s {args = a} :: HadoopStepConfig) Core.. Lens.mapping Lens._Coerce

-- | The path to the JAR file that runs during the step.
hadoopStepConfig_jar :: Lens.Lens' HadoopStepConfig (Core.Maybe Core.Text)
hadoopStepConfig_jar = Lens.lens (\HadoopStepConfig' {jar} -> jar) (\s@HadoopStepConfig' {} a -> s {jar = a} :: HadoopStepConfig)

-- | The list of Java properties that are set when the step runs. You can use
-- these properties to pass key-value pairs to your main function.
hadoopStepConfig_properties :: Lens.Lens' HadoopStepConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
hadoopStepConfig_properties = Lens.lens (\HadoopStepConfig' {properties} -> properties) (\s@HadoopStepConfig' {} a -> s {properties = a} :: HadoopStepConfig) Core.. Lens.mapping Lens._Coerce

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a main class in its manifest file.
hadoopStepConfig_mainClass :: Lens.Lens' HadoopStepConfig (Core.Maybe Core.Text)
hadoopStepConfig_mainClass = Lens.lens (\HadoopStepConfig' {mainClass} -> mainClass) (\s@HadoopStepConfig' {} a -> s {mainClass = a} :: HadoopStepConfig)

instance Core.FromJSON HadoopStepConfig where
  parseJSON =
    Core.withObject
      "HadoopStepConfig"
      ( \x ->
          HadoopStepConfig'
            Core.<$> (x Core..:? "Args" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Jar")
            Core.<*> (x Core..:? "Properties" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MainClass")
      )

instance Core.Hashable HadoopStepConfig

instance Core.NFData HadoopStepConfig
