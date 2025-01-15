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
-- Module      : Amazonka.EMR.Types.HadoopJarStepConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.HadoopJarStepConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.KeyValue
import qualified Amazonka.Prelude as Prelude

-- | A job flow step consisting of a JAR file whose main function will be
-- executed. The main function submits a job for Hadoop to execute and
-- waits for the job to finish or fail.
--
-- /See:/ 'newHadoopJarStepConfig' smart constructor.
data HadoopJarStepConfig = HadoopJarStepConfig'
  { -- | A list of command line arguments passed to the JAR file\'s main function
    -- when executed.
    args :: Prelude.Maybe [Prelude.Text],
    -- | The name of the main class in the specified Java file. If not specified,
    -- the JAR file should specify a Main-Class in its manifest file.
    mainClass :: Prelude.Maybe Prelude.Text,
    -- | A list of Java properties that are set when the step runs. You can use
    -- these properties to pass key-value pairs to your main function.
    properties :: Prelude.Maybe [KeyValue],
    -- | A path to a JAR file run during the step.
    jar :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HadoopJarStepConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'hadoopJarStepConfig_args' - A list of command line arguments passed to the JAR file\'s main function
-- when executed.
--
-- 'mainClass', 'hadoopJarStepConfig_mainClass' - The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a Main-Class in its manifest file.
--
-- 'properties', 'hadoopJarStepConfig_properties' - A list of Java properties that are set when the step runs. You can use
-- these properties to pass key-value pairs to your main function.
--
-- 'jar', 'hadoopJarStepConfig_jar' - A path to a JAR file run during the step.
newHadoopJarStepConfig ::
  -- | 'jar'
  Prelude.Text ->
  HadoopJarStepConfig
newHadoopJarStepConfig pJar_ =
  HadoopJarStepConfig'
    { args = Prelude.Nothing,
      mainClass = Prelude.Nothing,
      properties = Prelude.Nothing,
      jar = pJar_
    }

-- | A list of command line arguments passed to the JAR file\'s main function
-- when executed.
hadoopJarStepConfig_args :: Lens.Lens' HadoopJarStepConfig (Prelude.Maybe [Prelude.Text])
hadoopJarStepConfig_args = Lens.lens (\HadoopJarStepConfig' {args} -> args) (\s@HadoopJarStepConfig' {} a -> s {args = a} :: HadoopJarStepConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a Main-Class in its manifest file.
hadoopJarStepConfig_mainClass :: Lens.Lens' HadoopJarStepConfig (Prelude.Maybe Prelude.Text)
hadoopJarStepConfig_mainClass = Lens.lens (\HadoopJarStepConfig' {mainClass} -> mainClass) (\s@HadoopJarStepConfig' {} a -> s {mainClass = a} :: HadoopJarStepConfig)

-- | A list of Java properties that are set when the step runs. You can use
-- these properties to pass key-value pairs to your main function.
hadoopJarStepConfig_properties :: Lens.Lens' HadoopJarStepConfig (Prelude.Maybe [KeyValue])
hadoopJarStepConfig_properties = Lens.lens (\HadoopJarStepConfig' {properties} -> properties) (\s@HadoopJarStepConfig' {} a -> s {properties = a} :: HadoopJarStepConfig) Prelude.. Lens.mapping Lens.coerced

-- | A path to a JAR file run during the step.
hadoopJarStepConfig_jar :: Lens.Lens' HadoopJarStepConfig Prelude.Text
hadoopJarStepConfig_jar = Lens.lens (\HadoopJarStepConfig' {jar} -> jar) (\s@HadoopJarStepConfig' {} a -> s {jar = a} :: HadoopJarStepConfig)

instance Prelude.Hashable HadoopJarStepConfig where
  hashWithSalt _salt HadoopJarStepConfig' {..} =
    _salt
      `Prelude.hashWithSalt` args
      `Prelude.hashWithSalt` mainClass
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` jar

instance Prelude.NFData HadoopJarStepConfig where
  rnf HadoopJarStepConfig' {..} =
    Prelude.rnf args `Prelude.seq`
      Prelude.rnf mainClass `Prelude.seq`
        Prelude.rnf properties `Prelude.seq`
          Prelude.rnf jar

instance Data.ToJSON HadoopJarStepConfig where
  toJSON HadoopJarStepConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Args" Data..=) Prelude.<$> args,
            ("MainClass" Data..=) Prelude.<$> mainClass,
            ("Properties" Data..=) Prelude.<$> properties,
            Prelude.Just ("Jar" Data..= jar)
          ]
      )
