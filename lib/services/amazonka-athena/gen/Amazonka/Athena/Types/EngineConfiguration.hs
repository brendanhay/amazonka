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
-- Module      : Amazonka.Athena.Types.EngineConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.EngineConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains data processing unit (DPU) configuration settings and parameter
-- mappings for a notebook engine.
--
-- /See:/ 'newEngineConfiguration' smart constructor.
data EngineConfiguration = EngineConfiguration'
  { -- | Contains additional notebook engine @MAP\<string, string>@ parameter
    -- mappings in the form of key-value pairs. To specify an Athena notebook
    -- that the Jupyter server will download and serve, specify a value for the
    -- StartSessionRequest$NotebookVersion field, and then add a key named
    -- @NotebookId@ to @AdditionalConfigs@ that has the value of the Athena
    -- notebook ID.
    additionalConfigs :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of DPUs to use for the coordinator. A coordinator is a
    -- special executor that orchestrates processing work and manages other
    -- executors in a notebook session. The default is 1.
    coordinatorDpuSize :: Prelude.Maybe Prelude.Natural,
    -- | The default number of DPUs to use for executors. An executor is the
    -- smallest unit of compute that a notebook session can request from
    -- Athena. The default is 1.
    defaultExecutorDpuSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies custom jar files and Spark properties for use cases like
    -- cluster encryption, table formats, and general Spark tuning.
    sparkProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The maximum number of DPUs that can run concurrently.
    maxConcurrentDpus :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EngineConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConfigs', 'engineConfiguration_additionalConfigs' - Contains additional notebook engine @MAP\<string, string>@ parameter
-- mappings in the form of key-value pairs. To specify an Athena notebook
-- that the Jupyter server will download and serve, specify a value for the
-- StartSessionRequest$NotebookVersion field, and then add a key named
-- @NotebookId@ to @AdditionalConfigs@ that has the value of the Athena
-- notebook ID.
--
-- 'coordinatorDpuSize', 'engineConfiguration_coordinatorDpuSize' - The number of DPUs to use for the coordinator. A coordinator is a
-- special executor that orchestrates processing work and manages other
-- executors in a notebook session. The default is 1.
--
-- 'defaultExecutorDpuSize', 'engineConfiguration_defaultExecutorDpuSize' - The default number of DPUs to use for executors. An executor is the
-- smallest unit of compute that a notebook session can request from
-- Athena. The default is 1.
--
-- 'sparkProperties', 'engineConfiguration_sparkProperties' - Specifies custom jar files and Spark properties for use cases like
-- cluster encryption, table formats, and general Spark tuning.
--
-- 'maxConcurrentDpus', 'engineConfiguration_maxConcurrentDpus' - The maximum number of DPUs that can run concurrently.
newEngineConfiguration ::
  -- | 'maxConcurrentDpus'
  Prelude.Natural ->
  EngineConfiguration
newEngineConfiguration pMaxConcurrentDpus_ =
  EngineConfiguration'
    { additionalConfigs =
        Prelude.Nothing,
      coordinatorDpuSize = Prelude.Nothing,
      defaultExecutorDpuSize = Prelude.Nothing,
      sparkProperties = Prelude.Nothing,
      maxConcurrentDpus = pMaxConcurrentDpus_
    }

-- | Contains additional notebook engine @MAP\<string, string>@ parameter
-- mappings in the form of key-value pairs. To specify an Athena notebook
-- that the Jupyter server will download and serve, specify a value for the
-- StartSessionRequest$NotebookVersion field, and then add a key named
-- @NotebookId@ to @AdditionalConfigs@ that has the value of the Athena
-- notebook ID.
engineConfiguration_additionalConfigs :: Lens.Lens' EngineConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
engineConfiguration_additionalConfigs = Lens.lens (\EngineConfiguration' {additionalConfigs} -> additionalConfigs) (\s@EngineConfiguration' {} a -> s {additionalConfigs = a} :: EngineConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The number of DPUs to use for the coordinator. A coordinator is a
-- special executor that orchestrates processing work and manages other
-- executors in a notebook session. The default is 1.
engineConfiguration_coordinatorDpuSize :: Lens.Lens' EngineConfiguration (Prelude.Maybe Prelude.Natural)
engineConfiguration_coordinatorDpuSize = Lens.lens (\EngineConfiguration' {coordinatorDpuSize} -> coordinatorDpuSize) (\s@EngineConfiguration' {} a -> s {coordinatorDpuSize = a} :: EngineConfiguration)

-- | The default number of DPUs to use for executors. An executor is the
-- smallest unit of compute that a notebook session can request from
-- Athena. The default is 1.
engineConfiguration_defaultExecutorDpuSize :: Lens.Lens' EngineConfiguration (Prelude.Maybe Prelude.Natural)
engineConfiguration_defaultExecutorDpuSize = Lens.lens (\EngineConfiguration' {defaultExecutorDpuSize} -> defaultExecutorDpuSize) (\s@EngineConfiguration' {} a -> s {defaultExecutorDpuSize = a} :: EngineConfiguration)

-- | Specifies custom jar files and Spark properties for use cases like
-- cluster encryption, table formats, and general Spark tuning.
engineConfiguration_sparkProperties :: Lens.Lens' EngineConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
engineConfiguration_sparkProperties = Lens.lens (\EngineConfiguration' {sparkProperties} -> sparkProperties) (\s@EngineConfiguration' {} a -> s {sparkProperties = a} :: EngineConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of DPUs that can run concurrently.
engineConfiguration_maxConcurrentDpus :: Lens.Lens' EngineConfiguration Prelude.Natural
engineConfiguration_maxConcurrentDpus = Lens.lens (\EngineConfiguration' {maxConcurrentDpus} -> maxConcurrentDpus) (\s@EngineConfiguration' {} a -> s {maxConcurrentDpus = a} :: EngineConfiguration)

instance Data.FromJSON EngineConfiguration where
  parseJSON =
    Data.withObject
      "EngineConfiguration"
      ( \x ->
          EngineConfiguration'
            Prelude.<$> ( x
                            Data..:? "AdditionalConfigs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CoordinatorDpuSize")
            Prelude.<*> (x Data..:? "DefaultExecutorDpuSize")
            Prelude.<*> ( x
                            Data..:? "SparkProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "MaxConcurrentDpus")
      )

instance Prelude.Hashable EngineConfiguration where
  hashWithSalt _salt EngineConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` additionalConfigs
      `Prelude.hashWithSalt` coordinatorDpuSize
      `Prelude.hashWithSalt` defaultExecutorDpuSize
      `Prelude.hashWithSalt` sparkProperties
      `Prelude.hashWithSalt` maxConcurrentDpus

instance Prelude.NFData EngineConfiguration where
  rnf EngineConfiguration' {..} =
    Prelude.rnf additionalConfigs
      `Prelude.seq` Prelude.rnf coordinatorDpuSize
      `Prelude.seq` Prelude.rnf defaultExecutorDpuSize
      `Prelude.seq` Prelude.rnf sparkProperties
      `Prelude.seq` Prelude.rnf maxConcurrentDpus

instance Data.ToJSON EngineConfiguration where
  toJSON EngineConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalConfigs" Data..=)
              Prelude.<$> additionalConfigs,
            ("CoordinatorDpuSize" Data..=)
              Prelude.<$> coordinatorDpuSize,
            ("DefaultExecutorDpuSize" Data..=)
              Prelude.<$> defaultExecutorDpuSize,
            ("SparkProperties" Data..=)
              Prelude.<$> sparkProperties,
            Prelude.Just
              ("MaxConcurrentDpus" Data..= maxConcurrentDpus)
          ]
      )
