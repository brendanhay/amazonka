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
-- Module      : Amazonka.EMRContainers.Types.JobTemplateData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.JobTemplateData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types.JobDriver
import Amazonka.EMRContainers.Types.ParametricConfigurationOverrides
import Amazonka.EMRContainers.Types.TemplateParameterConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The values of StartJobRun API requests used in job runs started using
-- the job template.
--
-- /See:/ 'newJobTemplateData' smart constructor.
data JobTemplateData = JobTemplateData'
  { -- | The configuration settings that are used to override defaults
    -- configuration.
    configurationOverrides :: Prelude.Maybe ParametricConfigurationOverrides,
    -- | The tags assigned to jobs started using the job template.
    jobTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The configuration of parameters existing in the job template.
    parameterConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text TemplateParameterConfiguration),
    -- | The execution role ARN of the job run.
    executionRoleArn :: Prelude.Text,
    -- | The release version of Amazon EMR.
    releaseLabel :: Prelude.Text,
    jobDriver :: JobDriver
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobTemplateData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationOverrides', 'jobTemplateData_configurationOverrides' - The configuration settings that are used to override defaults
-- configuration.
--
-- 'jobTags', 'jobTemplateData_jobTags' - The tags assigned to jobs started using the job template.
--
-- 'parameterConfiguration', 'jobTemplateData_parameterConfiguration' - The configuration of parameters existing in the job template.
--
-- 'executionRoleArn', 'jobTemplateData_executionRoleArn' - The execution role ARN of the job run.
--
-- 'releaseLabel', 'jobTemplateData_releaseLabel' - The release version of Amazon EMR.
--
-- 'jobDriver', 'jobTemplateData_jobDriver' - Undocumented member.
newJobTemplateData ::
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'releaseLabel'
  Prelude.Text ->
  -- | 'jobDriver'
  JobDriver ->
  JobTemplateData
newJobTemplateData
  pExecutionRoleArn_
  pReleaseLabel_
  pJobDriver_ =
    JobTemplateData'
      { configurationOverrides =
          Prelude.Nothing,
        jobTags = Prelude.Nothing,
        parameterConfiguration = Prelude.Nothing,
        executionRoleArn = pExecutionRoleArn_,
        releaseLabel = pReleaseLabel_,
        jobDriver = pJobDriver_
      }

-- | The configuration settings that are used to override defaults
-- configuration.
jobTemplateData_configurationOverrides :: Lens.Lens' JobTemplateData (Prelude.Maybe ParametricConfigurationOverrides)
jobTemplateData_configurationOverrides = Lens.lens (\JobTemplateData' {configurationOverrides} -> configurationOverrides) (\s@JobTemplateData' {} a -> s {configurationOverrides = a} :: JobTemplateData)

-- | The tags assigned to jobs started using the job template.
jobTemplateData_jobTags :: Lens.Lens' JobTemplateData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobTemplateData_jobTags = Lens.lens (\JobTemplateData' {jobTags} -> jobTags) (\s@JobTemplateData' {} a -> s {jobTags = a} :: JobTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of parameters existing in the job template.
jobTemplateData_parameterConfiguration :: Lens.Lens' JobTemplateData (Prelude.Maybe (Prelude.HashMap Prelude.Text TemplateParameterConfiguration))
jobTemplateData_parameterConfiguration = Lens.lens (\JobTemplateData' {parameterConfiguration} -> parameterConfiguration) (\s@JobTemplateData' {} a -> s {parameterConfiguration = a} :: JobTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The execution role ARN of the job run.
jobTemplateData_executionRoleArn :: Lens.Lens' JobTemplateData Prelude.Text
jobTemplateData_executionRoleArn = Lens.lens (\JobTemplateData' {executionRoleArn} -> executionRoleArn) (\s@JobTemplateData' {} a -> s {executionRoleArn = a} :: JobTemplateData)

-- | The release version of Amazon EMR.
jobTemplateData_releaseLabel :: Lens.Lens' JobTemplateData Prelude.Text
jobTemplateData_releaseLabel = Lens.lens (\JobTemplateData' {releaseLabel} -> releaseLabel) (\s@JobTemplateData' {} a -> s {releaseLabel = a} :: JobTemplateData)

-- | Undocumented member.
jobTemplateData_jobDriver :: Lens.Lens' JobTemplateData JobDriver
jobTemplateData_jobDriver = Lens.lens (\JobTemplateData' {jobDriver} -> jobDriver) (\s@JobTemplateData' {} a -> s {jobDriver = a} :: JobTemplateData)

instance Data.FromJSON JobTemplateData where
  parseJSON =
    Data.withObject
      "JobTemplateData"
      ( \x ->
          JobTemplateData'
            Prelude.<$> (x Data..:? "configurationOverrides")
            Prelude.<*> (x Data..:? "jobTags" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "parameterConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "executionRoleArn")
            Prelude.<*> (x Data..: "releaseLabel")
            Prelude.<*> (x Data..: "jobDriver")
      )

instance Prelude.Hashable JobTemplateData where
  hashWithSalt _salt JobTemplateData' {..} =
    _salt `Prelude.hashWithSalt` configurationOverrides
      `Prelude.hashWithSalt` jobTags
      `Prelude.hashWithSalt` parameterConfiguration
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` jobDriver

instance Prelude.NFData JobTemplateData where
  rnf JobTemplateData' {..} =
    Prelude.rnf configurationOverrides
      `Prelude.seq` Prelude.rnf jobTags
      `Prelude.seq` Prelude.rnf parameterConfiguration
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf jobDriver

instance Data.ToJSON JobTemplateData where
  toJSON JobTemplateData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configurationOverrides" Data..=)
              Prelude.<$> configurationOverrides,
            ("jobTags" Data..=) Prelude.<$> jobTags,
            ("parameterConfiguration" Data..=)
              Prelude.<$> parameterConfiguration,
            Prelude.Just
              ("executionRoleArn" Data..= executionRoleArn),
            Prelude.Just ("releaseLabel" Data..= releaseLabel),
            Prelude.Just ("jobDriver" Data..= jobDriver)
          ]
      )
