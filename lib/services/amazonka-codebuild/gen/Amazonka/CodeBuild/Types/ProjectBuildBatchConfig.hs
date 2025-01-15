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
-- Module      : Amazonka.CodeBuild.Types.ProjectBuildBatchConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectBuildBatchConfig where

import Amazonka.CodeBuild.Types.BatchReportModeType
import Amazonka.CodeBuild.Types.BatchRestrictions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information about a batch build project.
--
-- /See:/ 'newProjectBuildBatchConfig' smart constructor.
data ProjectBuildBatchConfig = ProjectBuildBatchConfig'
  { -- | Specifies how build status reports are sent to the source provider for
    -- the batch build. This property is only used when the source provider for
    -- your project is Bitbucket, GitHub, or GitHub Enterprise, and your
    -- project is configured to report build statuses to the source provider.
    --
    -- [REPORT_AGGREGATED_BATCH]
    --     (Default) Aggregate all of the build statuses into a single status
    --     report.
    --
    -- [REPORT_INDIVIDUAL_BUILDS]
    --     Send a separate status report for each individual build.
    batchReportMode :: Prelude.Maybe BatchReportModeType,
    -- | Specifies if the build artifacts for the batch build should be combined
    -- into a single artifact location.
    combineArtifacts :: Prelude.Maybe Prelude.Bool,
    -- | A @BatchRestrictions@ object that specifies the restrictions for the
    -- batch build.
    restrictions :: Prelude.Maybe BatchRestrictions,
    -- | Specifies the service role ARN for the batch build project.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum amount of time, in minutes, that the batch build
    -- must be completed in.
    timeoutInMins :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectBuildBatchConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchReportMode', 'projectBuildBatchConfig_batchReportMode' - Specifies how build status reports are sent to the source provider for
-- the batch build. This property is only used when the source provider for
-- your project is Bitbucket, GitHub, or GitHub Enterprise, and your
-- project is configured to report build statuses to the source provider.
--
-- [REPORT_AGGREGATED_BATCH]
--     (Default) Aggregate all of the build statuses into a single status
--     report.
--
-- [REPORT_INDIVIDUAL_BUILDS]
--     Send a separate status report for each individual build.
--
-- 'combineArtifacts', 'projectBuildBatchConfig_combineArtifacts' - Specifies if the build artifacts for the batch build should be combined
-- into a single artifact location.
--
-- 'restrictions', 'projectBuildBatchConfig_restrictions' - A @BatchRestrictions@ object that specifies the restrictions for the
-- batch build.
--
-- 'serviceRole', 'projectBuildBatchConfig_serviceRole' - Specifies the service role ARN for the batch build project.
--
-- 'timeoutInMins', 'projectBuildBatchConfig_timeoutInMins' - Specifies the maximum amount of time, in minutes, that the batch build
-- must be completed in.
newProjectBuildBatchConfig ::
  ProjectBuildBatchConfig
newProjectBuildBatchConfig =
  ProjectBuildBatchConfig'
    { batchReportMode =
        Prelude.Nothing,
      combineArtifacts = Prelude.Nothing,
      restrictions = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      timeoutInMins = Prelude.Nothing
    }

-- | Specifies how build status reports are sent to the source provider for
-- the batch build. This property is only used when the source provider for
-- your project is Bitbucket, GitHub, or GitHub Enterprise, and your
-- project is configured to report build statuses to the source provider.
--
-- [REPORT_AGGREGATED_BATCH]
--     (Default) Aggregate all of the build statuses into a single status
--     report.
--
-- [REPORT_INDIVIDUAL_BUILDS]
--     Send a separate status report for each individual build.
projectBuildBatchConfig_batchReportMode :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe BatchReportModeType)
projectBuildBatchConfig_batchReportMode = Lens.lens (\ProjectBuildBatchConfig' {batchReportMode} -> batchReportMode) (\s@ProjectBuildBatchConfig' {} a -> s {batchReportMode = a} :: ProjectBuildBatchConfig)

-- | Specifies if the build artifacts for the batch build should be combined
-- into a single artifact location.
projectBuildBatchConfig_combineArtifacts :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe Prelude.Bool)
projectBuildBatchConfig_combineArtifacts = Lens.lens (\ProjectBuildBatchConfig' {combineArtifacts} -> combineArtifacts) (\s@ProjectBuildBatchConfig' {} a -> s {combineArtifacts = a} :: ProjectBuildBatchConfig)

-- | A @BatchRestrictions@ object that specifies the restrictions for the
-- batch build.
projectBuildBatchConfig_restrictions :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe BatchRestrictions)
projectBuildBatchConfig_restrictions = Lens.lens (\ProjectBuildBatchConfig' {restrictions} -> restrictions) (\s@ProjectBuildBatchConfig' {} a -> s {restrictions = a} :: ProjectBuildBatchConfig)

-- | Specifies the service role ARN for the batch build project.
projectBuildBatchConfig_serviceRole :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe Prelude.Text)
projectBuildBatchConfig_serviceRole = Lens.lens (\ProjectBuildBatchConfig' {serviceRole} -> serviceRole) (\s@ProjectBuildBatchConfig' {} a -> s {serviceRole = a} :: ProjectBuildBatchConfig)

-- | Specifies the maximum amount of time, in minutes, that the batch build
-- must be completed in.
projectBuildBatchConfig_timeoutInMins :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe Prelude.Int)
projectBuildBatchConfig_timeoutInMins = Lens.lens (\ProjectBuildBatchConfig' {timeoutInMins} -> timeoutInMins) (\s@ProjectBuildBatchConfig' {} a -> s {timeoutInMins = a} :: ProjectBuildBatchConfig)

instance Data.FromJSON ProjectBuildBatchConfig where
  parseJSON =
    Data.withObject
      "ProjectBuildBatchConfig"
      ( \x ->
          ProjectBuildBatchConfig'
            Prelude.<$> (x Data..:? "batchReportMode")
            Prelude.<*> (x Data..:? "combineArtifacts")
            Prelude.<*> (x Data..:? "restrictions")
            Prelude.<*> (x Data..:? "serviceRole")
            Prelude.<*> (x Data..:? "timeoutInMins")
      )

instance Prelude.Hashable ProjectBuildBatchConfig where
  hashWithSalt _salt ProjectBuildBatchConfig' {..} =
    _salt
      `Prelude.hashWithSalt` batchReportMode
      `Prelude.hashWithSalt` combineArtifacts
      `Prelude.hashWithSalt` restrictions
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` timeoutInMins

instance Prelude.NFData ProjectBuildBatchConfig where
  rnf ProjectBuildBatchConfig' {..} =
    Prelude.rnf batchReportMode `Prelude.seq`
      Prelude.rnf combineArtifacts `Prelude.seq`
        Prelude.rnf restrictions `Prelude.seq`
          Prelude.rnf serviceRole `Prelude.seq`
            Prelude.rnf timeoutInMins

instance Data.ToJSON ProjectBuildBatchConfig where
  toJSON ProjectBuildBatchConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("batchReportMode" Data..=)
              Prelude.<$> batchReportMode,
            ("combineArtifacts" Data..=)
              Prelude.<$> combineArtifacts,
            ("restrictions" Data..=) Prelude.<$> restrictions,
            ("serviceRole" Data..=) Prelude.<$> serviceRole,
            ("timeoutInMins" Data..=) Prelude.<$> timeoutInMins
          ]
      )
