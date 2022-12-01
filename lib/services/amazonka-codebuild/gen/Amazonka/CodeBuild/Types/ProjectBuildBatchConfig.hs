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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectBuildBatchConfig where

import Amazonka.CodeBuild.Types.BatchReportModeType
import Amazonka.CodeBuild.Types.BatchRestrictions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information about a batch build project.
--
-- /See:/ 'newProjectBuildBatchConfig' smart constructor.
data ProjectBuildBatchConfig = ProjectBuildBatchConfig'
  { -- | Specifies the maximum amount of time, in minutes, that the batch build
    -- must be completed in.
    timeoutInMins :: Prelude.Maybe Prelude.Int,
    -- | A @BatchRestrictions@ object that specifies the restrictions for the
    -- batch build.
    restrictions :: Prelude.Maybe BatchRestrictions,
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
    batchReportMode :: Prelude.Maybe BatchReportModeType,
    -- | Specifies if the build artifacts for the batch build should be combined
    -- into a single artifact location.
    combineArtifacts :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the service role ARN for the batch build project.
    serviceRole :: Prelude.Maybe Prelude.Text
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
-- 'timeoutInMins', 'projectBuildBatchConfig_timeoutInMins' - Specifies the maximum amount of time, in minutes, that the batch build
-- must be completed in.
--
-- 'restrictions', 'projectBuildBatchConfig_restrictions' - A @BatchRestrictions@ object that specifies the restrictions for the
-- batch build.
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
-- 'serviceRole', 'projectBuildBatchConfig_serviceRole' - Specifies the service role ARN for the batch build project.
newProjectBuildBatchConfig ::
  ProjectBuildBatchConfig
newProjectBuildBatchConfig =
  ProjectBuildBatchConfig'
    { timeoutInMins =
        Prelude.Nothing,
      restrictions = Prelude.Nothing,
      batchReportMode = Prelude.Nothing,
      combineArtifacts = Prelude.Nothing,
      serviceRole = Prelude.Nothing
    }

-- | Specifies the maximum amount of time, in minutes, that the batch build
-- must be completed in.
projectBuildBatchConfig_timeoutInMins :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe Prelude.Int)
projectBuildBatchConfig_timeoutInMins = Lens.lens (\ProjectBuildBatchConfig' {timeoutInMins} -> timeoutInMins) (\s@ProjectBuildBatchConfig' {} a -> s {timeoutInMins = a} :: ProjectBuildBatchConfig)

-- | A @BatchRestrictions@ object that specifies the restrictions for the
-- batch build.
projectBuildBatchConfig_restrictions :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe BatchRestrictions)
projectBuildBatchConfig_restrictions = Lens.lens (\ProjectBuildBatchConfig' {restrictions} -> restrictions) (\s@ProjectBuildBatchConfig' {} a -> s {restrictions = a} :: ProjectBuildBatchConfig)

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

-- | Specifies the service role ARN for the batch build project.
projectBuildBatchConfig_serviceRole :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe Prelude.Text)
projectBuildBatchConfig_serviceRole = Lens.lens (\ProjectBuildBatchConfig' {serviceRole} -> serviceRole) (\s@ProjectBuildBatchConfig' {} a -> s {serviceRole = a} :: ProjectBuildBatchConfig)

instance Core.FromJSON ProjectBuildBatchConfig where
  parseJSON =
    Core.withObject
      "ProjectBuildBatchConfig"
      ( \x ->
          ProjectBuildBatchConfig'
            Prelude.<$> (x Core..:? "timeoutInMins")
            Prelude.<*> (x Core..:? "restrictions")
            Prelude.<*> (x Core..:? "batchReportMode")
            Prelude.<*> (x Core..:? "combineArtifacts")
            Prelude.<*> (x Core..:? "serviceRole")
      )

instance Prelude.Hashable ProjectBuildBatchConfig where
  hashWithSalt _salt ProjectBuildBatchConfig' {..} =
    _salt `Prelude.hashWithSalt` timeoutInMins
      `Prelude.hashWithSalt` restrictions
      `Prelude.hashWithSalt` batchReportMode
      `Prelude.hashWithSalt` combineArtifacts
      `Prelude.hashWithSalt` serviceRole

instance Prelude.NFData ProjectBuildBatchConfig where
  rnf ProjectBuildBatchConfig' {..} =
    Prelude.rnf timeoutInMins
      `Prelude.seq` Prelude.rnf restrictions
      `Prelude.seq` Prelude.rnf batchReportMode
      `Prelude.seq` Prelude.rnf combineArtifacts
      `Prelude.seq` Prelude.rnf serviceRole

instance Core.ToJSON ProjectBuildBatchConfig where
  toJSON ProjectBuildBatchConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timeoutInMins" Core..=) Prelude.<$> timeoutInMins,
            ("restrictions" Core..=) Prelude.<$> restrictions,
            ("batchReportMode" Core..=)
              Prelude.<$> batchReportMode,
            ("combineArtifacts" Core..=)
              Prelude.<$> combineArtifacts,
            ("serviceRole" Core..=) Prelude.<$> serviceRole
          ]
      )
