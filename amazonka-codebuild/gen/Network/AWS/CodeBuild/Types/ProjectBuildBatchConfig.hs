{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig where

import Network.AWS.CodeBuild.Types.BatchRestrictions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains configuration information about a batch build project.
--
-- /See:/ 'newProjectBuildBatchConfig' smart constructor.
data ProjectBuildBatchConfig = ProjectBuildBatchConfig'
  { -- | Specifies if the build artifacts for the batch build should be combined
    -- into a single artifact location.
    combineArtifacts :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the service role ARN for the batch build project.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum amount of time, in minutes, that the batch build
    -- must be completed in.
    timeoutInMins :: Prelude.Maybe Prelude.Int,
    -- | A @BatchRestrictions@ object that specifies the restrictions for the
    -- batch build.
    restrictions :: Prelude.Maybe BatchRestrictions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProjectBuildBatchConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'combineArtifacts', 'projectBuildBatchConfig_combineArtifacts' - Specifies if the build artifacts for the batch build should be combined
-- into a single artifact location.
--
-- 'serviceRole', 'projectBuildBatchConfig_serviceRole' - Specifies the service role ARN for the batch build project.
--
-- 'timeoutInMins', 'projectBuildBatchConfig_timeoutInMins' - Specifies the maximum amount of time, in minutes, that the batch build
-- must be completed in.
--
-- 'restrictions', 'projectBuildBatchConfig_restrictions' - A @BatchRestrictions@ object that specifies the restrictions for the
-- batch build.
newProjectBuildBatchConfig ::
  ProjectBuildBatchConfig
newProjectBuildBatchConfig =
  ProjectBuildBatchConfig'
    { combineArtifacts =
        Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      timeoutInMins = Prelude.Nothing,
      restrictions = Prelude.Nothing
    }

-- | Specifies if the build artifacts for the batch build should be combined
-- into a single artifact location.
projectBuildBatchConfig_combineArtifacts :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe Prelude.Bool)
projectBuildBatchConfig_combineArtifacts = Lens.lens (\ProjectBuildBatchConfig' {combineArtifacts} -> combineArtifacts) (\s@ProjectBuildBatchConfig' {} a -> s {combineArtifacts = a} :: ProjectBuildBatchConfig)

-- | Specifies the service role ARN for the batch build project.
projectBuildBatchConfig_serviceRole :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe Prelude.Text)
projectBuildBatchConfig_serviceRole = Lens.lens (\ProjectBuildBatchConfig' {serviceRole} -> serviceRole) (\s@ProjectBuildBatchConfig' {} a -> s {serviceRole = a} :: ProjectBuildBatchConfig)

-- | Specifies the maximum amount of time, in minutes, that the batch build
-- must be completed in.
projectBuildBatchConfig_timeoutInMins :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe Prelude.Int)
projectBuildBatchConfig_timeoutInMins = Lens.lens (\ProjectBuildBatchConfig' {timeoutInMins} -> timeoutInMins) (\s@ProjectBuildBatchConfig' {} a -> s {timeoutInMins = a} :: ProjectBuildBatchConfig)

-- | A @BatchRestrictions@ object that specifies the restrictions for the
-- batch build.
projectBuildBatchConfig_restrictions :: Lens.Lens' ProjectBuildBatchConfig (Prelude.Maybe BatchRestrictions)
projectBuildBatchConfig_restrictions = Lens.lens (\ProjectBuildBatchConfig' {restrictions} -> restrictions) (\s@ProjectBuildBatchConfig' {} a -> s {restrictions = a} :: ProjectBuildBatchConfig)

instance Prelude.FromJSON ProjectBuildBatchConfig where
  parseJSON =
    Prelude.withObject
      "ProjectBuildBatchConfig"
      ( \x ->
          ProjectBuildBatchConfig'
            Prelude.<$> (x Prelude..:? "combineArtifacts")
            Prelude.<*> (x Prelude..:? "serviceRole")
            Prelude.<*> (x Prelude..:? "timeoutInMins")
            Prelude.<*> (x Prelude..:? "restrictions")
      )

instance Prelude.Hashable ProjectBuildBatchConfig

instance Prelude.NFData ProjectBuildBatchConfig

instance Prelude.ToJSON ProjectBuildBatchConfig where
  toJSON ProjectBuildBatchConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("combineArtifacts" Prelude..=)
              Prelude.<$> combineArtifacts,
            ("serviceRole" Prelude..=) Prelude.<$> serviceRole,
            ("timeoutInMins" Prelude..=)
              Prelude.<$> timeoutInMins,
            ("restrictions" Prelude..=)
              Prelude.<$> restrictions
          ]
      )
