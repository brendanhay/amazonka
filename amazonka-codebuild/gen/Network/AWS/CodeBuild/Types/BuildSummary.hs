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
-- Module      : Network.AWS.CodeBuild.Types.BuildSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildSummary where

import Network.AWS.CodeBuild.Types.ResolvedArtifact
import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary information about a batch build group.
--
-- /See:/ 'newBuildSummary' smart constructor.
data BuildSummary = BuildSummary'
  { -- | An array of @ResolvedArtifact@ objects that represents the secondary
    -- build artifacts for the build group.
    secondaryArtifacts :: Core.Maybe [ResolvedArtifact],
    -- | When the build was started, expressed in Unix time format.
    requestedOn :: Core.Maybe Core.POSIX,
    -- | The batch build ARN.
    arn :: Core.Maybe Core.Text,
    -- | The status of the build group.
    --
    -- [FAILED]
    --     The build group failed.
    --
    -- [FAULT]
    --     The build group faulted.
    --
    -- [IN_PROGRESS]
    --     The build group is still in progress.
    --
    -- [STOPPED]
    --     The build group stopped.
    --
    -- [SUCCEEDED]
    --     The build group succeeded.
    --
    -- [TIMED_OUT]
    --     The build group timed out.
    buildStatus :: Core.Maybe StatusType,
    -- | A @ResolvedArtifact@ object that represents the primary build artifacts
    -- for the build group.
    primaryArtifact :: Core.Maybe ResolvedArtifact
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BuildSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secondaryArtifacts', 'buildSummary_secondaryArtifacts' - An array of @ResolvedArtifact@ objects that represents the secondary
-- build artifacts for the build group.
--
-- 'requestedOn', 'buildSummary_requestedOn' - When the build was started, expressed in Unix time format.
--
-- 'arn', 'buildSummary_arn' - The batch build ARN.
--
-- 'buildStatus', 'buildSummary_buildStatus' - The status of the build group.
--
-- [FAILED]
--     The build group failed.
--
-- [FAULT]
--     The build group faulted.
--
-- [IN_PROGRESS]
--     The build group is still in progress.
--
-- [STOPPED]
--     The build group stopped.
--
-- [SUCCEEDED]
--     The build group succeeded.
--
-- [TIMED_OUT]
--     The build group timed out.
--
-- 'primaryArtifact', 'buildSummary_primaryArtifact' - A @ResolvedArtifact@ object that represents the primary build artifacts
-- for the build group.
newBuildSummary ::
  BuildSummary
newBuildSummary =
  BuildSummary'
    { secondaryArtifacts = Core.Nothing,
      requestedOn = Core.Nothing,
      arn = Core.Nothing,
      buildStatus = Core.Nothing,
      primaryArtifact = Core.Nothing
    }

-- | An array of @ResolvedArtifact@ objects that represents the secondary
-- build artifacts for the build group.
buildSummary_secondaryArtifacts :: Lens.Lens' BuildSummary (Core.Maybe [ResolvedArtifact])
buildSummary_secondaryArtifacts = Lens.lens (\BuildSummary' {secondaryArtifacts} -> secondaryArtifacts) (\s@BuildSummary' {} a -> s {secondaryArtifacts = a} :: BuildSummary) Core.. Lens.mapping Lens._Coerce

-- | When the build was started, expressed in Unix time format.
buildSummary_requestedOn :: Lens.Lens' BuildSummary (Core.Maybe Core.UTCTime)
buildSummary_requestedOn = Lens.lens (\BuildSummary' {requestedOn} -> requestedOn) (\s@BuildSummary' {} a -> s {requestedOn = a} :: BuildSummary) Core.. Lens.mapping Core._Time

-- | The batch build ARN.
buildSummary_arn :: Lens.Lens' BuildSummary (Core.Maybe Core.Text)
buildSummary_arn = Lens.lens (\BuildSummary' {arn} -> arn) (\s@BuildSummary' {} a -> s {arn = a} :: BuildSummary)

-- | The status of the build group.
--
-- [FAILED]
--     The build group failed.
--
-- [FAULT]
--     The build group faulted.
--
-- [IN_PROGRESS]
--     The build group is still in progress.
--
-- [STOPPED]
--     The build group stopped.
--
-- [SUCCEEDED]
--     The build group succeeded.
--
-- [TIMED_OUT]
--     The build group timed out.
buildSummary_buildStatus :: Lens.Lens' BuildSummary (Core.Maybe StatusType)
buildSummary_buildStatus = Lens.lens (\BuildSummary' {buildStatus} -> buildStatus) (\s@BuildSummary' {} a -> s {buildStatus = a} :: BuildSummary)

-- | A @ResolvedArtifact@ object that represents the primary build artifacts
-- for the build group.
buildSummary_primaryArtifact :: Lens.Lens' BuildSummary (Core.Maybe ResolvedArtifact)
buildSummary_primaryArtifact = Lens.lens (\BuildSummary' {primaryArtifact} -> primaryArtifact) (\s@BuildSummary' {} a -> s {primaryArtifact = a} :: BuildSummary)

instance Core.FromJSON BuildSummary where
  parseJSON =
    Core.withObject
      "BuildSummary"
      ( \x ->
          BuildSummary'
            Core.<$> ( x Core..:? "secondaryArtifacts"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "requestedOn")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "buildStatus")
            Core.<*> (x Core..:? "primaryArtifact")
      )

instance Core.Hashable BuildSummary

instance Core.NFData BuildSummary
