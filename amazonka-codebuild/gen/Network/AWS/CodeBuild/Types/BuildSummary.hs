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
-- Module      : Network.AWS.CodeBuild.Types.BuildSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildSummary where

import Network.AWS.CodeBuild.Types.ResolvedArtifact
import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains summary information about a batch build group.
--
-- /See:/ 'newBuildSummary' smart constructor.
data BuildSummary = BuildSummary'
  { -- | An array of @ResolvedArtifact@ objects that represents the secondary
    -- build artifacts for the build group.
    secondaryArtifacts :: Prelude.Maybe [ResolvedArtifact],
    -- | When the build was started, expressed in Unix time format.
    requestedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The batch build ARN.
    arn :: Prelude.Maybe Prelude.Text,
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
    buildStatus :: Prelude.Maybe StatusType,
    -- | A @ResolvedArtifact@ object that represents the primary build artifacts
    -- for the build group.
    primaryArtifact :: Prelude.Maybe ResolvedArtifact
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { secondaryArtifacts = Prelude.Nothing,
      requestedOn = Prelude.Nothing,
      arn = Prelude.Nothing,
      buildStatus = Prelude.Nothing,
      primaryArtifact = Prelude.Nothing
    }

-- | An array of @ResolvedArtifact@ objects that represents the secondary
-- build artifacts for the build group.
buildSummary_secondaryArtifacts :: Lens.Lens' BuildSummary (Prelude.Maybe [ResolvedArtifact])
buildSummary_secondaryArtifacts = Lens.lens (\BuildSummary' {secondaryArtifacts} -> secondaryArtifacts) (\s@BuildSummary' {} a -> s {secondaryArtifacts = a} :: BuildSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | When the build was started, expressed in Unix time format.
buildSummary_requestedOn :: Lens.Lens' BuildSummary (Prelude.Maybe Prelude.UTCTime)
buildSummary_requestedOn = Lens.lens (\BuildSummary' {requestedOn} -> requestedOn) (\s@BuildSummary' {} a -> s {requestedOn = a} :: BuildSummary) Prelude.. Lens.mapping Prelude._Time

-- | The batch build ARN.
buildSummary_arn :: Lens.Lens' BuildSummary (Prelude.Maybe Prelude.Text)
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
buildSummary_buildStatus :: Lens.Lens' BuildSummary (Prelude.Maybe StatusType)
buildSummary_buildStatus = Lens.lens (\BuildSummary' {buildStatus} -> buildStatus) (\s@BuildSummary' {} a -> s {buildStatus = a} :: BuildSummary)

-- | A @ResolvedArtifact@ object that represents the primary build artifacts
-- for the build group.
buildSummary_primaryArtifact :: Lens.Lens' BuildSummary (Prelude.Maybe ResolvedArtifact)
buildSummary_primaryArtifact = Lens.lens (\BuildSummary' {primaryArtifact} -> primaryArtifact) (\s@BuildSummary' {} a -> s {primaryArtifact = a} :: BuildSummary)

instance Prelude.FromJSON BuildSummary where
  parseJSON =
    Prelude.withObject
      "BuildSummary"
      ( \x ->
          BuildSummary'
            Prelude.<$> ( x Prelude..:? "secondaryArtifacts"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "requestedOn")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "buildStatus")
            Prelude.<*> (x Prelude..:? "primaryArtifact")
      )

instance Prelude.Hashable BuildSummary

instance Prelude.NFData BuildSummary
