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
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobArtifacts where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Artifacts that are generation during a job.
--
-- /See:/ 'newAutoMLJobArtifacts' smart constructor.
data AutoMLJobArtifacts = AutoMLJobArtifacts'
  { -- | The URL to the notebook location.
    candidateDefinitionNotebookLocation :: Core.Maybe Core.Text,
    -- | The URL to the notebook location.
    dataExplorationNotebookLocation :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoMLJobArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'candidateDefinitionNotebookLocation', 'autoMLJobArtifacts_candidateDefinitionNotebookLocation' - The URL to the notebook location.
--
-- 'dataExplorationNotebookLocation', 'autoMLJobArtifacts_dataExplorationNotebookLocation' - The URL to the notebook location.
newAutoMLJobArtifacts ::
  AutoMLJobArtifacts
newAutoMLJobArtifacts =
  AutoMLJobArtifacts'
    { candidateDefinitionNotebookLocation =
        Core.Nothing,
      dataExplorationNotebookLocation = Core.Nothing
    }

-- | The URL to the notebook location.
autoMLJobArtifacts_candidateDefinitionNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Core.Maybe Core.Text)
autoMLJobArtifacts_candidateDefinitionNotebookLocation = Lens.lens (\AutoMLJobArtifacts' {candidateDefinitionNotebookLocation} -> candidateDefinitionNotebookLocation) (\s@AutoMLJobArtifacts' {} a -> s {candidateDefinitionNotebookLocation = a} :: AutoMLJobArtifacts)

-- | The URL to the notebook location.
autoMLJobArtifacts_dataExplorationNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Core.Maybe Core.Text)
autoMLJobArtifacts_dataExplorationNotebookLocation = Lens.lens (\AutoMLJobArtifacts' {dataExplorationNotebookLocation} -> dataExplorationNotebookLocation) (\s@AutoMLJobArtifacts' {} a -> s {dataExplorationNotebookLocation = a} :: AutoMLJobArtifacts)

instance Core.FromJSON AutoMLJobArtifacts where
  parseJSON =
    Core.withObject
      "AutoMLJobArtifacts"
      ( \x ->
          AutoMLJobArtifacts'
            Core.<$> (x Core..:? "CandidateDefinitionNotebookLocation")
            Core.<*> (x Core..:? "DataExplorationNotebookLocation")
      )

instance Core.Hashable AutoMLJobArtifacts

instance Core.NFData AutoMLJobArtifacts
