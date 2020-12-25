{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobArtifacts
  ( AutoMLJobArtifacts (..),

    -- * Smart constructor
    mkAutoMLJobArtifacts,

    -- * Lenses
    amljaCandidateDefinitionNotebookLocation,
    amljaDataExplorationNotebookLocation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CandidateDefinitionNotebookLocation as Types
import qualified Network.AWS.SageMaker.Types.DataExplorationNotebookLocation as Types

-- | Artifacts that are generation during a job.
--
-- /See:/ 'mkAutoMLJobArtifacts' smart constructor.
data AutoMLJobArtifacts = AutoMLJobArtifacts'
  { -- | The URL to the notebook location.
    candidateDefinitionNotebookLocation :: Core.Maybe Types.CandidateDefinitionNotebookLocation,
    -- | The URL to the notebook location.
    dataExplorationNotebookLocation :: Core.Maybe Types.DataExplorationNotebookLocation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLJobArtifacts' value with any optional fields omitted.
mkAutoMLJobArtifacts ::
  AutoMLJobArtifacts
mkAutoMLJobArtifacts =
  AutoMLJobArtifacts'
    { candidateDefinitionNotebookLocation =
        Core.Nothing,
      dataExplorationNotebookLocation = Core.Nothing
    }

-- | The URL to the notebook location.
--
-- /Note:/ Consider using 'candidateDefinitionNotebookLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljaCandidateDefinitionNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Core.Maybe Types.CandidateDefinitionNotebookLocation)
amljaCandidateDefinitionNotebookLocation = Lens.field @"candidateDefinitionNotebookLocation"
{-# DEPRECATED amljaCandidateDefinitionNotebookLocation "Use generic-lens or generic-optics with 'candidateDefinitionNotebookLocation' instead." #-}

-- | The URL to the notebook location.
--
-- /Note:/ Consider using 'dataExplorationNotebookLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljaDataExplorationNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Core.Maybe Types.DataExplorationNotebookLocation)
amljaDataExplorationNotebookLocation = Lens.field @"dataExplorationNotebookLocation"
{-# DEPRECATED amljaDataExplorationNotebookLocation "Use generic-lens or generic-optics with 'dataExplorationNotebookLocation' instead." #-}

instance Core.FromJSON AutoMLJobArtifacts where
  parseJSON =
    Core.withObject "AutoMLJobArtifacts" Core.$
      \x ->
        AutoMLJobArtifacts'
          Core.<$> (x Core..:? "CandidateDefinitionNotebookLocation")
          Core.<*> (x Core..:? "DataExplorationNotebookLocation")
