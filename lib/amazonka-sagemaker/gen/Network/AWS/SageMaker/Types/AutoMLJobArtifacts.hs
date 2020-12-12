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
import qualified Network.AWS.Prelude as Lude

-- | Artifacts that are generation during a job.
--
-- /See:/ 'mkAutoMLJobArtifacts' smart constructor.
data AutoMLJobArtifacts = AutoMLJobArtifacts'
  { candidateDefinitionNotebookLocation ::
      Lude.Maybe Lude.Text,
    dataExplorationNotebookLocation ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLJobArtifacts' with the minimum fields required to make a request.
--
-- * 'candidateDefinitionNotebookLocation' - The URL to the notebook location.
-- * 'dataExplorationNotebookLocation' - The URL to the notebook location.
mkAutoMLJobArtifacts ::
  AutoMLJobArtifacts
mkAutoMLJobArtifacts =
  AutoMLJobArtifacts'
    { candidateDefinitionNotebookLocation =
        Lude.Nothing,
      dataExplorationNotebookLocation = Lude.Nothing
    }

-- | The URL to the notebook location.
--
-- /Note:/ Consider using 'candidateDefinitionNotebookLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljaCandidateDefinitionNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Lude.Maybe Lude.Text)
amljaCandidateDefinitionNotebookLocation = Lens.lens (candidateDefinitionNotebookLocation :: AutoMLJobArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {candidateDefinitionNotebookLocation = a} :: AutoMLJobArtifacts)
{-# DEPRECATED amljaCandidateDefinitionNotebookLocation "Use generic-lens or generic-optics with 'candidateDefinitionNotebookLocation' instead." #-}

-- | The URL to the notebook location.
--
-- /Note:/ Consider using 'dataExplorationNotebookLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljaDataExplorationNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Lude.Maybe Lude.Text)
amljaDataExplorationNotebookLocation = Lens.lens (dataExplorationNotebookLocation :: AutoMLJobArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {dataExplorationNotebookLocation = a} :: AutoMLJobArtifacts)
{-# DEPRECATED amljaDataExplorationNotebookLocation "Use generic-lens or generic-optics with 'dataExplorationNotebookLocation' instead." #-}

instance Lude.FromJSON AutoMLJobArtifacts where
  parseJSON =
    Lude.withObject
      "AutoMLJobArtifacts"
      ( \x ->
          AutoMLJobArtifacts'
            Lude.<$> (x Lude..:? "CandidateDefinitionNotebookLocation")
            Lude.<*> (x Lude..:? "DataExplorationNotebookLocation")
      )
