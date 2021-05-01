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
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobArtifacts where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Artifacts that are generation during a job.
--
-- /See:/ 'newAutoMLJobArtifacts' smart constructor.
data AutoMLJobArtifacts = AutoMLJobArtifacts'
  { -- | The URL to the notebook location.
    candidateDefinitionNotebookLocation :: Prelude.Maybe Prelude.Text,
    -- | The URL to the notebook location.
    dataExplorationNotebookLocation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      dataExplorationNotebookLocation = Prelude.Nothing
    }

-- | The URL to the notebook location.
autoMLJobArtifacts_candidateDefinitionNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Prelude.Maybe Prelude.Text)
autoMLJobArtifacts_candidateDefinitionNotebookLocation = Lens.lens (\AutoMLJobArtifacts' {candidateDefinitionNotebookLocation} -> candidateDefinitionNotebookLocation) (\s@AutoMLJobArtifacts' {} a -> s {candidateDefinitionNotebookLocation = a} :: AutoMLJobArtifacts)

-- | The URL to the notebook location.
autoMLJobArtifacts_dataExplorationNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Prelude.Maybe Prelude.Text)
autoMLJobArtifacts_dataExplorationNotebookLocation = Lens.lens (\AutoMLJobArtifacts' {dataExplorationNotebookLocation} -> dataExplorationNotebookLocation) (\s@AutoMLJobArtifacts' {} a -> s {dataExplorationNotebookLocation = a} :: AutoMLJobArtifacts)

instance Prelude.FromJSON AutoMLJobArtifacts where
  parseJSON =
    Prelude.withObject
      "AutoMLJobArtifacts"
      ( \x ->
          AutoMLJobArtifacts'
            Prelude.<$> (x Prelude..:? "CandidateDefinitionNotebookLocation")
            Prelude.<*> (x Prelude..:? "DataExplorationNotebookLocation")
      )

instance Prelude.Hashable AutoMLJobArtifacts

instance Prelude.NFData AutoMLJobArtifacts
