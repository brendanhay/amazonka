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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobArtifacts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobArtifacts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The artifacts that are generated during an AutoML job.
--
-- /See:/ 'newAutoMLJobArtifacts' smart constructor.
data AutoMLJobArtifacts = AutoMLJobArtifacts'
  { -- | The URL of the notebook location.
    candidateDefinitionNotebookLocation :: Prelude.Maybe Prelude.Text,
    -- | The URL of the notebook location.
    dataExplorationNotebookLocation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'candidateDefinitionNotebookLocation', 'autoMLJobArtifacts_candidateDefinitionNotebookLocation' - The URL of the notebook location.
--
-- 'dataExplorationNotebookLocation', 'autoMLJobArtifacts_dataExplorationNotebookLocation' - The URL of the notebook location.
newAutoMLJobArtifacts ::
  AutoMLJobArtifacts
newAutoMLJobArtifacts =
  AutoMLJobArtifacts'
    { candidateDefinitionNotebookLocation =
        Prelude.Nothing,
      dataExplorationNotebookLocation = Prelude.Nothing
    }

-- | The URL of the notebook location.
autoMLJobArtifacts_candidateDefinitionNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Prelude.Maybe Prelude.Text)
autoMLJobArtifacts_candidateDefinitionNotebookLocation = Lens.lens (\AutoMLJobArtifacts' {candidateDefinitionNotebookLocation} -> candidateDefinitionNotebookLocation) (\s@AutoMLJobArtifacts' {} a -> s {candidateDefinitionNotebookLocation = a} :: AutoMLJobArtifacts)

-- | The URL of the notebook location.
autoMLJobArtifacts_dataExplorationNotebookLocation :: Lens.Lens' AutoMLJobArtifacts (Prelude.Maybe Prelude.Text)
autoMLJobArtifacts_dataExplorationNotebookLocation = Lens.lens (\AutoMLJobArtifacts' {dataExplorationNotebookLocation} -> dataExplorationNotebookLocation) (\s@AutoMLJobArtifacts' {} a -> s {dataExplorationNotebookLocation = a} :: AutoMLJobArtifacts)

instance Data.FromJSON AutoMLJobArtifacts where
  parseJSON =
    Data.withObject
      "AutoMLJobArtifacts"
      ( \x ->
          AutoMLJobArtifacts'
            Prelude.<$> (x Data..:? "CandidateDefinitionNotebookLocation")
            Prelude.<*> (x Data..:? "DataExplorationNotebookLocation")
      )

instance Prelude.Hashable AutoMLJobArtifacts where
  hashWithSalt _salt AutoMLJobArtifacts' {..} =
    _salt
      `Prelude.hashWithSalt` candidateDefinitionNotebookLocation
      `Prelude.hashWithSalt` dataExplorationNotebookLocation

instance Prelude.NFData AutoMLJobArtifacts where
  rnf AutoMLJobArtifacts' {..} =
    Prelude.rnf candidateDefinitionNotebookLocation `Prelude.seq`
      Prelude.rnf dataExplorationNotebookLocation
