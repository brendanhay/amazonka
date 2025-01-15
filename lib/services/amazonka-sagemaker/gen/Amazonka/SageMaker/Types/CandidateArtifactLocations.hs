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
-- Module      : Amazonka.SageMaker.Types.CandidateArtifactLocations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CandidateArtifactLocations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location of artifacts for an AutoML candidate job.
--
-- /See:/ 'newCandidateArtifactLocations' smart constructor.
data CandidateArtifactLocations = CandidateArtifactLocations'
  { -- | The Amazon S3 prefix to the model insight artifacts generated for the
    -- AutoML candidate.
    modelInsights :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 prefix to the explainability artifacts generated for the
    -- AutoML candidate.
    explainability :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CandidateArtifactLocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelInsights', 'candidateArtifactLocations_modelInsights' - The Amazon S3 prefix to the model insight artifacts generated for the
-- AutoML candidate.
--
-- 'explainability', 'candidateArtifactLocations_explainability' - The Amazon S3 prefix to the explainability artifacts generated for the
-- AutoML candidate.
newCandidateArtifactLocations ::
  -- | 'explainability'
  Prelude.Text ->
  CandidateArtifactLocations
newCandidateArtifactLocations pExplainability_ =
  CandidateArtifactLocations'
    { modelInsights =
        Prelude.Nothing,
      explainability = pExplainability_
    }

-- | The Amazon S3 prefix to the model insight artifacts generated for the
-- AutoML candidate.
candidateArtifactLocations_modelInsights :: Lens.Lens' CandidateArtifactLocations (Prelude.Maybe Prelude.Text)
candidateArtifactLocations_modelInsights = Lens.lens (\CandidateArtifactLocations' {modelInsights} -> modelInsights) (\s@CandidateArtifactLocations' {} a -> s {modelInsights = a} :: CandidateArtifactLocations)

-- | The Amazon S3 prefix to the explainability artifacts generated for the
-- AutoML candidate.
candidateArtifactLocations_explainability :: Lens.Lens' CandidateArtifactLocations Prelude.Text
candidateArtifactLocations_explainability = Lens.lens (\CandidateArtifactLocations' {explainability} -> explainability) (\s@CandidateArtifactLocations' {} a -> s {explainability = a} :: CandidateArtifactLocations)

instance Data.FromJSON CandidateArtifactLocations where
  parseJSON =
    Data.withObject
      "CandidateArtifactLocations"
      ( \x ->
          CandidateArtifactLocations'
            Prelude.<$> (x Data..:? "ModelInsights")
            Prelude.<*> (x Data..: "Explainability")
      )

instance Prelude.Hashable CandidateArtifactLocations where
  hashWithSalt _salt CandidateArtifactLocations' {..} =
    _salt
      `Prelude.hashWithSalt` modelInsights
      `Prelude.hashWithSalt` explainability

instance Prelude.NFData CandidateArtifactLocations where
  rnf CandidateArtifactLocations' {..} =
    Prelude.rnf modelInsights `Prelude.seq`
      Prelude.rnf explainability
