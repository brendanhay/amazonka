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
-- Module      : Amazonka.SageMaker.Types.CandidateProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CandidateProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CandidateArtifactLocations
import Amazonka.SageMaker.Types.MetricDatum

-- | The properties of an AutoML candidate job.
--
-- /See:/ 'newCandidateProperties' smart constructor.
data CandidateProperties = CandidateProperties'
  { -- | The Amazon S3 prefix to the artifacts generated for an AutoML candidate.
    candidateArtifactLocations :: Prelude.Maybe CandidateArtifactLocations,
    -- | Information about the candidate metrics for an AutoML job.
    candidateMetrics :: Prelude.Maybe [MetricDatum]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CandidateProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'candidateArtifactLocations', 'candidateProperties_candidateArtifactLocations' - The Amazon S3 prefix to the artifacts generated for an AutoML candidate.
--
-- 'candidateMetrics', 'candidateProperties_candidateMetrics' - Information about the candidate metrics for an AutoML job.
newCandidateProperties ::
  CandidateProperties
newCandidateProperties =
  CandidateProperties'
    { candidateArtifactLocations =
        Prelude.Nothing,
      candidateMetrics = Prelude.Nothing
    }

-- | The Amazon S3 prefix to the artifacts generated for an AutoML candidate.
candidateProperties_candidateArtifactLocations :: Lens.Lens' CandidateProperties (Prelude.Maybe CandidateArtifactLocations)
candidateProperties_candidateArtifactLocations = Lens.lens (\CandidateProperties' {candidateArtifactLocations} -> candidateArtifactLocations) (\s@CandidateProperties' {} a -> s {candidateArtifactLocations = a} :: CandidateProperties)

-- | Information about the candidate metrics for an AutoML job.
candidateProperties_candidateMetrics :: Lens.Lens' CandidateProperties (Prelude.Maybe [MetricDatum])
candidateProperties_candidateMetrics = Lens.lens (\CandidateProperties' {candidateMetrics} -> candidateMetrics) (\s@CandidateProperties' {} a -> s {candidateMetrics = a} :: CandidateProperties) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CandidateProperties where
  parseJSON =
    Data.withObject
      "CandidateProperties"
      ( \x ->
          CandidateProperties'
            Prelude.<$> (x Data..:? "CandidateArtifactLocations")
            Prelude.<*> ( x
                            Data..:? "CandidateMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CandidateProperties where
  hashWithSalt _salt CandidateProperties' {..} =
    _salt
      `Prelude.hashWithSalt` candidateArtifactLocations
      `Prelude.hashWithSalt` candidateMetrics

instance Prelude.NFData CandidateProperties where
  rnf CandidateProperties' {..} =
    Prelude.rnf candidateArtifactLocations `Prelude.seq`
      Prelude.rnf candidateMetrics
