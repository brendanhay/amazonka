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
-- Module      : Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information about the accuracy of an entity recognizer for a
-- specific entity type.
--
-- /See:/ 'newEntityTypesEvaluationMetrics' smart constructor.
data EntityTypesEvaluationMetrics = EntityTypesEvaluationMetrics'
  { -- | A measure of how accurate the recognizer results are for a specific
    -- entity type in the test data. It is derived from the @Precision@ and
    -- @Recall@ values. The @F1Score@ is the harmonic average of the two
    -- scores. The highest score is 1, and the worst score is 0.
    f1Score :: Prelude.Maybe Prelude.Double,
    -- | A measure of the usefulness of the recognizer results for a specific
    -- entity type in the test data. High precision means that the recognizer
    -- returned substantially more relevant results than irrelevant ones.
    precision :: Prelude.Maybe Prelude.Double,
    -- | A measure of how complete the recognizer results are for a specific
    -- entity type in the test data. High recall means that the recognizer
    -- returned most of the relevant results.
    recall :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EntityTypesEvaluationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'f1Score', 'entityTypesEvaluationMetrics_f1Score' - A measure of how accurate the recognizer results are for a specific
-- entity type in the test data. It is derived from the @Precision@ and
-- @Recall@ values. The @F1Score@ is the harmonic average of the two
-- scores. The highest score is 1, and the worst score is 0.
--
-- 'precision', 'entityTypesEvaluationMetrics_precision' - A measure of the usefulness of the recognizer results for a specific
-- entity type in the test data. High precision means that the recognizer
-- returned substantially more relevant results than irrelevant ones.
--
-- 'recall', 'entityTypesEvaluationMetrics_recall' - A measure of how complete the recognizer results are for a specific
-- entity type in the test data. High recall means that the recognizer
-- returned most of the relevant results.
newEntityTypesEvaluationMetrics ::
  EntityTypesEvaluationMetrics
newEntityTypesEvaluationMetrics =
  EntityTypesEvaluationMetrics'
    { f1Score =
        Prelude.Nothing,
      precision = Prelude.Nothing,
      recall = Prelude.Nothing
    }

-- | A measure of how accurate the recognizer results are for a specific
-- entity type in the test data. It is derived from the @Precision@ and
-- @Recall@ values. The @F1Score@ is the harmonic average of the two
-- scores. The highest score is 1, and the worst score is 0.
entityTypesEvaluationMetrics_f1Score :: Lens.Lens' EntityTypesEvaluationMetrics (Prelude.Maybe Prelude.Double)
entityTypesEvaluationMetrics_f1Score = Lens.lens (\EntityTypesEvaluationMetrics' {f1Score} -> f1Score) (\s@EntityTypesEvaluationMetrics' {} a -> s {f1Score = a} :: EntityTypesEvaluationMetrics)

-- | A measure of the usefulness of the recognizer results for a specific
-- entity type in the test data. High precision means that the recognizer
-- returned substantially more relevant results than irrelevant ones.
entityTypesEvaluationMetrics_precision :: Lens.Lens' EntityTypesEvaluationMetrics (Prelude.Maybe Prelude.Double)
entityTypesEvaluationMetrics_precision = Lens.lens (\EntityTypesEvaluationMetrics' {precision} -> precision) (\s@EntityTypesEvaluationMetrics' {} a -> s {precision = a} :: EntityTypesEvaluationMetrics)

-- | A measure of how complete the recognizer results are for a specific
-- entity type in the test data. High recall means that the recognizer
-- returned most of the relevant results.
entityTypesEvaluationMetrics_recall :: Lens.Lens' EntityTypesEvaluationMetrics (Prelude.Maybe Prelude.Double)
entityTypesEvaluationMetrics_recall = Lens.lens (\EntityTypesEvaluationMetrics' {recall} -> recall) (\s@EntityTypesEvaluationMetrics' {} a -> s {recall = a} :: EntityTypesEvaluationMetrics)

instance
  Prelude.FromJSON
    EntityTypesEvaluationMetrics
  where
  parseJSON =
    Prelude.withObject
      "EntityTypesEvaluationMetrics"
      ( \x ->
          EntityTypesEvaluationMetrics'
            Prelude.<$> (x Prelude..:? "F1Score")
            Prelude.<*> (x Prelude..:? "Precision")
            Prelude.<*> (x Prelude..:? "Recall")
      )

instance
  Prelude.Hashable
    EntityTypesEvaluationMetrics

instance Prelude.NFData EntityTypesEvaluationMetrics
