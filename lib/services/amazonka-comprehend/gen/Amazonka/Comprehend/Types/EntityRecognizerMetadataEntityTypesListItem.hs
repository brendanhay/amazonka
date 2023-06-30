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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem where

import Amazonka.Comprehend.Types.EntityTypesEvaluationMetrics
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Individual item from the list of entity types in the metadata of an
-- entity recognizer.
--
-- /See:/ 'newEntityRecognizerMetadataEntityTypesListItem' smart constructor.
data EntityRecognizerMetadataEntityTypesListItem = EntityRecognizerMetadataEntityTypesListItem'
  { -- | Detailed information about the accuracy of the entity recognizer for a
    -- specific item on the list of entity types.
    evaluationMetrics :: Prelude.Maybe EntityTypesEvaluationMetrics,
    -- | Indicates the number of times the given entity type was seen in the
    -- training data.
    numberOfTrainMentions :: Prelude.Maybe Prelude.Int,
    -- | Type of entity from the list of entity types in the metadata of an
    -- entity recognizer.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerMetadataEntityTypesListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationMetrics', 'entityRecognizerMetadataEntityTypesListItem_evaluationMetrics' - Detailed information about the accuracy of the entity recognizer for a
-- specific item on the list of entity types.
--
-- 'numberOfTrainMentions', 'entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions' - Indicates the number of times the given entity type was seen in the
-- training data.
--
-- 'type'', 'entityRecognizerMetadataEntityTypesListItem_type' - Type of entity from the list of entity types in the metadata of an
-- entity recognizer.
newEntityRecognizerMetadataEntityTypesListItem ::
  EntityRecognizerMetadataEntityTypesListItem
newEntityRecognizerMetadataEntityTypesListItem =
  EntityRecognizerMetadataEntityTypesListItem'
    { evaluationMetrics =
        Prelude.Nothing,
      numberOfTrainMentions =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Detailed information about the accuracy of the entity recognizer for a
-- specific item on the list of entity types.
entityRecognizerMetadataEntityTypesListItem_evaluationMetrics :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe EntityTypesEvaluationMetrics)
entityRecognizerMetadataEntityTypesListItem_evaluationMetrics = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {evaluationMetrics} -> evaluationMetrics) (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {evaluationMetrics = a} :: EntityRecognizerMetadataEntityTypesListItem)

-- | Indicates the number of times the given entity type was seen in the
-- training data.
entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe Prelude.Int)
entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {numberOfTrainMentions} -> numberOfTrainMentions) (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {numberOfTrainMentions = a} :: EntityRecognizerMetadataEntityTypesListItem)

-- | Type of entity from the list of entity types in the metadata of an
-- entity recognizer.
entityRecognizerMetadataEntityTypesListItem_type :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe Prelude.Text)
entityRecognizerMetadataEntityTypesListItem_type = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {type'} -> type') (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {type' = a} :: EntityRecognizerMetadataEntityTypesListItem)

instance
  Data.FromJSON
    EntityRecognizerMetadataEntityTypesListItem
  where
  parseJSON =
    Data.withObject
      "EntityRecognizerMetadataEntityTypesListItem"
      ( \x ->
          EntityRecognizerMetadataEntityTypesListItem'
            Prelude.<$> (x Data..:? "EvaluationMetrics")
            Prelude.<*> (x Data..:? "NumberOfTrainMentions")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    EntityRecognizerMetadataEntityTypesListItem
  where
  hashWithSalt
    _salt
    EntityRecognizerMetadataEntityTypesListItem' {..} =
      _salt
        `Prelude.hashWithSalt` evaluationMetrics
        `Prelude.hashWithSalt` numberOfTrainMentions
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    EntityRecognizerMetadataEntityTypesListItem
  where
  rnf EntityRecognizerMetadataEntityTypesListItem' {..} =
    Prelude.rnf evaluationMetrics
      `Prelude.seq` Prelude.rnf numberOfTrainMentions
      `Prelude.seq` Prelude.rnf type'
