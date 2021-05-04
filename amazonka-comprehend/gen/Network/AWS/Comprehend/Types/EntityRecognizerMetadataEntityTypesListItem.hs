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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem where

import Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Individual item from the list of entity types in the metadata of an
-- entity recognizer.
--
-- /See:/ 'newEntityRecognizerMetadataEntityTypesListItem' smart constructor.
data EntityRecognizerMetadataEntityTypesListItem = EntityRecognizerMetadataEntityTypesListItem'
  { -- | Indicates the number of times the given entity type was seen in the
    -- training data.
    numberOfTrainMentions :: Prelude.Maybe Prelude.Int,
    -- | Detailed information about the accuracy of the entity recognizer for a
    -- specific item on the list of entity types.
    evaluationMetrics :: Prelude.Maybe EntityTypesEvaluationMetrics,
    -- | Type of entity from the list of entity types in the metadata of an
    -- entity recognizer.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerMetadataEntityTypesListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfTrainMentions', 'entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions' - Indicates the number of times the given entity type was seen in the
-- training data.
--
-- 'evaluationMetrics', 'entityRecognizerMetadataEntityTypesListItem_evaluationMetrics' - Detailed information about the accuracy of the entity recognizer for a
-- specific item on the list of entity types.
--
-- 'type'', 'entityRecognizerMetadataEntityTypesListItem_type' - Type of entity from the list of entity types in the metadata of an
-- entity recognizer.
newEntityRecognizerMetadataEntityTypesListItem ::
  EntityRecognizerMetadataEntityTypesListItem
newEntityRecognizerMetadataEntityTypesListItem =
  EntityRecognizerMetadataEntityTypesListItem'
    { numberOfTrainMentions =
        Prelude.Nothing,
      evaluationMetrics =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Indicates the number of times the given entity type was seen in the
-- training data.
entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe Prelude.Int)
entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {numberOfTrainMentions} -> numberOfTrainMentions) (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {numberOfTrainMentions = a} :: EntityRecognizerMetadataEntityTypesListItem)

-- | Detailed information about the accuracy of the entity recognizer for a
-- specific item on the list of entity types.
entityRecognizerMetadataEntityTypesListItem_evaluationMetrics :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe EntityTypesEvaluationMetrics)
entityRecognizerMetadataEntityTypesListItem_evaluationMetrics = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {evaluationMetrics} -> evaluationMetrics) (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {evaluationMetrics = a} :: EntityRecognizerMetadataEntityTypesListItem)

-- | Type of entity from the list of entity types in the metadata of an
-- entity recognizer.
entityRecognizerMetadataEntityTypesListItem_type :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe Prelude.Text)
entityRecognizerMetadataEntityTypesListItem_type = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {type'} -> type') (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {type' = a} :: EntityRecognizerMetadataEntityTypesListItem)

instance
  Prelude.FromJSON
    EntityRecognizerMetadataEntityTypesListItem
  where
  parseJSON =
    Prelude.withObject
      "EntityRecognizerMetadataEntityTypesListItem"
      ( \x ->
          EntityRecognizerMetadataEntityTypesListItem'
            Prelude.<$> (x Prelude..:? "NumberOfTrainMentions")
              Prelude.<*> (x Prelude..:? "EvaluationMetrics")
              Prelude.<*> (x Prelude..:? "Type")
      )

instance
  Prelude.Hashable
    EntityRecognizerMetadataEntityTypesListItem

instance
  Prelude.NFData
    EntityRecognizerMetadataEntityTypesListItem
