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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Individual item from the list of entity types in the metadata of an
-- entity recognizer.
--
-- /See:/ 'newEntityRecognizerMetadataEntityTypesListItem' smart constructor.
data EntityRecognizerMetadataEntityTypesListItem = EntityRecognizerMetadataEntityTypesListItem'
  { -- | Indicates the number of times the given entity type was seen in the
    -- training data.
    numberOfTrainMentions :: Core.Maybe Core.Int,
    -- | Detailed information about the accuracy of the entity recognizer for a
    -- specific item on the list of entity types.
    evaluationMetrics :: Core.Maybe EntityTypesEvaluationMetrics,
    -- | Type of entity from the list of entity types in the metadata of an
    -- entity recognizer.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      evaluationMetrics =
        Core.Nothing,
      type' = Core.Nothing
    }

-- | Indicates the number of times the given entity type was seen in the
-- training data.
entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Core.Maybe Core.Int)
entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {numberOfTrainMentions} -> numberOfTrainMentions) (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {numberOfTrainMentions = a} :: EntityRecognizerMetadataEntityTypesListItem)

-- | Detailed information about the accuracy of the entity recognizer for a
-- specific item on the list of entity types.
entityRecognizerMetadataEntityTypesListItem_evaluationMetrics :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Core.Maybe EntityTypesEvaluationMetrics)
entityRecognizerMetadataEntityTypesListItem_evaluationMetrics = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {evaluationMetrics} -> evaluationMetrics) (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {evaluationMetrics = a} :: EntityRecognizerMetadataEntityTypesListItem)

-- | Type of entity from the list of entity types in the metadata of an
-- entity recognizer.
entityRecognizerMetadataEntityTypesListItem_type :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Core.Maybe Core.Text)
entityRecognizerMetadataEntityTypesListItem_type = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {type'} -> type') (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {type' = a} :: EntityRecognizerMetadataEntityTypesListItem)

instance
  Core.FromJSON
    EntityRecognizerMetadataEntityTypesListItem
  where
  parseJSON =
    Core.withObject
      "EntityRecognizerMetadataEntityTypesListItem"
      ( \x ->
          EntityRecognizerMetadataEntityTypesListItem'
            Core.<$> (x Core..:? "NumberOfTrainMentions")
            Core.<*> (x Core..:? "EvaluationMetrics")
            Core.<*> (x Core..:? "Type")
      )

instance
  Core.Hashable
    EntityRecognizerMetadataEntityTypesListItem

instance
  Core.NFData
    EntityRecognizerMetadataEntityTypesListItem
