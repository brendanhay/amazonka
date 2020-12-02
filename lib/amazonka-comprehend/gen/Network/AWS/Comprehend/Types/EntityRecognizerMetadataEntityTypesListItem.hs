{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem where

import Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Individual item from the list of entity types in the metadata of an entity recognizer.
--
--
--
-- /See:/ 'entityRecognizerMetadataEntityTypesListItem' smart constructor.
data EntityRecognizerMetadataEntityTypesListItem = EntityRecognizerMetadataEntityTypesListItem'
  { _ermetliEvaluationMetrics ::
      !( Maybe
           EntityTypesEvaluationMetrics
       ),
    _ermetliType ::
      !( Maybe
           Text
       ),
    _ermetliNumberOfTrainMentions ::
      !( Maybe
           Int
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'EntityRecognizerMetadataEntityTypesListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ermetliEvaluationMetrics' - Detailed information about the accuracy of the entity recognizer for a specific item on the list of entity types.
--
-- * 'ermetliType' - Type of entity from the list of entity types in the metadata of an entity recognizer.
--
-- * 'ermetliNumberOfTrainMentions' - Indicates the number of times the given entity type was seen in the training data.
entityRecognizerMetadataEntityTypesListItem ::
  EntityRecognizerMetadataEntityTypesListItem
entityRecognizerMetadataEntityTypesListItem =
  EntityRecognizerMetadataEntityTypesListItem'
    { _ermetliEvaluationMetrics =
        Nothing,
      _ermetliType = Nothing,
      _ermetliNumberOfTrainMentions = Nothing
    }

-- | Detailed information about the accuracy of the entity recognizer for a specific item on the list of entity types.
ermetliEvaluationMetrics :: Lens' EntityRecognizerMetadataEntityTypesListItem (Maybe EntityTypesEvaluationMetrics)
ermetliEvaluationMetrics = lens _ermetliEvaluationMetrics (\s a -> s {_ermetliEvaluationMetrics = a})

-- | Type of entity from the list of entity types in the metadata of an entity recognizer.
ermetliType :: Lens' EntityRecognizerMetadataEntityTypesListItem (Maybe Text)
ermetliType = lens _ermetliType (\s a -> s {_ermetliType = a})

-- | Indicates the number of times the given entity type was seen in the training data.
ermetliNumberOfTrainMentions :: Lens' EntityRecognizerMetadataEntityTypesListItem (Maybe Int)
ermetliNumberOfTrainMentions = lens _ermetliNumberOfTrainMentions (\s a -> s {_ermetliNumberOfTrainMentions = a})

instance FromJSON EntityRecognizerMetadataEntityTypesListItem where
  parseJSON =
    withObject
      "EntityRecognizerMetadataEntityTypesListItem"
      ( \x ->
          EntityRecognizerMetadataEntityTypesListItem'
            <$> (x .:? "EvaluationMetrics")
            <*> (x .:? "Type")
            <*> (x .:? "NumberOfTrainMentions")
      )

instance Hashable EntityRecognizerMetadataEntityTypesListItem

instance NFData EntityRecognizerMetadataEntityTypesListItem
