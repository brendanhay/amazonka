{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerMetadata where

import Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detailed information about an entity recognizer.
--
--
--
-- /See:/ 'entityRecognizerMetadata' smart constructor.
data EntityRecognizerMetadata = EntityRecognizerMetadata'
  { _ermEntityTypes ::
      !( Maybe
           [EntityRecognizerMetadataEntityTypesListItem]
       ),
    _ermEvaluationMetrics ::
      !( Maybe
           EntityRecognizerEvaluationMetrics
       ),
    _ermNumberOfTrainedDocuments ::
      !(Maybe Int),
    _ermNumberOfTestDocuments :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityRecognizerMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ermEntityTypes' - Entity types from the metadata of an entity recognizer.
--
-- * 'ermEvaluationMetrics' - Detailed information about the accuracy of an entity recognizer.
--
-- * 'ermNumberOfTrainedDocuments' - The number of documents in the input data that were used to train the entity recognizer. Typically this is 80 to 90 percent of the input documents.
--
-- * 'ermNumberOfTestDocuments' - The number of documents in the input data that were used to test the entity recognizer. Typically this is 10 to 20 percent of the input documents.
entityRecognizerMetadata ::
  EntityRecognizerMetadata
entityRecognizerMetadata =
  EntityRecognizerMetadata'
    { _ermEntityTypes = Nothing,
      _ermEvaluationMetrics = Nothing,
      _ermNumberOfTrainedDocuments = Nothing,
      _ermNumberOfTestDocuments = Nothing
    }

-- | Entity types from the metadata of an entity recognizer.
ermEntityTypes :: Lens' EntityRecognizerMetadata [EntityRecognizerMetadataEntityTypesListItem]
ermEntityTypes = lens _ermEntityTypes (\s a -> s {_ermEntityTypes = a}) . _Default . _Coerce

-- | Detailed information about the accuracy of an entity recognizer.
ermEvaluationMetrics :: Lens' EntityRecognizerMetadata (Maybe EntityRecognizerEvaluationMetrics)
ermEvaluationMetrics = lens _ermEvaluationMetrics (\s a -> s {_ermEvaluationMetrics = a})

-- | The number of documents in the input data that were used to train the entity recognizer. Typically this is 80 to 90 percent of the input documents.
ermNumberOfTrainedDocuments :: Lens' EntityRecognizerMetadata (Maybe Int)
ermNumberOfTrainedDocuments = lens _ermNumberOfTrainedDocuments (\s a -> s {_ermNumberOfTrainedDocuments = a})

-- | The number of documents in the input data that were used to test the entity recognizer. Typically this is 10 to 20 percent of the input documents.
ermNumberOfTestDocuments :: Lens' EntityRecognizerMetadata (Maybe Int)
ermNumberOfTestDocuments = lens _ermNumberOfTestDocuments (\s a -> s {_ermNumberOfTestDocuments = a})

instance FromJSON EntityRecognizerMetadata where
  parseJSON =
    withObject
      "EntityRecognizerMetadata"
      ( \x ->
          EntityRecognizerMetadata'
            <$> (x .:? "EntityTypes" .!= mempty)
            <*> (x .:? "EvaluationMetrics")
            <*> (x .:? "NumberOfTrainedDocuments")
            <*> (x .:? "NumberOfTestDocuments")
      )

instance Hashable EntityRecognizerMetadata

instance NFData EntityRecognizerMetadata
