{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult where

import Network.AWS.Comprehend.Types.Entity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
--
--
-- /See:/ 'batchDetectEntitiesItemResult' smart constructor.
data BatchDetectEntitiesItemResult = BatchDetectEntitiesItemResult'
  { _bdeirEntities ::
      !(Maybe [Entity]),
    _bdeirIndex :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetectEntitiesItemResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdeirEntities' - One or more 'Entity' objects, one for each entity detected in the document.
--
-- * 'bdeirIndex' - The zero-based index of the document in the input list.
batchDetectEntitiesItemResult ::
  BatchDetectEntitiesItemResult
batchDetectEntitiesItemResult =
  BatchDetectEntitiesItemResult'
    { _bdeirEntities = Nothing,
      _bdeirIndex = Nothing
    }

-- | One or more 'Entity' objects, one for each entity detected in the document.
bdeirEntities :: Lens' BatchDetectEntitiesItemResult [Entity]
bdeirEntities = lens _bdeirEntities (\s a -> s {_bdeirEntities = a}) . _Default . _Coerce

-- | The zero-based index of the document in the input list.
bdeirIndex :: Lens' BatchDetectEntitiesItemResult (Maybe Int)
bdeirIndex = lens _bdeirIndex (\s a -> s {_bdeirIndex = a})

instance FromJSON BatchDetectEntitiesItemResult where
  parseJSON =
    withObject
      "BatchDetectEntitiesItemResult"
      ( \x ->
          BatchDetectEntitiesItemResult'
            <$> (x .:? "Entities" .!= mempty) <*> (x .:? "Index")
      )

instance Hashable BatchDetectEntitiesItemResult

instance NFData BatchDetectEntitiesItemResult
