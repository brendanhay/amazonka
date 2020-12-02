{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityTypesListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityTypesListItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer.
--
--
--
-- /See:/ 'entityTypesListItem' smart constructor.
newtype EntityTypesListItem = EntityTypesListItem'
  { _etliType ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityTypesListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etliType' - An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer. Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break, \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
entityTypesListItem ::
  -- | 'etliType'
  Text ->
  EntityTypesListItem
entityTypesListItem pType_ =
  EntityTypesListItem' {_etliType = pType_}

-- | An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer. Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break, \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
etliType :: Lens' EntityTypesListItem Text
etliType = lens _etliType (\s a -> s {_etliType = a})

instance FromJSON EntityTypesListItem where
  parseJSON =
    withObject
      "EntityTypesListItem"
      (\x -> EntityTypesListItem' <$> (x .: "Type"))

instance Hashable EntityTypesListItem

instance NFData EntityTypesListItem

instance ToJSON EntityTypesListItem where
  toJSON EntityTypesListItem' {..} =
    object (catMaybes [Just ("Type" .= _etliType)])
