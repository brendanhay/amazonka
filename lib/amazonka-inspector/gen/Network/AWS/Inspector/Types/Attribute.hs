{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Attribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a request parameter in the 'AddAttributesToFindings' and 'CreateAssessmentTemplate' actions.
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
  { _aValue :: !(Maybe Text),
    _aKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aValue' - The value assigned to the attribute key.
--
-- * 'aKey' - The attribute key.
attribute ::
  -- | 'aKey'
  Text ->
  Attribute
attribute pKey_ = Attribute' {_aValue = Nothing, _aKey = pKey_}

-- | The value assigned to the attribute key.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\s a -> s {_aValue = a})

-- | The attribute key.
aKey :: Lens' Attribute Text
aKey = lens _aKey (\s a -> s {_aKey = a})

instance FromJSON Attribute where
  parseJSON =
    withObject
      "Attribute"
      (\x -> Attribute' <$> (x .:? "value") <*> (x .: "key"))

instance Hashable Attribute

instance NFData Attribute

instance ToJSON Attribute where
  toJSON Attribute' {..} =
    object
      (catMaybes [("value" .=) <$> _aValue, Just ("key" .= _aKey)])
