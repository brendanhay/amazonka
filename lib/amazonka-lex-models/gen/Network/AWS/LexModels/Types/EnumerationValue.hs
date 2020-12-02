{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.EnumerationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.EnumerationValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Each slot type can have a set of values. Each enumeration value represents a value the slot type can take.
--
--
-- For example, a pizza ordering bot could have a slot type that specifies the type of crust that the pizza should have. The slot type could include the values
--
--     * thick
--
--     * thin
--
--     * stuffed
--
--
--
--
-- /See:/ 'enumerationValue' smart constructor.
data EnumerationValue = EnumerationValue'
  { _evSynonyms ::
      !(Maybe [Text]),
    _evValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnumerationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evSynonyms' - Additional values related to the slot type value.
--
-- * 'evValue' - The value of the slot type.
enumerationValue ::
  -- | 'evValue'
  Text ->
  EnumerationValue
enumerationValue pValue_ =
  EnumerationValue' {_evSynonyms = Nothing, _evValue = pValue_}

-- | Additional values related to the slot type value.
evSynonyms :: Lens' EnumerationValue [Text]
evSynonyms = lens _evSynonyms (\s a -> s {_evSynonyms = a}) . _Default . _Coerce

-- | The value of the slot type.
evValue :: Lens' EnumerationValue Text
evValue = lens _evValue (\s a -> s {_evValue = a})

instance FromJSON EnumerationValue where
  parseJSON =
    withObject
      "EnumerationValue"
      ( \x ->
          EnumerationValue'
            <$> (x .:? "synonyms" .!= mempty) <*> (x .: "value")
      )

instance Hashable EnumerationValue

instance NFData EnumerationValue

instance ToJSON EnumerationValue where
  toJSON EnumerationValue' {..} =
    object
      ( catMaybes
          [("synonyms" .=) <$> _evSynonyms, Just ("value" .= _evValue)]
      )
