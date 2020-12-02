{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeKeyAndValue where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The combination of an attribute key and an attribute value.
--
--
--
-- /See:/ 'attributeKeyAndValue' smart constructor.
data AttributeKeyAndValue = AttributeKeyAndValue'
  { _akavKey ::
      !AttributeKey,
    _akavValue :: !TypedAttributeValue
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeKeyAndValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akavKey' - The key of the attribute.
--
-- * 'akavValue' - The value of the attribute.
attributeKeyAndValue ::
  -- | 'akavKey'
  AttributeKey ->
  -- | 'akavValue'
  TypedAttributeValue ->
  AttributeKeyAndValue
attributeKeyAndValue pKey_ pValue_ =
  AttributeKeyAndValue' {_akavKey = pKey_, _akavValue = pValue_}

-- | The key of the attribute.
akavKey :: Lens' AttributeKeyAndValue AttributeKey
akavKey = lens _akavKey (\s a -> s {_akavKey = a})

-- | The value of the attribute.
akavValue :: Lens' AttributeKeyAndValue TypedAttributeValue
akavValue = lens _akavValue (\s a -> s {_akavValue = a})

instance FromJSON AttributeKeyAndValue where
  parseJSON =
    withObject
      "AttributeKeyAndValue"
      (\x -> AttributeKeyAndValue' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable AttributeKeyAndValue

instance NFData AttributeKeyAndValue

instance ToJSON AttributeKeyAndValue where
  toJSON AttributeKeyAndValue' {..} =
    object
      ( catMaybes
          [Just ("Key" .= _akavKey), Just ("Value" .= _akavValue)]
      )
