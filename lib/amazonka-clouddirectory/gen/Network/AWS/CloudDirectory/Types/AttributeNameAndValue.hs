{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.AttributeNameAndValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeNameAndValue where

import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the attribute name and value for a typed link.
--
--
--
-- /See:/ 'attributeNameAndValue' smart constructor.
data AttributeNameAndValue = AttributeNameAndValue'
  { _anavAttributeName ::
      !Text,
    _anavValue :: !TypedAttributeValue
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeNameAndValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'anavAttributeName' - The attribute name of the typed link.
--
-- * 'anavValue' - The value for the typed link.
attributeNameAndValue ::
  -- | 'anavAttributeName'
  Text ->
  -- | 'anavValue'
  TypedAttributeValue ->
  AttributeNameAndValue
attributeNameAndValue pAttributeName_ pValue_ =
  AttributeNameAndValue'
    { _anavAttributeName = pAttributeName_,
      _anavValue = pValue_
    }

-- | The attribute name of the typed link.
anavAttributeName :: Lens' AttributeNameAndValue Text
anavAttributeName = lens _anavAttributeName (\s a -> s {_anavAttributeName = a})

-- | The value for the typed link.
anavValue :: Lens' AttributeNameAndValue TypedAttributeValue
anavValue = lens _anavValue (\s a -> s {_anavValue = a})

instance FromJSON AttributeNameAndValue where
  parseJSON =
    withObject
      "AttributeNameAndValue"
      ( \x ->
          AttributeNameAndValue'
            <$> (x .: "AttributeName") <*> (x .: "Value")
      )

instance Hashable AttributeNameAndValue

instance NFData AttributeNameAndValue

instance ToJSON AttributeNameAndValue where
  toJSON AttributeNameAndValue' {..} =
    object
      ( catMaybes
          [ Just ("AttributeName" .= _anavAttributeName),
            Just ("Value" .= _anavValue)
          ]
      )
