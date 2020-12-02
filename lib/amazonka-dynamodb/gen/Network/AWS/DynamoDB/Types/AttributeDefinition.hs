{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AttributeDefinition where

import Network.AWS.DynamoDB.Types.ScalarAttributeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an attribute for describing the key schema for the table and indexes.
--
--
--
-- /See:/ 'attributeDefinition' smart constructor.
data AttributeDefinition = AttributeDefinition'
  { _adAttributeName ::
      !Text,
    _adAttributeType :: !ScalarAttributeType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAttributeName' - A name for the attribute.
--
-- * 'adAttributeType' - The data type for the attribute, where:     * @S@ - the attribute is of type String     * @N@ - the attribute is of type Number     * @B@ - the attribute is of type Binary
attributeDefinition ::
  -- | 'adAttributeName'
  Text ->
  -- | 'adAttributeType'
  ScalarAttributeType ->
  AttributeDefinition
attributeDefinition pAttributeName_ pAttributeType_ =
  AttributeDefinition'
    { _adAttributeName = pAttributeName_,
      _adAttributeType = pAttributeType_
    }

-- | A name for the attribute.
adAttributeName :: Lens' AttributeDefinition Text
adAttributeName = lens _adAttributeName (\s a -> s {_adAttributeName = a})

-- | The data type for the attribute, where:     * @S@ - the attribute is of type String     * @N@ - the attribute is of type Number     * @B@ - the attribute is of type Binary
adAttributeType :: Lens' AttributeDefinition ScalarAttributeType
adAttributeType = lens _adAttributeType (\s a -> s {_adAttributeType = a})

instance FromJSON AttributeDefinition where
  parseJSON =
    withObject
      "AttributeDefinition"
      ( \x ->
          AttributeDefinition'
            <$> (x .: "AttributeName") <*> (x .: "AttributeType")
      )

instance Hashable AttributeDefinition

instance NFData AttributeDefinition

instance ToJSON AttributeDefinition where
  toJSON AttributeDefinition' {..} =
    object
      ( catMaybes
          [ Just ("AttributeName" .= _adAttributeName),
            Just ("AttributeType" .= _adAttributeType)
          ]
      )
