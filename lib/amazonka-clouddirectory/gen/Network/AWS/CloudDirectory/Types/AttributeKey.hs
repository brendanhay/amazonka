{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.AttributeKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeKey where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A unique identifier for an attribute.
--
--
--
-- /See:/ 'attributeKey' smart constructor.
data AttributeKey = AttributeKey'
  { _akSchemaARN :: !Text,
    _akFacetName :: !Text,
    _akName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akSchemaARN' - The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
--
-- * 'akFacetName' - The name of the facet that the attribute exists within.
--
-- * 'akName' - The name of the attribute.
attributeKey ::
  -- | 'akSchemaARN'
  Text ->
  -- | 'akFacetName'
  Text ->
  -- | 'akName'
  Text ->
  AttributeKey
attributeKey pSchemaARN_ pFacetName_ pName_ =
  AttributeKey'
    { _akSchemaARN = pSchemaARN_,
      _akFacetName = pFacetName_,
      _akName = pName_
    }

-- | The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
akSchemaARN :: Lens' AttributeKey Text
akSchemaARN = lens _akSchemaARN (\s a -> s {_akSchemaARN = a})

-- | The name of the facet that the attribute exists within.
akFacetName :: Lens' AttributeKey Text
akFacetName = lens _akFacetName (\s a -> s {_akFacetName = a})

-- | The name of the attribute.
akName :: Lens' AttributeKey Text
akName = lens _akName (\s a -> s {_akName = a})

instance FromJSON AttributeKey where
  parseJSON =
    withObject
      "AttributeKey"
      ( \x ->
          AttributeKey'
            <$> (x .: "SchemaArn") <*> (x .: "FacetName") <*> (x .: "Name")
      )

instance Hashable AttributeKey

instance NFData AttributeKey

instance ToJSON AttributeKey where
  toJSON AttributeKey' {..} =
    object
      ( catMaybes
          [ Just ("SchemaArn" .= _akSchemaARN),
            Just ("FacetName" .= _akFacetName),
            Just ("Name" .= _akName)
          ]
      )
