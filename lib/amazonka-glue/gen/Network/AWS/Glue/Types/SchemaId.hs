{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaId where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'schemaId' smart constructor.
data SchemaId = SchemaId'
  { _siRegistryName :: !(Maybe Text),
    _siSchemaName :: !(Maybe Text),
    _siSchemaARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siRegistryName' - Undocumented member.
--
-- * 'siSchemaName' - Undocumented member.
--
-- * 'siSchemaARN' - Undocumented member.
schemaId ::
  SchemaId
schemaId =
  SchemaId'
    { _siRegistryName = Nothing,
      _siSchemaName = Nothing,
      _siSchemaARN = Nothing
    }

-- | Undocumented member.
siRegistryName :: Lens' SchemaId (Maybe Text)
siRegistryName = lens _siRegistryName (\s a -> s {_siRegistryName = a})

-- | Undocumented member.
siSchemaName :: Lens' SchemaId (Maybe Text)
siSchemaName = lens _siSchemaName (\s a -> s {_siSchemaName = a})

-- | Undocumented member.
siSchemaARN :: Lens' SchemaId (Maybe Text)
siSchemaARN = lens _siSchemaARN (\s a -> s {_siSchemaARN = a})

instance FromJSON SchemaId where
  parseJSON =
    withObject
      "SchemaId"
      ( \x ->
          SchemaId'
            <$> (x .:? "RegistryName")
            <*> (x .:? "SchemaName")
            <*> (x .:? "SchemaArn")
      )

instance Hashable SchemaId

instance NFData SchemaId

instance ToJSON SchemaId where
  toJSON SchemaId' {..} =
    object
      ( catMaybes
          [ ("RegistryName" .=) <$> _siRegistryName,
            ("SchemaName" .=) <$> _siSchemaName,
            ("SchemaArn" .=) <$> _siSchemaARN
          ]
      )
