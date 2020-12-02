{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaColumn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaColumn where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key-value pair representing a column and data type that this transform can run against. The @Schema@ parameter of the @MLTransform@ may contain up to 100 of these structures.
--
--
--
-- /See:/ 'schemaColumn' smart constructor.
data SchemaColumn = SchemaColumn'
  { _sName :: !(Maybe Text),
    _sDataType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaColumn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sName' - The name of the column.
--
-- * 'sDataType' - The type of data in the column.
schemaColumn ::
  SchemaColumn
schemaColumn =
  SchemaColumn' {_sName = Nothing, _sDataType = Nothing}

-- | The name of the column.
sName :: Lens' SchemaColumn (Maybe Text)
sName = lens _sName (\s a -> s {_sName = a})

-- | The type of data in the column.
sDataType :: Lens' SchemaColumn (Maybe Text)
sDataType = lens _sDataType (\s a -> s {_sDataType = a})

instance FromJSON SchemaColumn where
  parseJSON =
    withObject
      "SchemaColumn"
      (\x -> SchemaColumn' <$> (x .:? "Name") <*> (x .:? "DataType"))

instance Hashable SchemaColumn

instance NFData SchemaColumn

instance ToJSON SchemaColumn where
  toJSON SchemaColumn' {..} =
    object
      ( catMaybes
          [("Name" .=) <$> _sName, ("DataType" .=) <$> _sDataType]
      )
