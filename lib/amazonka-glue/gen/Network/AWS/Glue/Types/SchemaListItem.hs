{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaListItem where

import Network.AWS.Glue.Types.SchemaStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains minimal details for a schema.
--
--
--
-- /See:/ 'schemaListItem' smart constructor.
data SchemaListItem = SchemaListItem'
  { _sliRegistryName ::
      !(Maybe Text),
    _sliCreatedTime :: !(Maybe Text),
    _sliSchemaStatus :: !(Maybe SchemaStatus),
    _sliSchemaName :: !(Maybe Text),
    _sliSchemaARN :: !(Maybe Text),
    _sliUpdatedTime :: !(Maybe Text),
    _sliDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sliRegistryName' - the name of the registry where the schema resides.
--
-- * 'sliCreatedTime' - The date and time that a schema was created.
--
-- * 'sliSchemaStatus' - The status of the schema.
--
-- * 'sliSchemaName' - The name of the schema.
--
-- * 'sliSchemaARN' - The Amazon Resource Name (ARN) for the schema.
--
-- * 'sliUpdatedTime' - The date and time that a schema was updated.
--
-- * 'sliDescription' - A description for the schema.
schemaListItem ::
  SchemaListItem
schemaListItem =
  SchemaListItem'
    { _sliRegistryName = Nothing,
      _sliCreatedTime = Nothing,
      _sliSchemaStatus = Nothing,
      _sliSchemaName = Nothing,
      _sliSchemaARN = Nothing,
      _sliUpdatedTime = Nothing,
      _sliDescription = Nothing
    }

-- | the name of the registry where the schema resides.
sliRegistryName :: Lens' SchemaListItem (Maybe Text)
sliRegistryName = lens _sliRegistryName (\s a -> s {_sliRegistryName = a})

-- | The date and time that a schema was created.
sliCreatedTime :: Lens' SchemaListItem (Maybe Text)
sliCreatedTime = lens _sliCreatedTime (\s a -> s {_sliCreatedTime = a})

-- | The status of the schema.
sliSchemaStatus :: Lens' SchemaListItem (Maybe SchemaStatus)
sliSchemaStatus = lens _sliSchemaStatus (\s a -> s {_sliSchemaStatus = a})

-- | The name of the schema.
sliSchemaName :: Lens' SchemaListItem (Maybe Text)
sliSchemaName = lens _sliSchemaName (\s a -> s {_sliSchemaName = a})

-- | The Amazon Resource Name (ARN) for the schema.
sliSchemaARN :: Lens' SchemaListItem (Maybe Text)
sliSchemaARN = lens _sliSchemaARN (\s a -> s {_sliSchemaARN = a})

-- | The date and time that a schema was updated.
sliUpdatedTime :: Lens' SchemaListItem (Maybe Text)
sliUpdatedTime = lens _sliUpdatedTime (\s a -> s {_sliUpdatedTime = a})

-- | A description for the schema.
sliDescription :: Lens' SchemaListItem (Maybe Text)
sliDescription = lens _sliDescription (\s a -> s {_sliDescription = a})

instance FromJSON SchemaListItem where
  parseJSON =
    withObject
      "SchemaListItem"
      ( \x ->
          SchemaListItem'
            <$> (x .:? "RegistryName")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "SchemaStatus")
            <*> (x .:? "SchemaName")
            <*> (x .:? "SchemaArn")
            <*> (x .:? "UpdatedTime")
            <*> (x .:? "Description")
      )

instance Hashable SchemaListItem

instance NFData SchemaListItem
