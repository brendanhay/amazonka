{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionListItem where

import Network.AWS.Glue.Types.SchemaVersionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object containing the details about a schema version.
--
--
--
-- /See:/ 'schemaVersionListItem' smart constructor.
data SchemaVersionListItem = SchemaVersionListItem'
  { _svliStatus ::
      !(Maybe SchemaVersionStatus),
    _svliCreatedTime :: !(Maybe Text),
    _svliSchemaVersionId :: !(Maybe Text),
    _svliVersionNumber :: !(Maybe Nat),
    _svliSchemaARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaVersionListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svliStatus' - The status of the schema version.
--
-- * 'svliCreatedTime' - The date and time the schema version was created.
--
-- * 'svliSchemaVersionId' - The unique identifier of the schema version.
--
-- * 'svliVersionNumber' - The version number of the schema.
--
-- * 'svliSchemaARN' - The Amazon Resource Name (ARN) of the schema.
schemaVersionListItem ::
  SchemaVersionListItem
schemaVersionListItem =
  SchemaVersionListItem'
    { _svliStatus = Nothing,
      _svliCreatedTime = Nothing,
      _svliSchemaVersionId = Nothing,
      _svliVersionNumber = Nothing,
      _svliSchemaARN = Nothing
    }

-- | The status of the schema version.
svliStatus :: Lens' SchemaVersionListItem (Maybe SchemaVersionStatus)
svliStatus = lens _svliStatus (\s a -> s {_svliStatus = a})

-- | The date and time the schema version was created.
svliCreatedTime :: Lens' SchemaVersionListItem (Maybe Text)
svliCreatedTime = lens _svliCreatedTime (\s a -> s {_svliCreatedTime = a})

-- | The unique identifier of the schema version.
svliSchemaVersionId :: Lens' SchemaVersionListItem (Maybe Text)
svliSchemaVersionId = lens _svliSchemaVersionId (\s a -> s {_svliSchemaVersionId = a})

-- | The version number of the schema.
svliVersionNumber :: Lens' SchemaVersionListItem (Maybe Natural)
svliVersionNumber = lens _svliVersionNumber (\s a -> s {_svliVersionNumber = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the schema.
svliSchemaARN :: Lens' SchemaVersionListItem (Maybe Text)
svliSchemaARN = lens _svliSchemaARN (\s a -> s {_svliSchemaARN = a})

instance FromJSON SchemaVersionListItem where
  parseJSON =
    withObject
      "SchemaVersionListItem"
      ( \x ->
          SchemaVersionListItem'
            <$> (x .:? "Status")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "SchemaVersionId")
            <*> (x .:? "VersionNumber")
            <*> (x .:? "SchemaArn")
      )

instance Hashable SchemaVersionListItem

instance NFData SchemaVersionListItem
