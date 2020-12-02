{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SchemaExtensionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SchemaExtensionInfo where

import Network.AWS.DirectoryService.Types.SchemaExtensionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a schema extension.
--
--
--
-- /See:/ 'schemaExtensionInfo' smart constructor.
data SchemaExtensionInfo = SchemaExtensionInfo'
  { _seiDirectoryId ::
      !(Maybe Text),
    _seiSchemaExtensionId :: !(Maybe Text),
    _seiSchemaExtensionStatusReason :: !(Maybe Text),
    _seiSchemaExtensionStatus ::
      !(Maybe SchemaExtensionStatus),
    _seiDescription :: !(Maybe Text),
    _seiEndDateTime :: !(Maybe POSIX),
    _seiStartDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaExtensionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seiDirectoryId' - The identifier of the directory to which the schema extension is applied.
--
-- * 'seiSchemaExtensionId' - The identifier of the schema extension.
--
-- * 'seiSchemaExtensionStatusReason' - The reason for the @SchemaExtensionStatus@ .
--
-- * 'seiSchemaExtensionStatus' - The current status of the schema extension.
--
-- * 'seiDescription' - A description of the schema extension.
--
-- * 'seiEndDateTime' - The date and time that the schema extension was completed.
--
-- * 'seiStartDateTime' - The date and time that the schema extension started being applied to the directory.
schemaExtensionInfo ::
  SchemaExtensionInfo
schemaExtensionInfo =
  SchemaExtensionInfo'
    { _seiDirectoryId = Nothing,
      _seiSchemaExtensionId = Nothing,
      _seiSchemaExtensionStatusReason = Nothing,
      _seiSchemaExtensionStatus = Nothing,
      _seiDescription = Nothing,
      _seiEndDateTime = Nothing,
      _seiStartDateTime = Nothing
    }

-- | The identifier of the directory to which the schema extension is applied.
seiDirectoryId :: Lens' SchemaExtensionInfo (Maybe Text)
seiDirectoryId = lens _seiDirectoryId (\s a -> s {_seiDirectoryId = a})

-- | The identifier of the schema extension.
seiSchemaExtensionId :: Lens' SchemaExtensionInfo (Maybe Text)
seiSchemaExtensionId = lens _seiSchemaExtensionId (\s a -> s {_seiSchemaExtensionId = a})

-- | The reason for the @SchemaExtensionStatus@ .
seiSchemaExtensionStatusReason :: Lens' SchemaExtensionInfo (Maybe Text)
seiSchemaExtensionStatusReason = lens _seiSchemaExtensionStatusReason (\s a -> s {_seiSchemaExtensionStatusReason = a})

-- | The current status of the schema extension.
seiSchemaExtensionStatus :: Lens' SchemaExtensionInfo (Maybe SchemaExtensionStatus)
seiSchemaExtensionStatus = lens _seiSchemaExtensionStatus (\s a -> s {_seiSchemaExtensionStatus = a})

-- | A description of the schema extension.
seiDescription :: Lens' SchemaExtensionInfo (Maybe Text)
seiDescription = lens _seiDescription (\s a -> s {_seiDescription = a})

-- | The date and time that the schema extension was completed.
seiEndDateTime :: Lens' SchemaExtensionInfo (Maybe UTCTime)
seiEndDateTime = lens _seiEndDateTime (\s a -> s {_seiEndDateTime = a}) . mapping _Time

-- | The date and time that the schema extension started being applied to the directory.
seiStartDateTime :: Lens' SchemaExtensionInfo (Maybe UTCTime)
seiStartDateTime = lens _seiStartDateTime (\s a -> s {_seiStartDateTime = a}) . mapping _Time

instance FromJSON SchemaExtensionInfo where
  parseJSON =
    withObject
      "SchemaExtensionInfo"
      ( \x ->
          SchemaExtensionInfo'
            <$> (x .:? "DirectoryId")
            <*> (x .:? "SchemaExtensionId")
            <*> (x .:? "SchemaExtensionStatusReason")
            <*> (x .:? "SchemaExtensionStatus")
            <*> (x .:? "Description")
            <*> (x .:? "EndDateTime")
            <*> (x .:? "StartDateTime")
      )

instance Hashable SchemaExtensionInfo

instance NFData SchemaExtensionInfo
