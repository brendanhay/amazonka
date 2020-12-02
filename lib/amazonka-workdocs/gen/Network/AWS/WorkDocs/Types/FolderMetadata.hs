{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.FolderMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.FolderMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.ResourceStateType

-- | Describes a folder.
--
--
--
-- /See:/ 'folderMetadata' smart constructor.
data FolderMetadata = FolderMetadata'
  { _fmSignature ::
      !(Maybe Text),
    _fmParentFolderId :: !(Maybe Text),
    _fmSize :: !(Maybe Integer),
    _fmLatestVersionSize :: !(Maybe Integer),
    _fmName :: !(Maybe Text),
    _fmModifiedTimestamp :: !(Maybe POSIX),
    _fmId :: !(Maybe Text),
    _fmLabels :: !(Maybe [Text]),
    _fmResourceState :: !(Maybe ResourceStateType),
    _fmCreatedTimestamp :: !(Maybe POSIX),
    _fmCreatorId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FolderMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmSignature' - The unique identifier created from the subfolders and documents of the folder.
--
-- * 'fmParentFolderId' - The ID of the parent folder.
--
-- * 'fmSize' - The size of the folder metadata.
--
-- * 'fmLatestVersionSize' - The size of the latest version of the folder metadata.
--
-- * 'fmName' - The name of the folder.
--
-- * 'fmModifiedTimestamp' - The time when the folder was updated.
--
-- * 'fmId' - The ID of the folder.
--
-- * 'fmLabels' - List of labels on the folder.
--
-- * 'fmResourceState' - The resource state of the folder.
--
-- * 'fmCreatedTimestamp' - The time when the folder was created.
--
-- * 'fmCreatorId' - The ID of the creator.
folderMetadata ::
  FolderMetadata
folderMetadata =
  FolderMetadata'
    { _fmSignature = Nothing,
      _fmParentFolderId = Nothing,
      _fmSize = Nothing,
      _fmLatestVersionSize = Nothing,
      _fmName = Nothing,
      _fmModifiedTimestamp = Nothing,
      _fmId = Nothing,
      _fmLabels = Nothing,
      _fmResourceState = Nothing,
      _fmCreatedTimestamp = Nothing,
      _fmCreatorId = Nothing
    }

-- | The unique identifier created from the subfolders and documents of the folder.
fmSignature :: Lens' FolderMetadata (Maybe Text)
fmSignature = lens _fmSignature (\s a -> s {_fmSignature = a})

-- | The ID of the parent folder.
fmParentFolderId :: Lens' FolderMetadata (Maybe Text)
fmParentFolderId = lens _fmParentFolderId (\s a -> s {_fmParentFolderId = a})

-- | The size of the folder metadata.
fmSize :: Lens' FolderMetadata (Maybe Integer)
fmSize = lens _fmSize (\s a -> s {_fmSize = a})

-- | The size of the latest version of the folder metadata.
fmLatestVersionSize :: Lens' FolderMetadata (Maybe Integer)
fmLatestVersionSize = lens _fmLatestVersionSize (\s a -> s {_fmLatestVersionSize = a})

-- | The name of the folder.
fmName :: Lens' FolderMetadata (Maybe Text)
fmName = lens _fmName (\s a -> s {_fmName = a})

-- | The time when the folder was updated.
fmModifiedTimestamp :: Lens' FolderMetadata (Maybe UTCTime)
fmModifiedTimestamp = lens _fmModifiedTimestamp (\s a -> s {_fmModifiedTimestamp = a}) . mapping _Time

-- | The ID of the folder.
fmId :: Lens' FolderMetadata (Maybe Text)
fmId = lens _fmId (\s a -> s {_fmId = a})

-- | List of labels on the folder.
fmLabels :: Lens' FolderMetadata [Text]
fmLabels = lens _fmLabels (\s a -> s {_fmLabels = a}) . _Default . _Coerce

-- | The resource state of the folder.
fmResourceState :: Lens' FolderMetadata (Maybe ResourceStateType)
fmResourceState = lens _fmResourceState (\s a -> s {_fmResourceState = a})

-- | The time when the folder was created.
fmCreatedTimestamp :: Lens' FolderMetadata (Maybe UTCTime)
fmCreatedTimestamp = lens _fmCreatedTimestamp (\s a -> s {_fmCreatedTimestamp = a}) . mapping _Time

-- | The ID of the creator.
fmCreatorId :: Lens' FolderMetadata (Maybe Text)
fmCreatorId = lens _fmCreatorId (\s a -> s {_fmCreatorId = a})

instance FromJSON FolderMetadata where
  parseJSON =
    withObject
      "FolderMetadata"
      ( \x ->
          FolderMetadata'
            <$> (x .:? "Signature")
            <*> (x .:? "ParentFolderId")
            <*> (x .:? "Size")
            <*> (x .:? "LatestVersionSize")
            <*> (x .:? "Name")
            <*> (x .:? "ModifiedTimestamp")
            <*> (x .:? "Id")
            <*> (x .:? "Labels" .!= mempty)
            <*> (x .:? "ResourceState")
            <*> (x .:? "CreatedTimestamp")
            <*> (x .:? "CreatorId")
      )

instance Hashable FolderMetadata

instance NFData FolderMetadata
