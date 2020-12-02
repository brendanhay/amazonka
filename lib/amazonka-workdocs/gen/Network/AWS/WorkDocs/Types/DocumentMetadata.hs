{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.DocumentVersionMetadata
import Network.AWS.WorkDocs.Types.ResourceStateType

-- | Describes the document.
--
--
--
-- /See:/ 'documentMetadata' smart constructor.
data DocumentMetadata = DocumentMetadata'
  { _dmLatestVersionMetadata ::
      !(Maybe DocumentVersionMetadata),
    _dmParentFolderId :: !(Maybe Text),
    _dmModifiedTimestamp :: !(Maybe POSIX),
    _dmId :: !(Maybe Text),
    _dmLabels :: !(Maybe [Text]),
    _dmResourceState :: !(Maybe ResourceStateType),
    _dmCreatedTimestamp :: !(Maybe POSIX),
    _dmCreatorId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmLatestVersionMetadata' - The latest version of the document.
--
-- * 'dmParentFolderId' - The ID of the parent folder.
--
-- * 'dmModifiedTimestamp' - The time when the document was updated.
--
-- * 'dmId' - The ID of the document.
--
-- * 'dmLabels' - List of labels on the document.
--
-- * 'dmResourceState' - The resource state.
--
-- * 'dmCreatedTimestamp' - The time when the document was created.
--
-- * 'dmCreatorId' - The ID of the creator.
documentMetadata ::
  DocumentMetadata
documentMetadata =
  DocumentMetadata'
    { _dmLatestVersionMetadata = Nothing,
      _dmParentFolderId = Nothing,
      _dmModifiedTimestamp = Nothing,
      _dmId = Nothing,
      _dmLabels = Nothing,
      _dmResourceState = Nothing,
      _dmCreatedTimestamp = Nothing,
      _dmCreatorId = Nothing
    }

-- | The latest version of the document.
dmLatestVersionMetadata :: Lens' DocumentMetadata (Maybe DocumentVersionMetadata)
dmLatestVersionMetadata = lens _dmLatestVersionMetadata (\s a -> s {_dmLatestVersionMetadata = a})

-- | The ID of the parent folder.
dmParentFolderId :: Lens' DocumentMetadata (Maybe Text)
dmParentFolderId = lens _dmParentFolderId (\s a -> s {_dmParentFolderId = a})

-- | The time when the document was updated.
dmModifiedTimestamp :: Lens' DocumentMetadata (Maybe UTCTime)
dmModifiedTimestamp = lens _dmModifiedTimestamp (\s a -> s {_dmModifiedTimestamp = a}) . mapping _Time

-- | The ID of the document.
dmId :: Lens' DocumentMetadata (Maybe Text)
dmId = lens _dmId (\s a -> s {_dmId = a})

-- | List of labels on the document.
dmLabels :: Lens' DocumentMetadata [Text]
dmLabels = lens _dmLabels (\s a -> s {_dmLabels = a}) . _Default . _Coerce

-- | The resource state.
dmResourceState :: Lens' DocumentMetadata (Maybe ResourceStateType)
dmResourceState = lens _dmResourceState (\s a -> s {_dmResourceState = a})

-- | The time when the document was created.
dmCreatedTimestamp :: Lens' DocumentMetadata (Maybe UTCTime)
dmCreatedTimestamp = lens _dmCreatedTimestamp (\s a -> s {_dmCreatedTimestamp = a}) . mapping _Time

-- | The ID of the creator.
dmCreatorId :: Lens' DocumentMetadata (Maybe Text)
dmCreatorId = lens _dmCreatorId (\s a -> s {_dmCreatorId = a})

instance FromJSON DocumentMetadata where
  parseJSON =
    withObject
      "DocumentMetadata"
      ( \x ->
          DocumentMetadata'
            <$> (x .:? "LatestVersionMetadata")
            <*> (x .:? "ParentFolderId")
            <*> (x .:? "ModifiedTimestamp")
            <*> (x .:? "Id")
            <*> (x .:? "Labels" .!= mempty)
            <*> (x .:? "ResourceState")
            <*> (x .:? "CreatedTimestamp")
            <*> (x .:? "CreatorId")
      )

instance Hashable DocumentMetadata

instance NFData DocumentMetadata
