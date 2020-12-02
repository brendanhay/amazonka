{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentVersionMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.DocumentSourceType
import Network.AWS.WorkDocs.Types.DocumentStatusType
import Network.AWS.WorkDocs.Types.DocumentThumbnailType

-- | Describes a version of a document.
--
--
--
-- /See:/ 'documentVersionMetadata' smart constructor.
data DocumentVersionMetadata = DocumentVersionMetadata'
  { _dvmThumbnail ::
      !( Maybe
           ( Map
               DocumentThumbnailType
               (Sensitive Text)
           )
       ),
    _dvmStatus :: !(Maybe DocumentStatusType),
    _dvmSignature :: !(Maybe Text),
    _dvmContentCreatedTimestamp ::
      !(Maybe POSIX),
    _dvmSize :: !(Maybe Integer),
    _dvmName :: !(Maybe Text),
    _dvmModifiedTimestamp :: !(Maybe POSIX),
    _dvmSource ::
      !( Maybe
           ( Map
               DocumentSourceType
               (Sensitive Text)
           )
       ),
    _dvmId :: !(Maybe Text),
    _dvmCreatedTimestamp :: !(Maybe POSIX),
    _dvmContentModifiedTimestamp ::
      !(Maybe POSIX),
    _dvmCreatorId :: !(Maybe Text),
    _dvmContentType :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentVersionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvmThumbnail' - The thumbnail of the document.
--
-- * 'dvmStatus' - The status of the document.
--
-- * 'dvmSignature' - The signature of the document.
--
-- * 'dvmContentCreatedTimestamp' - The timestamp when the content of the document was originally created.
--
-- * 'dvmSize' - The size of the document, in bytes.
--
-- * 'dvmName' - The name of the version.
--
-- * 'dvmModifiedTimestamp' - The timestamp when the document was last uploaded.
--
-- * 'dvmSource' - The source of the document.
--
-- * 'dvmId' - The ID of the version.
--
-- * 'dvmCreatedTimestamp' - The timestamp when the document was first uploaded.
--
-- * 'dvmContentModifiedTimestamp' - The timestamp when the content of the document was modified.
--
-- * 'dvmCreatorId' - The ID of the creator.
--
-- * 'dvmContentType' - The content type of the document.
documentVersionMetadata ::
  DocumentVersionMetadata
documentVersionMetadata =
  DocumentVersionMetadata'
    { _dvmThumbnail = Nothing,
      _dvmStatus = Nothing,
      _dvmSignature = Nothing,
      _dvmContentCreatedTimestamp = Nothing,
      _dvmSize = Nothing,
      _dvmName = Nothing,
      _dvmModifiedTimestamp = Nothing,
      _dvmSource = Nothing,
      _dvmId = Nothing,
      _dvmCreatedTimestamp = Nothing,
      _dvmContentModifiedTimestamp = Nothing,
      _dvmCreatorId = Nothing,
      _dvmContentType = Nothing
    }

-- | The thumbnail of the document.
dvmThumbnail :: Lens' DocumentVersionMetadata (HashMap DocumentThumbnailType (Text))
dvmThumbnail = lens _dvmThumbnail (\s a -> s {_dvmThumbnail = a}) . _Default . _Map

-- | The status of the document.
dvmStatus :: Lens' DocumentVersionMetadata (Maybe DocumentStatusType)
dvmStatus = lens _dvmStatus (\s a -> s {_dvmStatus = a})

-- | The signature of the document.
dvmSignature :: Lens' DocumentVersionMetadata (Maybe Text)
dvmSignature = lens _dvmSignature (\s a -> s {_dvmSignature = a})

-- | The timestamp when the content of the document was originally created.
dvmContentCreatedTimestamp :: Lens' DocumentVersionMetadata (Maybe UTCTime)
dvmContentCreatedTimestamp = lens _dvmContentCreatedTimestamp (\s a -> s {_dvmContentCreatedTimestamp = a}) . mapping _Time

-- | The size of the document, in bytes.
dvmSize :: Lens' DocumentVersionMetadata (Maybe Integer)
dvmSize = lens _dvmSize (\s a -> s {_dvmSize = a})

-- | The name of the version.
dvmName :: Lens' DocumentVersionMetadata (Maybe Text)
dvmName = lens _dvmName (\s a -> s {_dvmName = a})

-- | The timestamp when the document was last uploaded.
dvmModifiedTimestamp :: Lens' DocumentVersionMetadata (Maybe UTCTime)
dvmModifiedTimestamp = lens _dvmModifiedTimestamp (\s a -> s {_dvmModifiedTimestamp = a}) . mapping _Time

-- | The source of the document.
dvmSource :: Lens' DocumentVersionMetadata (HashMap DocumentSourceType (Text))
dvmSource = lens _dvmSource (\s a -> s {_dvmSource = a}) . _Default . _Map

-- | The ID of the version.
dvmId :: Lens' DocumentVersionMetadata (Maybe Text)
dvmId = lens _dvmId (\s a -> s {_dvmId = a})

-- | The timestamp when the document was first uploaded.
dvmCreatedTimestamp :: Lens' DocumentVersionMetadata (Maybe UTCTime)
dvmCreatedTimestamp = lens _dvmCreatedTimestamp (\s a -> s {_dvmCreatedTimestamp = a}) . mapping _Time

-- | The timestamp when the content of the document was modified.
dvmContentModifiedTimestamp :: Lens' DocumentVersionMetadata (Maybe UTCTime)
dvmContentModifiedTimestamp = lens _dvmContentModifiedTimestamp (\s a -> s {_dvmContentModifiedTimestamp = a}) . mapping _Time

-- | The ID of the creator.
dvmCreatorId :: Lens' DocumentVersionMetadata (Maybe Text)
dvmCreatorId = lens _dvmCreatorId (\s a -> s {_dvmCreatorId = a})

-- | The content type of the document.
dvmContentType :: Lens' DocumentVersionMetadata (Maybe Text)
dvmContentType = lens _dvmContentType (\s a -> s {_dvmContentType = a})

instance FromJSON DocumentVersionMetadata where
  parseJSON =
    withObject
      "DocumentVersionMetadata"
      ( \x ->
          DocumentVersionMetadata'
            <$> (x .:? "Thumbnail" .!= mempty)
            <*> (x .:? "Status")
            <*> (x .:? "Signature")
            <*> (x .:? "ContentCreatedTimestamp")
            <*> (x .:? "Size")
            <*> (x .:? "Name")
            <*> (x .:? "ModifiedTimestamp")
            <*> (x .:? "Source" .!= mempty)
            <*> (x .:? "Id")
            <*> (x .:? "CreatedTimestamp")
            <*> (x .:? "ContentModifiedTimestamp")
            <*> (x .:? "CreatorId")
            <*> (x .:? "ContentType")
      )

instance Hashable DocumentVersionMetadata

instance NFData DocumentVersionMetadata
