{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentVersionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentVersionInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentStatus

-- | Version information about the document.
--
--
--
-- /See:/ 'documentVersionInfo' smart constructor.
data DocumentVersionInfo = DocumentVersionInfo'
  { _dviStatus ::
      !(Maybe DocumentStatus),
    _dviVersionName :: !(Maybe Text),
    _dviCreatedDate :: !(Maybe POSIX),
    _dviDocumentFormat :: !(Maybe DocumentFormat),
    _dviName :: !(Maybe Text),
    _dviDocumentVersion :: !(Maybe Text),
    _dviStatusInformation :: !(Maybe Text),
    _dviIsDefaultVersion :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentVersionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dviStatus' - The status of the Systems Manager document, such as @Creating@ , @Active@ , @Failed@ , and @Deleting@ .
--
-- * 'dviVersionName' - The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- * 'dviCreatedDate' - The date the document was created.
--
-- * 'dviDocumentFormat' - The document format, either JSON or YAML.
--
-- * 'dviName' - The document name.
--
-- * 'dviDocumentVersion' - The document version.
--
-- * 'dviStatusInformation' - A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- * 'dviIsDefaultVersion' - An identifier for the default version of the document.
documentVersionInfo ::
  DocumentVersionInfo
documentVersionInfo =
  DocumentVersionInfo'
    { _dviStatus = Nothing,
      _dviVersionName = Nothing,
      _dviCreatedDate = Nothing,
      _dviDocumentFormat = Nothing,
      _dviName = Nothing,
      _dviDocumentVersion = Nothing,
      _dviStatusInformation = Nothing,
      _dviIsDefaultVersion = Nothing
    }

-- | The status of the Systems Manager document, such as @Creating@ , @Active@ , @Failed@ , and @Deleting@ .
dviStatus :: Lens' DocumentVersionInfo (Maybe DocumentStatus)
dviStatus = lens _dviStatus (\s a -> s {_dviStatus = a})

-- | The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
dviVersionName :: Lens' DocumentVersionInfo (Maybe Text)
dviVersionName = lens _dviVersionName (\s a -> s {_dviVersionName = a})

-- | The date the document was created.
dviCreatedDate :: Lens' DocumentVersionInfo (Maybe UTCTime)
dviCreatedDate = lens _dviCreatedDate (\s a -> s {_dviCreatedDate = a}) . mapping _Time

-- | The document format, either JSON or YAML.
dviDocumentFormat :: Lens' DocumentVersionInfo (Maybe DocumentFormat)
dviDocumentFormat = lens _dviDocumentFormat (\s a -> s {_dviDocumentFormat = a})

-- | The document name.
dviName :: Lens' DocumentVersionInfo (Maybe Text)
dviName = lens _dviName (\s a -> s {_dviName = a})

-- | The document version.
dviDocumentVersion :: Lens' DocumentVersionInfo (Maybe Text)
dviDocumentVersion = lens _dviDocumentVersion (\s a -> s {_dviDocumentVersion = a})

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
dviStatusInformation :: Lens' DocumentVersionInfo (Maybe Text)
dviStatusInformation = lens _dviStatusInformation (\s a -> s {_dviStatusInformation = a})

-- | An identifier for the default version of the document.
dviIsDefaultVersion :: Lens' DocumentVersionInfo (Maybe Bool)
dviIsDefaultVersion = lens _dviIsDefaultVersion (\s a -> s {_dviIsDefaultVersion = a})

instance FromJSON DocumentVersionInfo where
  parseJSON =
    withObject
      "DocumentVersionInfo"
      ( \x ->
          DocumentVersionInfo'
            <$> (x .:? "Status")
            <*> (x .:? "VersionName")
            <*> (x .:? "CreatedDate")
            <*> (x .:? "DocumentFormat")
            <*> (x .:? "Name")
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "StatusInformation")
            <*> (x .:? "IsDefaultVersion")
      )

instance Hashable DocumentVersionInfo

instance NFData DocumentVersionInfo
