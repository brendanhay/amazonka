{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of the specified Systems Manager document.
module Network.AWS.SSM.GetDocument
  ( -- * Creating a Request
    getDocument,
    GetDocument,

    -- * Request Lenses
    gdVersionName,
    gdDocumentFormat,
    gdDocumentVersion,
    gdName,

    -- * Destructuring the Response
    getDocumentResponse,
    GetDocumentResponse,

    -- * Response Lenses
    gdrsStatus,
    gdrsDocumentType,
    gdrsVersionName,
    gdrsAttachmentsContent,
    gdrsContent,
    gdrsDocumentFormat,
    gdrsName,
    gdrsDocumentVersion,
    gdrsStatusInformation,
    gdrsRequires,
    gdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'getDocument' smart constructor.
data GetDocument = GetDocument'
  { _gdVersionName :: !(Maybe Text),
    _gdDocumentFormat :: !(Maybe DocumentFormat),
    _gdDocumentVersion :: !(Maybe Text),
    _gdName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdVersionName' - An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document and can't be changed.
--
-- * 'gdDocumentFormat' - Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
--
-- * 'gdDocumentVersion' - The document version for which you want information.
--
-- * 'gdName' - The name of the Systems Manager document.
getDocument ::
  -- | 'gdName'
  Text ->
  GetDocument
getDocument pName_ =
  GetDocument'
    { _gdVersionName = Nothing,
      _gdDocumentFormat = Nothing,
      _gdDocumentVersion = Nothing,
      _gdName = pName_
    }

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document and can't be changed.
gdVersionName :: Lens' GetDocument (Maybe Text)
gdVersionName = lens _gdVersionName (\s a -> s {_gdVersionName = a})

-- | Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
gdDocumentFormat :: Lens' GetDocument (Maybe DocumentFormat)
gdDocumentFormat = lens _gdDocumentFormat (\s a -> s {_gdDocumentFormat = a})

-- | The document version for which you want information.
gdDocumentVersion :: Lens' GetDocument (Maybe Text)
gdDocumentVersion = lens _gdDocumentVersion (\s a -> s {_gdDocumentVersion = a})

-- | The name of the Systems Manager document.
gdName :: Lens' GetDocument Text
gdName = lens _gdName (\s a -> s {_gdName = a})

instance AWSRequest GetDocument where
  type Rs GetDocument = GetDocumentResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            <$> (x .?> "Status")
            <*> (x .?> "DocumentType")
            <*> (x .?> "VersionName")
            <*> (x .?> "AttachmentsContent" .!@ mempty)
            <*> (x .?> "Content")
            <*> (x .?> "DocumentFormat")
            <*> (x .?> "Name")
            <*> (x .?> "DocumentVersion")
            <*> (x .?> "StatusInformation")
            <*> (x .?> "Requires")
            <*> (pure (fromEnum s))
      )

instance Hashable GetDocument

instance NFData GetDocument

instance ToHeaders GetDocument where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.GetDocument" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDocument where
  toJSON GetDocument' {..} =
    object
      ( catMaybes
          [ ("VersionName" .=) <$> _gdVersionName,
            ("DocumentFormat" .=) <$> _gdDocumentFormat,
            ("DocumentVersion" .=) <$> _gdDocumentVersion,
            Just ("Name" .= _gdName)
          ]
      )

instance ToPath GetDocument where
  toPath = const "/"

instance ToQuery GetDocument where
  toQuery = const mempty

-- | /See:/ 'getDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { _gdrsStatus ::
      !(Maybe DocumentStatus),
    _gdrsDocumentType :: !(Maybe DocumentType),
    _gdrsVersionName :: !(Maybe Text),
    _gdrsAttachmentsContent ::
      !(Maybe [AttachmentContent]),
    _gdrsContent :: !(Maybe Text),
    _gdrsDocumentFormat :: !(Maybe DocumentFormat),
    _gdrsName :: !(Maybe Text),
    _gdrsDocumentVersion :: !(Maybe Text),
    _gdrsStatusInformation :: !(Maybe Text),
    _gdrsRequires :: !(Maybe (List1 DocumentRequires)),
    _gdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsStatus' - The status of the Systems Manager document, such as @Creating@ , @Active@ , @Updating@ , @Failed@ , and @Deleting@ .
--
-- * 'gdrsDocumentType' - The document type.
--
-- * 'gdrsVersionName' - The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- * 'gdrsAttachmentsContent' - A description of the document attachments, including names, locations, sizes, and so on.
--
-- * 'gdrsContent' - The contents of the Systems Manager document.
--
-- * 'gdrsDocumentFormat' - The document format, either JSON or YAML.
--
-- * 'gdrsName' - The name of the Systems Manager document.
--
-- * 'gdrsDocumentVersion' - The document version.
--
-- * 'gdrsStatusInformation' - A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- * 'gdrsRequires' - A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDocumentResponse ::
  -- | 'gdrsResponseStatus'
  Int ->
  GetDocumentResponse
getDocumentResponse pResponseStatus_ =
  GetDocumentResponse'
    { _gdrsStatus = Nothing,
      _gdrsDocumentType = Nothing,
      _gdrsVersionName = Nothing,
      _gdrsAttachmentsContent = Nothing,
      _gdrsContent = Nothing,
      _gdrsDocumentFormat = Nothing,
      _gdrsName = Nothing,
      _gdrsDocumentVersion = Nothing,
      _gdrsStatusInformation = Nothing,
      _gdrsRequires = Nothing,
      _gdrsResponseStatus = pResponseStatus_
    }

-- | The status of the Systems Manager document, such as @Creating@ , @Active@ , @Updating@ , @Failed@ , and @Deleting@ .
gdrsStatus :: Lens' GetDocumentResponse (Maybe DocumentStatus)
gdrsStatus = lens _gdrsStatus (\s a -> s {_gdrsStatus = a})

-- | The document type.
gdrsDocumentType :: Lens' GetDocumentResponse (Maybe DocumentType)
gdrsDocumentType = lens _gdrsDocumentType (\s a -> s {_gdrsDocumentType = a})

-- | The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
gdrsVersionName :: Lens' GetDocumentResponse (Maybe Text)
gdrsVersionName = lens _gdrsVersionName (\s a -> s {_gdrsVersionName = a})

-- | A description of the document attachments, including names, locations, sizes, and so on.
gdrsAttachmentsContent :: Lens' GetDocumentResponse [AttachmentContent]
gdrsAttachmentsContent = lens _gdrsAttachmentsContent (\s a -> s {_gdrsAttachmentsContent = a}) . _Default . _Coerce

-- | The contents of the Systems Manager document.
gdrsContent :: Lens' GetDocumentResponse (Maybe Text)
gdrsContent = lens _gdrsContent (\s a -> s {_gdrsContent = a})

-- | The document format, either JSON or YAML.
gdrsDocumentFormat :: Lens' GetDocumentResponse (Maybe DocumentFormat)
gdrsDocumentFormat = lens _gdrsDocumentFormat (\s a -> s {_gdrsDocumentFormat = a})

-- | The name of the Systems Manager document.
gdrsName :: Lens' GetDocumentResponse (Maybe Text)
gdrsName = lens _gdrsName (\s a -> s {_gdrsName = a})

-- | The document version.
gdrsDocumentVersion :: Lens' GetDocumentResponse (Maybe Text)
gdrsDocumentVersion = lens _gdrsDocumentVersion (\s a -> s {_gdrsDocumentVersion = a})

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
gdrsStatusInformation :: Lens' GetDocumentResponse (Maybe Text)
gdrsStatusInformation = lens _gdrsStatusInformation (\s a -> s {_gdrsStatusInformation = a})

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
gdrsRequires :: Lens' GetDocumentResponse (Maybe (NonEmpty DocumentRequires))
gdrsRequires = lens _gdrsRequires (\s a -> s {_gdrsRequires = a}) . mapping _List1

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDocumentResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\s a -> s {_gdrsResponseStatus = a})

instance NFData GetDocumentResponse
