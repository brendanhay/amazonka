{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetFile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the base-64 encoded contents of a specified file and its metadata.
--
--
module Network.AWS.CodeCommit.GetFile
    (
    -- * Creating a Request
      getFile
    , GetFile
    -- * Request Lenses
    , getCommitSpecifier
    , getRepositoryName
    , getFilePath

    -- * Destructuring the Response
    , getFileResponse
    , GetFileResponse
    -- * Response Lenses
    , getrsResponseStatus
    , getrsCommitId
    , getrsBlobId
    , getrsFilePath
    , getrsFileMode
    , getrsFileSize
    , getrsFileContent
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFile' smart constructor.
data GetFile = GetFile'
  { _getCommitSpecifier :: !(Maybe Text)
  , _getRepositoryName  :: !Text
  , _getFilePath        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getCommitSpecifier' - The fully-quaified reference that identifies the commit that contains the file. For example, you could specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, then the head commit will be used.
--
-- * 'getRepositoryName' - The name of the repository that contains the file.
--
-- * 'getFilePath' - The fully-qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully-qualified path to a file named file.md in a folder named examples.
getFile
    :: Text -- ^ 'getRepositoryName'
    -> Text -- ^ 'getFilePath'
    -> GetFile
getFile pRepositoryName_ pFilePath_ =
  GetFile'
    { _getCommitSpecifier = Nothing
    , _getRepositoryName = pRepositoryName_
    , _getFilePath = pFilePath_
    }


-- | The fully-quaified reference that identifies the commit that contains the file. For example, you could specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, then the head commit will be used.
getCommitSpecifier :: Lens' GetFile (Maybe Text)
getCommitSpecifier = lens _getCommitSpecifier (\ s a -> s{_getCommitSpecifier = a})

-- | The name of the repository that contains the file.
getRepositoryName :: Lens' GetFile Text
getRepositoryName = lens _getRepositoryName (\ s a -> s{_getRepositoryName = a})

-- | The fully-qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully-qualified path to a file named file.md in a folder named examples.
getFilePath :: Lens' GetFile Text
getFilePath = lens _getFilePath (\ s a -> s{_getFilePath = a})

instance AWSRequest GetFile where
        type Rs GetFile = GetFileResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetFileResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "commitId") <*>
                     (x .:> "blobId")
                     <*> (x .:> "filePath")
                     <*> (x .:> "fileMode")
                     <*> (x .:> "fileSize")
                     <*> (x .:> "fileContent"))

instance Hashable GetFile where

instance NFData GetFile where

instance ToHeaders GetFile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetFile" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetFile where
        toJSON GetFile'{..}
          = object
              (catMaybes
                 [("commitSpecifier" .=) <$> _getCommitSpecifier,
                  Just ("repositoryName" .= _getRepositoryName),
                  Just ("filePath" .= _getFilePath)])

instance ToPath GetFile where
        toPath = const "/"

instance ToQuery GetFile where
        toQuery = const mempty

-- | /See:/ 'getFileResponse' smart constructor.
data GetFileResponse = GetFileResponse'
  { _getrsResponseStatus :: !Int
  , _getrsCommitId       :: !Text
  , _getrsBlobId         :: !Text
  , _getrsFilePath       :: !Text
  , _getrsFileMode       :: !FileModeTypeEnum
  , _getrsFileSize       :: !Integer
  , _getrsFileContent    :: !Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsResponseStatus' - -- | The response status code.
--
-- * 'getrsCommitId' - The full commit ID of the commit that contains the content returned by GetFile.
--
-- * 'getrsBlobId' - The blob ID of the object that represents the file content.
--
-- * 'getrsFilePath' - The fully qualified path to the specified file. This returns the name and extension of the file.
--
-- * 'getrsFileMode' - The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
--
-- * 'getrsFileSize' - The size of the contents of the file, in bytes.
--
-- * 'getrsFileContent' - The base-64 encoded binary data object that represents the content of the file.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
getFileResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> Text -- ^ 'getrsCommitId'
    -> Text -- ^ 'getrsBlobId'
    -> Text -- ^ 'getrsFilePath'
    -> FileModeTypeEnum -- ^ 'getrsFileMode'
    -> Integer -- ^ 'getrsFileSize'
    -> ByteString -- ^ 'getrsFileContent'
    -> GetFileResponse
getFileResponse pResponseStatus_ pCommitId_ pBlobId_ pFilePath_ pFileMode_ pFileSize_ pFileContent_ =
  GetFileResponse'
    { _getrsResponseStatus = pResponseStatus_
    , _getrsCommitId = pCommitId_
    , _getrsBlobId = pBlobId_
    , _getrsFilePath = pFilePath_
    , _getrsFileMode = pFileMode_
    , _getrsFileSize = pFileSize_
    , _getrsFileContent = _Base64 # pFileContent_
    }


-- | -- | The response status code.
getrsResponseStatus :: Lens' GetFileResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a})

-- | The full commit ID of the commit that contains the content returned by GetFile.
getrsCommitId :: Lens' GetFileResponse Text
getrsCommitId = lens _getrsCommitId (\ s a -> s{_getrsCommitId = a})

-- | The blob ID of the object that represents the file content.
getrsBlobId :: Lens' GetFileResponse Text
getrsBlobId = lens _getrsBlobId (\ s a -> s{_getrsBlobId = a})

-- | The fully qualified path to the specified file. This returns the name and extension of the file.
getrsFilePath :: Lens' GetFileResponse Text
getrsFilePath = lens _getrsFilePath (\ s a -> s{_getrsFilePath = a})

-- | The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
getrsFileMode :: Lens' GetFileResponse FileModeTypeEnum
getrsFileMode = lens _getrsFileMode (\ s a -> s{_getrsFileMode = a})

-- | The size of the contents of the file, in bytes.
getrsFileSize :: Lens' GetFileResponse Integer
getrsFileSize = lens _getrsFileSize (\ s a -> s{_getrsFileSize = a})

-- | The base-64 encoded binary data object that represents the content of the file.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
getrsFileContent :: Lens' GetFileResponse ByteString
getrsFileContent = lens _getrsFileContent (\ s a -> s{_getrsFileContent = a}) . _Base64

instance NFData GetFileResponse where
