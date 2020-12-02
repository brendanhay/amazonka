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
-- Module      : Network.AWS.CodeCommit.PutFile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a file in an AWS CodeCommit repository.
--
--
module Network.AWS.CodeCommit.PutFile
    (
    -- * Creating a Request
      putFile
    , PutFile
    -- * Request Lenses
    , pfEmail
    , pfFileMode
    , pfParentCommitId
    , pfName
    , pfCommitMessage
    , pfRepositoryName
    , pfBranchName
    , pfFileContent
    , pfFilePath

    -- * Destructuring the Response
    , putFileResponse
    , PutFileResponse
    -- * Response Lenses
    , pfrsResponseStatus
    , pfrsCommitId
    , pfrsBlobId
    , pfrsTreeId
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putFile' smart constructor.
data PutFile = PutFile'
  { _pfEmail          :: !(Maybe Text)
  , _pfFileMode       :: !(Maybe FileModeTypeEnum)
  , _pfParentCommitId :: !(Maybe Text)
  , _pfName           :: !(Maybe Text)
  , _pfCommitMessage  :: !(Maybe Text)
  , _pfRepositoryName :: !Text
  , _pfBranchName     :: !Text
  , _pfFileContent    :: !Base64
  , _pfFilePath       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfEmail' - An email address for the person adding or updating the file.
--
-- * 'pfFileMode' - The file mode permissions of the blob. Valid file mode permissions are listed below.
--
-- * 'pfParentCommitId' - The full commit ID of the head commit in the branch where you want to add or update the file. If the commit ID does not match the ID of the head commit at the time of the operation, an error will occur, and the file will not be added or updated.
--
-- * 'pfName' - The name of the person adding or updating the file. While optional, adding a name is strongly encouraged in order to provide a more useful commit history for your repository.
--
-- * 'pfCommitMessage' - A message about why this file was added or updated. While optional, adding a message is strongly encouraged in order to provide a more useful commit history for your repository.
--
-- * 'pfRepositoryName' - The name of the repository where you want to add or update the file.
--
-- * 'pfBranchName' - The name of the branch where you want to add or update the file.
--
-- * 'pfFileContent' - The content of the file, in binary object format. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'pfFilePath' - The name of the file you want to add or update, including the relative path to the file in the repository.
putFile
    :: Text -- ^ 'pfRepositoryName'
    -> Text -- ^ 'pfBranchName'
    -> ByteString -- ^ 'pfFileContent'
    -> Text -- ^ 'pfFilePath'
    -> PutFile
putFile pRepositoryName_ pBranchName_ pFileContent_ pFilePath_ =
  PutFile'
    { _pfEmail = Nothing
    , _pfFileMode = Nothing
    , _pfParentCommitId = Nothing
    , _pfName = Nothing
    , _pfCommitMessage = Nothing
    , _pfRepositoryName = pRepositoryName_
    , _pfBranchName = pBranchName_
    , _pfFileContent = _Base64 # pFileContent_
    , _pfFilePath = pFilePath_
    }


-- | An email address for the person adding or updating the file.
pfEmail :: Lens' PutFile (Maybe Text)
pfEmail = lens _pfEmail (\ s a -> s{_pfEmail = a})

-- | The file mode permissions of the blob. Valid file mode permissions are listed below.
pfFileMode :: Lens' PutFile (Maybe FileModeTypeEnum)
pfFileMode = lens _pfFileMode (\ s a -> s{_pfFileMode = a})

-- | The full commit ID of the head commit in the branch where you want to add or update the file. If the commit ID does not match the ID of the head commit at the time of the operation, an error will occur, and the file will not be added or updated.
pfParentCommitId :: Lens' PutFile (Maybe Text)
pfParentCommitId = lens _pfParentCommitId (\ s a -> s{_pfParentCommitId = a})

-- | The name of the person adding or updating the file. While optional, adding a name is strongly encouraged in order to provide a more useful commit history for your repository.
pfName :: Lens' PutFile (Maybe Text)
pfName = lens _pfName (\ s a -> s{_pfName = a})

-- | A message about why this file was added or updated. While optional, adding a message is strongly encouraged in order to provide a more useful commit history for your repository.
pfCommitMessage :: Lens' PutFile (Maybe Text)
pfCommitMessage = lens _pfCommitMessage (\ s a -> s{_pfCommitMessage = a})

-- | The name of the repository where you want to add or update the file.
pfRepositoryName :: Lens' PutFile Text
pfRepositoryName = lens _pfRepositoryName (\ s a -> s{_pfRepositoryName = a})

-- | The name of the branch where you want to add or update the file.
pfBranchName :: Lens' PutFile Text
pfBranchName = lens _pfBranchName (\ s a -> s{_pfBranchName = a})

-- | The content of the file, in binary object format. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
pfFileContent :: Lens' PutFile ByteString
pfFileContent = lens _pfFileContent (\ s a -> s{_pfFileContent = a}) . _Base64

-- | The name of the file you want to add or update, including the relative path to the file in the repository.
pfFilePath :: Lens' PutFile Text
pfFilePath = lens _pfFilePath (\ s a -> s{_pfFilePath = a})

instance AWSRequest PutFile where
        type Rs PutFile = PutFileResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 PutFileResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "commitId") <*>
                     (x .:> "blobId")
                     <*> (x .:> "treeId"))

instance Hashable PutFile where

instance NFData PutFile where

instance ToHeaders PutFile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.PutFile" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutFile where
        toJSON PutFile'{..}
          = object
              (catMaybes
                 [("email" .=) <$> _pfEmail,
                  ("fileMode" .=) <$> _pfFileMode,
                  ("parentCommitId" .=) <$> _pfParentCommitId,
                  ("name" .=) <$> _pfName,
                  ("commitMessage" .=) <$> _pfCommitMessage,
                  Just ("repositoryName" .= _pfRepositoryName),
                  Just ("branchName" .= _pfBranchName),
                  Just ("fileContent" .= _pfFileContent),
                  Just ("filePath" .= _pfFilePath)])

instance ToPath PutFile where
        toPath = const "/"

instance ToQuery PutFile where
        toQuery = const mempty

-- | /See:/ 'putFileResponse' smart constructor.
data PutFileResponse = PutFileResponse'
  { _pfrsResponseStatus :: !Int
  , _pfrsCommitId       :: !Text
  , _pfrsBlobId         :: !Text
  , _pfrsTreeId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutFileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfrsResponseStatus' - -- | The response status code.
--
-- * 'pfrsCommitId' - The full SHA of the commit that contains this file change.
--
-- * 'pfrsBlobId' - The ID of the blob, which is its SHA-1 pointer.
--
-- * 'pfrsTreeId' - Tree information for the commit that contains this file change.
putFileResponse
    :: Int -- ^ 'pfrsResponseStatus'
    -> Text -- ^ 'pfrsCommitId'
    -> Text -- ^ 'pfrsBlobId'
    -> Text -- ^ 'pfrsTreeId'
    -> PutFileResponse
putFileResponse pResponseStatus_ pCommitId_ pBlobId_ pTreeId_ =
  PutFileResponse'
    { _pfrsResponseStatus = pResponseStatus_
    , _pfrsCommitId = pCommitId_
    , _pfrsBlobId = pBlobId_
    , _pfrsTreeId = pTreeId_
    }


-- | -- | The response status code.
pfrsResponseStatus :: Lens' PutFileResponse Int
pfrsResponseStatus = lens _pfrsResponseStatus (\ s a -> s{_pfrsResponseStatus = a})

-- | The full SHA of the commit that contains this file change.
pfrsCommitId :: Lens' PutFileResponse Text
pfrsCommitId = lens _pfrsCommitId (\ s a -> s{_pfrsCommitId = a})

-- | The ID of the blob, which is its SHA-1 pointer.
pfrsBlobId :: Lens' PutFileResponse Text
pfrsBlobId = lens _pfrsBlobId (\ s a -> s{_pfrsBlobId = a})

-- | Tree information for the commit that contains this file change.
pfrsTreeId :: Lens' PutFileResponse Text
pfrsTreeId = lens _pfrsTreeId (\ s a -> s{_pfrsTreeId = a})

instance NFData PutFileResponse where
