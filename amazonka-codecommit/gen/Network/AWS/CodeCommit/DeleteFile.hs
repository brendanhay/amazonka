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
-- Module      : Network.AWS.CodeCommit.DeleteFile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified file from a specified branch. A commit is created on the branch that contains the revision. The file will still exist in the commits prior to the commit that contains the deletion.
--
--
module Network.AWS.CodeCommit.DeleteFile
    (
    -- * Creating a Request
      deleteFile
    , DeleteFile
    -- * Request Lenses
    , dfEmail
    , dfName
    , dfCommitMessage
    , dfKeepEmptyFolders
    , dfRepositoryName
    , dfBranchName
    , dfFilePath
    , dfParentCommitId

    -- * Destructuring the Response
    , deleteFileResponse
    , DeleteFileResponse
    -- * Response Lenses
    , dfrsResponseStatus
    , dfrsCommitId
    , dfrsBlobId
    , dfrsTreeId
    , dfrsFilePath
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFile' smart constructor.
data DeleteFile = DeleteFile'
  { _dfEmail            :: !(Maybe Text)
  , _dfName             :: !(Maybe Text)
  , _dfCommitMessage    :: !(Maybe Text)
  , _dfKeepEmptyFolders :: !(Maybe Bool)
  , _dfRepositoryName   :: !Text
  , _dfBranchName       :: !Text
  , _dfFilePath         :: !Text
  , _dfParentCommitId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfEmail' - The email address for the commit that deletes the file. If no email address is specified, the email address will be left blank.
--
-- * 'dfName' - The name of the author of the commit that deletes the file. If no name is specified, the user's ARN will be used as the author name and committer name.
--
-- * 'dfCommitMessage' - The commit message you want to include as part of deleting the file. Commit messages are limited to 256 KB. If no message is specified, a default message will be used.
--
-- * 'dfKeepEmptyFolders' - Specifies whether to delete the folder or directory that contains the file you want to delete if that file is the only object in the folder or directory. By default, empty folders will be deleted. This includes empty folders that are part of the directory structure. For example, if the path to a file is dir1/dir2/dir3/dir4, and dir2 and dir3 are empty, deleting the last file in dir4 will also delete the empty folders dir4, dir3, and dir2.
--
-- * 'dfRepositoryName' - The name of the repository that contains the file to delete.
--
-- * 'dfBranchName' - The name of the branch where the commit will be made deleting the file.
--
-- * 'dfFilePath' - The fully-qualified path to the file that will be deleted, including the full name and extension of that file. For example, /examples/file.md is a fully qualified path to a file named file.md in a folder named examples.
--
-- * 'dfParentCommitId' - The ID of the commit that is the tip of the branch where you want to create the commit that will delete the file. This must be the HEAD commit for the branch. The commit that deletes the file will be created from this commit ID.
deleteFile
    :: Text -- ^ 'dfRepositoryName'
    -> Text -- ^ 'dfBranchName'
    -> Text -- ^ 'dfFilePath'
    -> Text -- ^ 'dfParentCommitId'
    -> DeleteFile
deleteFile pRepositoryName_ pBranchName_ pFilePath_ pParentCommitId_ =
  DeleteFile'
    { _dfEmail = Nothing
    , _dfName = Nothing
    , _dfCommitMessage = Nothing
    , _dfKeepEmptyFolders = Nothing
    , _dfRepositoryName = pRepositoryName_
    , _dfBranchName = pBranchName_
    , _dfFilePath = pFilePath_
    , _dfParentCommitId = pParentCommitId_
    }


-- | The email address for the commit that deletes the file. If no email address is specified, the email address will be left blank.
dfEmail :: Lens' DeleteFile (Maybe Text)
dfEmail = lens _dfEmail (\ s a -> s{_dfEmail = a})

-- | The name of the author of the commit that deletes the file. If no name is specified, the user's ARN will be used as the author name and committer name.
dfName :: Lens' DeleteFile (Maybe Text)
dfName = lens _dfName (\ s a -> s{_dfName = a})

-- | The commit message you want to include as part of deleting the file. Commit messages are limited to 256 KB. If no message is specified, a default message will be used.
dfCommitMessage :: Lens' DeleteFile (Maybe Text)
dfCommitMessage = lens _dfCommitMessage (\ s a -> s{_dfCommitMessage = a})

-- | Specifies whether to delete the folder or directory that contains the file you want to delete if that file is the only object in the folder or directory. By default, empty folders will be deleted. This includes empty folders that are part of the directory structure. For example, if the path to a file is dir1/dir2/dir3/dir4, and dir2 and dir3 are empty, deleting the last file in dir4 will also delete the empty folders dir4, dir3, and dir2.
dfKeepEmptyFolders :: Lens' DeleteFile (Maybe Bool)
dfKeepEmptyFolders = lens _dfKeepEmptyFolders (\ s a -> s{_dfKeepEmptyFolders = a})

-- | The name of the repository that contains the file to delete.
dfRepositoryName :: Lens' DeleteFile Text
dfRepositoryName = lens _dfRepositoryName (\ s a -> s{_dfRepositoryName = a})

-- | The name of the branch where the commit will be made deleting the file.
dfBranchName :: Lens' DeleteFile Text
dfBranchName = lens _dfBranchName (\ s a -> s{_dfBranchName = a})

-- | The fully-qualified path to the file that will be deleted, including the full name and extension of that file. For example, /examples/file.md is a fully qualified path to a file named file.md in a folder named examples.
dfFilePath :: Lens' DeleteFile Text
dfFilePath = lens _dfFilePath (\ s a -> s{_dfFilePath = a})

-- | The ID of the commit that is the tip of the branch where you want to create the commit that will delete the file. This must be the HEAD commit for the branch. The commit that deletes the file will be created from this commit ID.
dfParentCommitId :: Lens' DeleteFile Text
dfParentCommitId = lens _dfParentCommitId (\ s a -> s{_dfParentCommitId = a})

instance AWSRequest DeleteFile where
        type Rs DeleteFile = DeleteFileResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 DeleteFileResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "commitId") <*>
                     (x .:> "blobId")
                     <*> (x .:> "treeId")
                     <*> (x .:> "filePath"))

instance Hashable DeleteFile where

instance NFData DeleteFile where

instance ToHeaders DeleteFile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.DeleteFile" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteFile where
        toJSON DeleteFile'{..}
          = object
              (catMaybes
                 [("email" .=) <$> _dfEmail, ("name" .=) <$> _dfName,
                  ("commitMessage" .=) <$> _dfCommitMessage,
                  ("keepEmptyFolders" .=) <$> _dfKeepEmptyFolders,
                  Just ("repositoryName" .= _dfRepositoryName),
                  Just ("branchName" .= _dfBranchName),
                  Just ("filePath" .= _dfFilePath),
                  Just ("parentCommitId" .= _dfParentCommitId)])

instance ToPath DeleteFile where
        toPath = const "/"

instance ToQuery DeleteFile where
        toQuery = const mempty

-- | /See:/ 'deleteFileResponse' smart constructor.
data DeleteFileResponse = DeleteFileResponse'
  { _dfrsResponseStatus :: !Int
  , _dfrsCommitId       :: !Text
  , _dfrsBlobId         :: !Text
  , _dfrsTreeId         :: !Text
  , _dfrsFilePath       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsResponseStatus' - -- | The response status code.
--
-- * 'dfrsCommitId' - The full commit ID of the commit that contains the change that deletes the file.
--
-- * 'dfrsBlobId' - The blob ID removed from the tree as part of deleting the file.
--
-- * 'dfrsTreeId' - The full SHA-1 pointer of the tree information for the commit that contains the delete file change.
--
-- * 'dfrsFilePath' - The fully-qualified path to the file that will be deleted, including the full name and extension of that file.
deleteFileResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> Text -- ^ 'dfrsCommitId'
    -> Text -- ^ 'dfrsBlobId'
    -> Text -- ^ 'dfrsTreeId'
    -> Text -- ^ 'dfrsFilePath'
    -> DeleteFileResponse
deleteFileResponse pResponseStatus_ pCommitId_ pBlobId_ pTreeId_ pFilePath_ =
  DeleteFileResponse'
    { _dfrsResponseStatus = pResponseStatus_
    , _dfrsCommitId = pCommitId_
    , _dfrsBlobId = pBlobId_
    , _dfrsTreeId = pTreeId_
    , _dfrsFilePath = pFilePath_
    }


-- | -- | The response status code.
dfrsResponseStatus :: Lens' DeleteFileResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a})

-- | The full commit ID of the commit that contains the change that deletes the file.
dfrsCommitId :: Lens' DeleteFileResponse Text
dfrsCommitId = lens _dfrsCommitId (\ s a -> s{_dfrsCommitId = a})

-- | The blob ID removed from the tree as part of deleting the file.
dfrsBlobId :: Lens' DeleteFileResponse Text
dfrsBlobId = lens _dfrsBlobId (\ s a -> s{_dfrsBlobId = a})

-- | The full SHA-1 pointer of the tree information for the commit that contains the delete file change.
dfrsTreeId :: Lens' DeleteFileResponse Text
dfrsTreeId = lens _dfrsTreeId (\ s a -> s{_dfrsTreeId = a})

-- | The fully-qualified path to the file that will be deleted, including the full name and extension of that file.
dfrsFilePath :: Lens' DeleteFileResponse Text
dfrsFilePath = lens _dfrsFilePath (\ s a -> s{_dfrsFilePath = a})

instance NFData DeleteFileResponse where
