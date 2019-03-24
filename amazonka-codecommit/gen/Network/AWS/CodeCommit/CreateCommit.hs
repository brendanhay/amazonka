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
-- Module      : Network.AWS.CodeCommit.CreateCommit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a commit for a repository on the tip of a specified branch.
--
--
module Network.AWS.CodeCommit.CreateCommit
    (
    -- * Creating a Request
      createCommit
    , CreateCommit
    -- * Request Lenses
    , ccSetFileModes
    , ccEmail
    , ccAuthorName
    , ccParentCommitId
    , ccDeleteFiles
    , ccPutFiles
    , ccCommitMessage
    , ccKeepEmptyFolders
    , ccRepositoryName
    , ccBranchName

    -- * Destructuring the Response
    , createCommitResponse
    , CreateCommitResponse
    -- * Response Lenses
    , ccrsCommitId
    , ccrsTreeId
    , ccrsFilesAdded
    , ccrsFilesUpdated
    , ccrsFilesDeleted
    , ccrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCommit' smart constructor.
data CreateCommit = CreateCommit'
  { _ccSetFileModes     :: !(Maybe [SetFileModeEntry])
  , _ccEmail            :: !(Maybe Text)
  , _ccAuthorName       :: !(Maybe Text)
  , _ccParentCommitId   :: !(Maybe Text)
  , _ccDeleteFiles      :: !(Maybe [DeleteFileEntry])
  , _ccPutFiles         :: !(Maybe [PutFileEntry])
  , _ccCommitMessage    :: !(Maybe Text)
  , _ccKeepEmptyFolders :: !(Maybe Bool)
  , _ccRepositoryName   :: !Text
  , _ccBranchName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCommit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccSetFileModes' - The file modes to update for files in this commit.
--
-- * 'ccEmail' - The email address of the person who created the commit.
--
-- * 'ccAuthorName' - The name of the author who created the commit. This information will be used as both the author and committer for the commit.
--
-- * 'ccParentCommitId' - The ID of the commit that is the parent of the commit you will create. If this is an empty repository, this is not required.
--
-- * 'ccDeleteFiles' - The files to delete in this commit. These files will still exist in prior commits.
--
-- * 'ccPutFiles' - The files to add or update in this commit.
--
-- * 'ccCommitMessage' - The commit message you want to include as part of creating the commit. Commit messages are limited to 256 KB. If no message is specified, a default message will be used.
--
-- * 'ccKeepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file will be created for empty folders.
--
-- * 'ccRepositoryName' - The name of the repository where you will create the commit.
--
-- * 'ccBranchName' - The name of the branch where you will create the commit.
createCommit
    :: Text -- ^ 'ccRepositoryName'
    -> Text -- ^ 'ccBranchName'
    -> CreateCommit
createCommit pRepositoryName_ pBranchName_ =
  CreateCommit'
    { _ccSetFileModes = Nothing
    , _ccEmail = Nothing
    , _ccAuthorName = Nothing
    , _ccParentCommitId = Nothing
    , _ccDeleteFiles = Nothing
    , _ccPutFiles = Nothing
    , _ccCommitMessage = Nothing
    , _ccKeepEmptyFolders = Nothing
    , _ccRepositoryName = pRepositoryName_
    , _ccBranchName = pBranchName_
    }


-- | The file modes to update for files in this commit.
ccSetFileModes :: Lens' CreateCommit [SetFileModeEntry]
ccSetFileModes = lens _ccSetFileModes (\ s a -> s{_ccSetFileModes = a}) . _Default . _Coerce

-- | The email address of the person who created the commit.
ccEmail :: Lens' CreateCommit (Maybe Text)
ccEmail = lens _ccEmail (\ s a -> s{_ccEmail = a})

-- | The name of the author who created the commit. This information will be used as both the author and committer for the commit.
ccAuthorName :: Lens' CreateCommit (Maybe Text)
ccAuthorName = lens _ccAuthorName (\ s a -> s{_ccAuthorName = a})

-- | The ID of the commit that is the parent of the commit you will create. If this is an empty repository, this is not required.
ccParentCommitId :: Lens' CreateCommit (Maybe Text)
ccParentCommitId = lens _ccParentCommitId (\ s a -> s{_ccParentCommitId = a})

-- | The files to delete in this commit. These files will still exist in prior commits.
ccDeleteFiles :: Lens' CreateCommit [DeleteFileEntry]
ccDeleteFiles = lens _ccDeleteFiles (\ s a -> s{_ccDeleteFiles = a}) . _Default . _Coerce

-- | The files to add or update in this commit.
ccPutFiles :: Lens' CreateCommit [PutFileEntry]
ccPutFiles = lens _ccPutFiles (\ s a -> s{_ccPutFiles = a}) . _Default . _Coerce

-- | The commit message you want to include as part of creating the commit. Commit messages are limited to 256 KB. If no message is specified, a default message will be used.
ccCommitMessage :: Lens' CreateCommit (Maybe Text)
ccCommitMessage = lens _ccCommitMessage (\ s a -> s{_ccCommitMessage = a})

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file will be created for empty folders.
ccKeepEmptyFolders :: Lens' CreateCommit (Maybe Bool)
ccKeepEmptyFolders = lens _ccKeepEmptyFolders (\ s a -> s{_ccKeepEmptyFolders = a})

-- | The name of the repository where you will create the commit.
ccRepositoryName :: Lens' CreateCommit Text
ccRepositoryName = lens _ccRepositoryName (\ s a -> s{_ccRepositoryName = a})

-- | The name of the branch where you will create the commit.
ccBranchName :: Lens' CreateCommit Text
ccBranchName = lens _ccBranchName (\ s a -> s{_ccBranchName = a})

instance AWSRequest CreateCommit where
        type Rs CreateCommit = CreateCommitResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 CreateCommitResponse' <$>
                   (x .?> "commitId") <*> (x .?> "treeId") <*>
                     (x .?> "filesAdded" .!@ mempty)
                     <*> (x .?> "filesUpdated" .!@ mempty)
                     <*> (x .?> "filesDeleted" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreateCommit where

instance NFData CreateCommit where

instance ToHeaders CreateCommit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.CreateCommit" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCommit where
        toJSON CreateCommit'{..}
          = object
              (catMaybes
                 [("setFileModes" .=) <$> _ccSetFileModes,
                  ("email" .=) <$> _ccEmail,
                  ("authorName" .=) <$> _ccAuthorName,
                  ("parentCommitId" .=) <$> _ccParentCommitId,
                  ("deleteFiles" .=) <$> _ccDeleteFiles,
                  ("putFiles" .=) <$> _ccPutFiles,
                  ("commitMessage" .=) <$> _ccCommitMessage,
                  ("keepEmptyFolders" .=) <$> _ccKeepEmptyFolders,
                  Just ("repositoryName" .= _ccRepositoryName),
                  Just ("branchName" .= _ccBranchName)])

instance ToPath CreateCommit where
        toPath = const "/"

instance ToQuery CreateCommit where
        toQuery = const mempty

-- | /See:/ 'createCommitResponse' smart constructor.
data CreateCommitResponse = CreateCommitResponse'
  { _ccrsCommitId       :: !(Maybe Text)
  , _ccrsTreeId         :: !(Maybe Text)
  , _ccrsFilesAdded     :: !(Maybe [FileMetadata])
  , _ccrsFilesUpdated   :: !(Maybe [FileMetadata])
  , _ccrsFilesDeleted   :: !(Maybe [FileMetadata])
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCommitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsCommitId' - The full commit ID of the commit that contains your committed file changes.
--
-- * 'ccrsTreeId' - The full SHA-1 pointer of the tree information for the commit that contains the commited file changes.
--
-- * 'ccrsFilesAdded' - The files added as part of the committed file changes.
--
-- * 'ccrsFilesUpdated' - The files updated as part of the commited file changes.
--
-- * 'ccrsFilesDeleted' - The files deleted as part of the committed file changes.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createCommitResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateCommitResponse
createCommitResponse pResponseStatus_ =
  CreateCommitResponse'
    { _ccrsCommitId = Nothing
    , _ccrsTreeId = Nothing
    , _ccrsFilesAdded = Nothing
    , _ccrsFilesUpdated = Nothing
    , _ccrsFilesDeleted = Nothing
    , _ccrsResponseStatus = pResponseStatus_
    }


-- | The full commit ID of the commit that contains your committed file changes.
ccrsCommitId :: Lens' CreateCommitResponse (Maybe Text)
ccrsCommitId = lens _ccrsCommitId (\ s a -> s{_ccrsCommitId = a})

-- | The full SHA-1 pointer of the tree information for the commit that contains the commited file changes.
ccrsTreeId :: Lens' CreateCommitResponse (Maybe Text)
ccrsTreeId = lens _ccrsTreeId (\ s a -> s{_ccrsTreeId = a})

-- | The files added as part of the committed file changes.
ccrsFilesAdded :: Lens' CreateCommitResponse [FileMetadata]
ccrsFilesAdded = lens _ccrsFilesAdded (\ s a -> s{_ccrsFilesAdded = a}) . _Default . _Coerce

-- | The files updated as part of the commited file changes.
ccrsFilesUpdated :: Lens' CreateCommitResponse [FileMetadata]
ccrsFilesUpdated = lens _ccrsFilesUpdated (\ s a -> s{_ccrsFilesUpdated = a}) . _Default . _Coerce

-- | The files deleted as part of the committed file changes.
ccrsFilesDeleted :: Lens' CreateCommitResponse [FileMetadata]
ccrsFilesDeleted = lens _ccrsFilesDeleted (\ s a -> s{_ccrsFilesDeleted = a}) . _Default . _Coerce

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateCommitResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateCommitResponse where
