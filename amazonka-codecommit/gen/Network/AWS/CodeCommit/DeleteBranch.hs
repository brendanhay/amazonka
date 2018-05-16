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
-- Module      : Network.AWS.CodeCommit.DeleteBranch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a branch from a repository, unless that branch is the default branch for the repository.
--
--
module Network.AWS.CodeCommit.DeleteBranch
    (
    -- * Creating a Request
      deleteBranch
    , DeleteBranch
    -- * Request Lenses
    , dbRepositoryName
    , dbBranchName

    -- * Destructuring the Response
    , deleteBranchResponse
    , DeleteBranchResponse
    -- * Response Lenses
    , dbrsDeletedBranch
    , dbrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a delete branch operation.
--
--
--
-- /See:/ 'deleteBranch' smart constructor.
data DeleteBranch = DeleteBranch'
  { _dbRepositoryName :: !Text
  , _dbBranchName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbRepositoryName' - The name of the repository that contains the branch to be deleted.
--
-- * 'dbBranchName' - The name of the branch to delete.
deleteBranch
    :: Text -- ^ 'dbRepositoryName'
    -> Text -- ^ 'dbBranchName'
    -> DeleteBranch
deleteBranch pRepositoryName_ pBranchName_ =
  DeleteBranch'
    {_dbRepositoryName = pRepositoryName_, _dbBranchName = pBranchName_}


-- | The name of the repository that contains the branch to be deleted.
dbRepositoryName :: Lens' DeleteBranch Text
dbRepositoryName = lens _dbRepositoryName (\ s a -> s{_dbRepositoryName = a})

-- | The name of the branch to delete.
dbBranchName :: Lens' DeleteBranch Text
dbBranchName = lens _dbBranchName (\ s a -> s{_dbBranchName = a})

instance AWSRequest DeleteBranch where
        type Rs DeleteBranch = DeleteBranchResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBranchResponse' <$>
                   (x .?> "deletedBranch") <*> (pure (fromEnum s)))

instance Hashable DeleteBranch where

instance NFData DeleteBranch where

instance ToHeaders DeleteBranch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.DeleteBranch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteBranch where
        toJSON DeleteBranch'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _dbRepositoryName),
                  Just ("branchName" .= _dbBranchName)])

instance ToPath DeleteBranch where
        toPath = const "/"

instance ToQuery DeleteBranch where
        toQuery = const mempty

-- | Represents the output of a delete branch operation.
--
--
--
-- /See:/ 'deleteBranchResponse' smart constructor.
data DeleteBranchResponse = DeleteBranchResponse'
  { _dbrsDeletedBranch  :: !(Maybe BranchInfo)
  , _dbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBranchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsDeletedBranch' - Information about the branch deleted by the operation, including the branch name and the commit ID that was the tip of the branch.
--
-- * 'dbrsResponseStatus' - -- | The response status code.
deleteBranchResponse
    :: Int -- ^ 'dbrsResponseStatus'
    -> DeleteBranchResponse
deleteBranchResponse pResponseStatus_ =
  DeleteBranchResponse'
    {_dbrsDeletedBranch = Nothing, _dbrsResponseStatus = pResponseStatus_}


-- | Information about the branch deleted by the operation, including the branch name and the commit ID that was the tip of the branch.
dbrsDeletedBranch :: Lens' DeleteBranchResponse (Maybe BranchInfo)
dbrsDeletedBranch = lens _dbrsDeletedBranch (\ s a -> s{_dbrsDeletedBranch = a})

-- | -- | The response status code.
dbrsResponseStatus :: Lens' DeleteBranchResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\ s a -> s{_dbrsResponseStatus = a})

instance NFData DeleteBranchResponse where
