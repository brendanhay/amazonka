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
-- Module      : Network.AWS.CodeCommit.CreateBranch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new branch in a repository and points the branch to a commit.
--
-- Calling the create branch operation does not set a repository\'s default
-- branch. To do this, call the update default branch operation.
--
-- /See:/ <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_CreateBranch.html AWS API Reference> for CreateBranch.
module Network.AWS.CodeCommit.CreateBranch
    (
    -- * Creating a Request
      createBranch
    , CreateBranch
    -- * Request Lenses
    , cbRepositoryName
    , cbBranchName
    , cbCommitId

    -- * Destructuring the Response
    , createBranchResponse
    , CreateBranchResponse
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.CodeCommit.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create branch operation.
--
-- /See:/ 'createBranch' smart constructor.
data CreateBranch = CreateBranch'
    { _cbRepositoryName :: !Text
    , _cbBranchName     :: !Text
    , _cbCommitId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbRepositoryName'
--
-- * 'cbBranchName'
--
-- * 'cbCommitId'
createBranch
    :: Text -- ^ 'cbRepositoryName'
    -> Text -- ^ 'cbBranchName'
    -> Text -- ^ 'cbCommitId'
    -> CreateBranch
createBranch pRepositoryName_ pBranchName_ pCommitId_ =
    CreateBranch'
    { _cbRepositoryName = pRepositoryName_
    , _cbBranchName = pBranchName_
    , _cbCommitId = pCommitId_
    }

-- | The name of the repository in which you want to create the new branch.
cbRepositoryName :: Lens' CreateBranch Text
cbRepositoryName = lens _cbRepositoryName (\ s a -> s{_cbRepositoryName = a});

-- | The name of the new branch to create.
cbBranchName :: Lens' CreateBranch Text
cbBranchName = lens _cbBranchName (\ s a -> s{_cbBranchName = a});

-- | The ID of the commit to point the new branch to.
--
-- If this commit ID is not specified, the new branch will point to the
-- commit that is pointed to by the repository\'s default branch.
cbCommitId :: Lens' CreateBranch Text
cbCommitId = lens _cbCommitId (\ s a -> s{_cbCommitId = a});

instance AWSRequest CreateBranch where
        type Rs CreateBranch = CreateBranchResponse
        request = postJSON codeCommit
        response = receiveNull CreateBranchResponse'

instance ToHeaders CreateBranch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.CreateBranch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBranch where
        toJSON CreateBranch'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _cbRepositoryName),
                  Just ("branchName" .= _cbBranchName),
                  Just ("commitId" .= _cbCommitId)])

instance ToPath CreateBranch where
        toPath = const "/"

instance ToQuery CreateBranch where
        toQuery = const mempty

-- | /See:/ 'createBranchResponse' smart constructor.
data CreateBranchResponse =
    CreateBranchResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateBranchResponse' with the minimum fields required to make a request.
--
createBranchResponse
    :: CreateBranchResponse
createBranchResponse = CreateBranchResponse'
