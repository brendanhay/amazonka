{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateBranch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new branch in a repository and points the branch to a commit.
--
-- Calling the create branch operation does not set a repository\'s default
-- branch. To do this, call the update default branch operation.
--
-- <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_CreateBranch.html>
module Network.AWS.CodeCommit.CreateBranch
    (
    -- * Request
      CreateBranch
    -- ** Request constructor
    , createBranch
    -- ** Request lenses
    , cbRepositoryName
    , cbBranchName
    , cbCommitId

    -- * Response
    , CreateBranchResponse
    -- ** Response constructor
    , createBranchResponse
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create branch operation.
--
-- /See:/ 'createBranch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbRepositoryName'
--
-- * 'cbBranchName'
--
-- * 'cbCommitId'
data CreateBranch = CreateBranch'
    { _cbRepositoryName :: !Text
    , _cbBranchName     :: !Text
    , _cbCommitId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateBranch' smart constructor.
createBranch :: Text -> Text -> Text -> CreateBranch
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
        type Sv CreateBranch = CodeCommit
        type Rs CreateBranch = CreateBranchResponse
        request = postJSON "CreateBranch"
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
              ["repositoryName" .= _cbRepositoryName,
               "branchName" .= _cbBranchName,
               "commitId" .= _cbCommitId]

instance ToPath CreateBranch where
        toPath = const "/"

instance ToQuery CreateBranch where
        toQuery = const mempty

-- | /See:/ 'createBranchResponse' smart constructor.
data CreateBranchResponse =
    CreateBranchResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateBranchResponse' smart constructor.
createBranchResponse :: CreateBranchResponse
createBranchResponse = CreateBranchResponse'
