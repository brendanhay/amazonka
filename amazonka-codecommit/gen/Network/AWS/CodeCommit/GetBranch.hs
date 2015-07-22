{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetBranch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a repository branch, including its name and
-- the last commit ID.
--
-- <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_GetBranch.html>
module Network.AWS.CodeCommit.GetBranch
    (
    -- * Request
      GetBranch
    -- ** Request constructor
    , getBranch
    -- ** Request lenses
    , gbrqBranchName
    , gbrqRepositoryName

    -- * Response
    , GetBranchResponse
    -- ** Response constructor
    , getBranchResponse
    -- ** Response lenses
    , gbrsBranch
    , gbrsStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get branch operation.
--
-- /See:/ 'getBranch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrqBranchName'
--
-- * 'gbrqRepositoryName'
data GetBranch = GetBranch'
    { _gbrqBranchName     :: !(Maybe Text)
    , _gbrqRepositoryName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBranch' smart constructor.
getBranch :: GetBranch
getBranch =
    GetBranch'
    { _gbrqBranchName = Nothing
    , _gbrqRepositoryName = Nothing
    }

-- | The name of the branch for which you want to retrieve information.
gbrqBranchName :: Lens' GetBranch (Maybe Text)
gbrqBranchName = lens _gbrqBranchName (\ s a -> s{_gbrqBranchName = a});

-- | FIXME: Undocumented member.
gbrqRepositoryName :: Lens' GetBranch (Maybe Text)
gbrqRepositoryName = lens _gbrqRepositoryName (\ s a -> s{_gbrqRepositoryName = a});

instance AWSRequest GetBranch where
        type Sv GetBranch = CodeCommit
        type Rs GetBranch = GetBranchResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetBranchResponse' <$>
                   (x .?> "branch") <*> (pure (fromEnum s)))

instance ToHeaders GetBranch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetBranch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetBranch where
        toJSON GetBranch'{..}
          = object
              ["branchName" .= _gbrqBranchName,
               "repositoryName" .= _gbrqRepositoryName]

instance ToPath GetBranch where
        toPath = const "/"

instance ToQuery GetBranch where
        toQuery = const mempty

-- | Represents the output of a get branch operation.
--
-- /See:/ 'getBranchResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrsBranch'
--
-- * 'gbrsStatus'
data GetBranchResponse = GetBranchResponse'
    { _gbrsBranch :: !(Maybe BranchInfo)
    , _gbrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBranchResponse' smart constructor.
getBranchResponse :: Int -> GetBranchResponse
getBranchResponse pStatus_ =
    GetBranchResponse'
    { _gbrsBranch = Nothing
    , _gbrsStatus = pStatus_
    }

-- | The name of the branch.
gbrsBranch :: Lens' GetBranchResponse (Maybe BranchInfo)
gbrsBranch = lens _gbrsBranch (\ s a -> s{_gbrsBranch = a});

-- | FIXME: Undocumented member.
gbrsStatus :: Lens' GetBranchResponse Int
gbrsStatus = lens _gbrsStatus (\ s a -> s{_gbrsStatus = a});
