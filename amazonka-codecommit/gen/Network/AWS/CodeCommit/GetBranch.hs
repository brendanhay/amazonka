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
-- Module      : Network.AWS.CodeCommit.GetBranch
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a repository branch, including its name and the last commit ID.
module Network.AWS.CodeCommit.GetBranch
    (
    -- * Creating a Request
      getBranch
    , GetBranch
    -- * Request Lenses
    , gbBranchName
    , gbRepositoryName

    -- * Destructuring the Response
    , getBranchResponse
    , GetBranchResponse
    -- * Response Lenses
    , gbrsBranch
    , gbrsResponseStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.CodeCommit.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get branch operation.
--
-- /See:/ 'getBranch' smart constructor.
data GetBranch = GetBranch'
    { _gbBranchName     :: !(Maybe Text)
    , _gbRepositoryName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbBranchName'
--
-- * 'gbRepositoryName'
getBranch
    :: GetBranch
getBranch =
    GetBranch'
    { _gbBranchName = Nothing
    , _gbRepositoryName = Nothing
    }

-- | The name of the branch for which you want to retrieve information.
gbBranchName :: Lens' GetBranch (Maybe Text)
gbBranchName = lens _gbBranchName (\ s a -> s{_gbBranchName = a});

-- | The name of the repository that contains the branch for which you want to retrieve information.
gbRepositoryName :: Lens' GetBranch (Maybe Text)
gbRepositoryName = lens _gbRepositoryName (\ s a -> s{_gbRepositoryName = a});

instance AWSRequest GetBranch where
        type Rs GetBranch = GetBranchResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetBranchResponse' <$>
                   (x .?> "branch") <*> (pure (fromEnum s)))

instance Hashable GetBranch

instance NFData GetBranch

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
              (catMaybes
                 [("branchName" .=) <$> _gbBranchName,
                  ("repositoryName" .=) <$> _gbRepositoryName])

instance ToPath GetBranch where
        toPath = const "/"

instance ToQuery GetBranch where
        toQuery = const mempty

-- | Represents the output of a get branch operation.
--
-- /See:/ 'getBranchResponse' smart constructor.
data GetBranchResponse = GetBranchResponse'
    { _gbrsBranch         :: !(Maybe BranchInfo)
    , _gbrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBranchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrsBranch'
--
-- * 'gbrsResponseStatus'
getBranchResponse
    :: Int -- ^ 'gbrsResponseStatus'
    -> GetBranchResponse
getBranchResponse pResponseStatus_ =
    GetBranchResponse'
    { _gbrsBranch = Nothing
    , _gbrsResponseStatus = pResponseStatus_
    }

-- | The name of the branch.
gbrsBranch :: Lens' GetBranchResponse (Maybe BranchInfo)
gbrsBranch = lens _gbrsBranch (\ s a -> s{_gbrsBranch = a});

-- | The response status code.
gbrsResponseStatus :: Lens' GetBranchResponse Int
gbrsResponseStatus = lens _gbrsResponseStatus (\ s a -> s{_gbrsResponseStatus = a});

instance NFData GetBranchResponse
