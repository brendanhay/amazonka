{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateDefaultBranch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets or changes the default branch name for the specified repository.
--
-- If you use this operation to change the default branch name to the
-- current default branch name, a success message is returned even though
-- the default branch did not change.
--
-- <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_UpdateDefaultBranch.html>
module Network.AWS.CodeCommit.UpdateDefaultBranch
    (
    -- * Request
      UpdateDefaultBranch
    -- ** Request constructor
    , updateDefaultBranch
    -- ** Request lenses
    , udbrqRepositoryName
    , udbrqDefaultBranchName

    -- * Response
    , UpdateDefaultBranchResponse
    -- ** Response constructor
    , updateDefaultBranchResponse
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an update default branch operation.
--
-- /See:/ 'updateDefaultBranch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udbrqRepositoryName'
--
-- * 'udbrqDefaultBranchName'
data UpdateDefaultBranch = UpdateDefaultBranch'
    { _udbrqRepositoryName    :: !Text
    , _udbrqDefaultBranchName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDefaultBranch' smart constructor.
updateDefaultBranch :: Text -> Text -> UpdateDefaultBranch
updateDefaultBranch pRepositoryName pDefaultBranchName =
    UpdateDefaultBranch'
    { _udbrqRepositoryName = pRepositoryName
    , _udbrqDefaultBranchName = pDefaultBranchName
    }

-- | The name of the repository to set or change the default branch for.
udbrqRepositoryName :: Lens' UpdateDefaultBranch Text
udbrqRepositoryName = lens _udbrqRepositoryName (\ s a -> s{_udbrqRepositoryName = a});

-- | The name of the branch to set as the default.
udbrqDefaultBranchName :: Lens' UpdateDefaultBranch Text
udbrqDefaultBranchName = lens _udbrqDefaultBranchName (\ s a -> s{_udbrqDefaultBranchName = a});

instance AWSRequest UpdateDefaultBranch where
        type Sv UpdateDefaultBranch = CodeCommit
        type Rs UpdateDefaultBranch =
             UpdateDefaultBranchResponse
        request = postJSON
        response = receiveNull UpdateDefaultBranchResponse'

instance ToHeaders UpdateDefaultBranch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.UpdateDefaultBranch" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDefaultBranch where
        toJSON UpdateDefaultBranch'{..}
          = object
              ["repositoryName" .= _udbrqRepositoryName,
               "defaultBranchName" .= _udbrqDefaultBranchName]

instance ToPath UpdateDefaultBranch where
        toPath = const "/"

instance ToQuery UpdateDefaultBranch where
        toQuery = const mempty

-- | /See:/ 'updateDefaultBranchResponse' smart constructor.
data UpdateDefaultBranchResponse =
    UpdateDefaultBranchResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDefaultBranchResponse' smart constructor.
updateDefaultBranchResponse :: UpdateDefaultBranchResponse
updateDefaultBranchResponse = UpdateDefaultBranchResponse'
