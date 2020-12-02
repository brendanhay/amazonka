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
-- Module      : Network.AWS.CodeCommit.UpdateDefaultBranch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or changes the default branch name for the specified repository.
--
--
module Network.AWS.CodeCommit.UpdateDefaultBranch
    (
    -- * Creating a Request
      updateDefaultBranch
    , UpdateDefaultBranch
    -- * Request Lenses
    , udbRepositoryName
    , udbDefaultBranchName

    -- * Destructuring the Response
    , updateDefaultBranchResponse
    , UpdateDefaultBranchResponse
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an update default branch operation.
--
--
--
-- /See:/ 'updateDefaultBranch' smart constructor.
data UpdateDefaultBranch = UpdateDefaultBranch'
  { _udbRepositoryName    :: !Text
  , _udbDefaultBranchName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDefaultBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udbRepositoryName' - The name of the repository to set or change the default branch for.
--
-- * 'udbDefaultBranchName' - The name of the branch to set as the default.
updateDefaultBranch
    :: Text -- ^ 'udbRepositoryName'
    -> Text -- ^ 'udbDefaultBranchName'
    -> UpdateDefaultBranch
updateDefaultBranch pRepositoryName_ pDefaultBranchName_ =
  UpdateDefaultBranch'
    { _udbRepositoryName = pRepositoryName_
    , _udbDefaultBranchName = pDefaultBranchName_
    }


-- | The name of the repository to set or change the default branch for.
udbRepositoryName :: Lens' UpdateDefaultBranch Text
udbRepositoryName = lens _udbRepositoryName (\ s a -> s{_udbRepositoryName = a})

-- | The name of the branch to set as the default.
udbDefaultBranchName :: Lens' UpdateDefaultBranch Text
udbDefaultBranchName = lens _udbDefaultBranchName (\ s a -> s{_udbDefaultBranchName = a})

instance AWSRequest UpdateDefaultBranch where
        type Rs UpdateDefaultBranch =
             UpdateDefaultBranchResponse
        request = postJSON codeCommit
        response = receiveNull UpdateDefaultBranchResponse'

instance Hashable UpdateDefaultBranch where

instance NFData UpdateDefaultBranch where

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
              (catMaybes
                 [Just ("repositoryName" .= _udbRepositoryName),
                  Just ("defaultBranchName" .= _udbDefaultBranchName)])

instance ToPath UpdateDefaultBranch where
        toPath = const "/"

instance ToQuery UpdateDefaultBranch where
        toQuery = const mempty

-- | /See:/ 'updateDefaultBranchResponse' smart constructor.
data UpdateDefaultBranchResponse =
  UpdateDefaultBranchResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDefaultBranchResponse' with the minimum fields required to make a request.
--
updateDefaultBranchResponse
    :: UpdateDefaultBranchResponse
updateDefaultBranchResponse = UpdateDefaultBranchResponse'


instance NFData UpdateDefaultBranchResponse where
