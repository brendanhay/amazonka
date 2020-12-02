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
-- Module      : Network.AWS.CodeCommit.GetCommit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a commit, including commit message and committer information.
--
--
module Network.AWS.CodeCommit.GetCommit
    (
    -- * Creating a Request
      getCommit
    , GetCommit
    -- * Request Lenses
    , gcRepositoryName
    , gcCommitId

    -- * Destructuring the Response
    , getCommitResponse
    , GetCommitResponse
    -- * Response Lenses
    , gcrsResponseStatus
    , gcrsCommit
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a get commit operation.
--
--
--
-- /See:/ 'getCommit' smart constructor.
data GetCommit = GetCommit'
  { _gcRepositoryName :: !Text
  , _gcCommitId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcRepositoryName' - The name of the repository to which the commit was made.
--
-- * 'gcCommitId' - The commit ID. Commit IDs are the full SHA of the commit.
getCommit
    :: Text -- ^ 'gcRepositoryName'
    -> Text -- ^ 'gcCommitId'
    -> GetCommit
getCommit pRepositoryName_ pCommitId_ =
  GetCommit' {_gcRepositoryName = pRepositoryName_, _gcCommitId = pCommitId_}


-- | The name of the repository to which the commit was made.
gcRepositoryName :: Lens' GetCommit Text
gcRepositoryName = lens _gcRepositoryName (\ s a -> s{_gcRepositoryName = a})

-- | The commit ID. Commit IDs are the full SHA of the commit.
gcCommitId :: Lens' GetCommit Text
gcCommitId = lens _gcCommitId (\ s a -> s{_gcCommitId = a})

instance AWSRequest GetCommit where
        type Rs GetCommit = GetCommitResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetCommitResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "commit"))

instance Hashable GetCommit where

instance NFData GetCommit where

instance ToHeaders GetCommit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetCommit" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCommit where
        toJSON GetCommit'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _gcRepositoryName),
                  Just ("commitId" .= _gcCommitId)])

instance ToPath GetCommit where
        toPath = const "/"

instance ToQuery GetCommit where
        toQuery = const mempty

-- | Represents the output of a get commit operation.
--
--
--
-- /See:/ 'getCommitResponse' smart constructor.
data GetCommitResponse = GetCommitResponse'
  { _gcrsResponseStatus :: !Int
  , _gcrsCommit         :: !Commit
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsResponseStatus' - -- | The response status code.
--
-- * 'gcrsCommit' - A commit data type object that contains information about the specified commit.
getCommitResponse
    :: Int -- ^ 'gcrsResponseStatus'
    -> Commit -- ^ 'gcrsCommit'
    -> GetCommitResponse
getCommitResponse pResponseStatus_ pCommit_ =
  GetCommitResponse'
    {_gcrsResponseStatus = pResponseStatus_, _gcrsCommit = pCommit_}


-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetCommitResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\ s a -> s{_gcrsResponseStatus = a})

-- | A commit data type object that contains information about the specified commit.
gcrsCommit :: Lens' GetCommitResponse Commit
gcrsCommit = lens _gcrsCommit (\ s a -> s{_gcrsCommit = a})

instance NFData GetCommitResponse where
