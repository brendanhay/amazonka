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
-- Module      : Network.AWS.CodeCommit.MergePullRequestByFastForward
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes a pull request and attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the fast-forward merge option.
--
--
module Network.AWS.CodeCommit.MergePullRequestByFastForward
    (
    -- * Creating a Request
      mergePullRequestByFastForward
    , MergePullRequestByFastForward
    -- * Request Lenses
    , mprbffSourceCommitId
    , mprbffPullRequestId
    , mprbffRepositoryName

    -- * Destructuring the Response
    , mergePullRequestByFastForwardResponse
    , MergePullRequestByFastForwardResponse
    -- * Response Lenses
    , mprbffrsPullRequest
    , mprbffrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'mergePullRequestByFastForward' smart constructor.
data MergePullRequestByFastForward = MergePullRequestByFastForward'
  { _mprbffSourceCommitId :: !(Maybe Text)
  , _mprbffPullRequestId  :: !Text
  , _mprbffRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MergePullRequestByFastForward' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mprbffSourceCommitId' - The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
--
-- * 'mprbffPullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- * 'mprbffRepositoryName' - The name of the repository where the pull request was created.
mergePullRequestByFastForward
    :: Text -- ^ 'mprbffPullRequestId'
    -> Text -- ^ 'mprbffRepositoryName'
    -> MergePullRequestByFastForward
mergePullRequestByFastForward pPullRequestId_ pRepositoryName_ =
  MergePullRequestByFastForward'
    { _mprbffSourceCommitId = Nothing
    , _mprbffPullRequestId = pPullRequestId_
    , _mprbffRepositoryName = pRepositoryName_
    }


-- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
mprbffSourceCommitId :: Lens' MergePullRequestByFastForward (Maybe Text)
mprbffSourceCommitId = lens _mprbffSourceCommitId (\ s a -> s{_mprbffSourceCommitId = a})

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
mprbffPullRequestId :: Lens' MergePullRequestByFastForward Text
mprbffPullRequestId = lens _mprbffPullRequestId (\ s a -> s{_mprbffPullRequestId = a})

-- | The name of the repository where the pull request was created.
mprbffRepositoryName :: Lens' MergePullRequestByFastForward Text
mprbffRepositoryName = lens _mprbffRepositoryName (\ s a -> s{_mprbffRepositoryName = a})

instance AWSRequest MergePullRequestByFastForward
         where
        type Rs MergePullRequestByFastForward =
             MergePullRequestByFastForwardResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 MergePullRequestByFastForwardResponse' <$>
                   (x .?> "pullRequest") <*> (pure (fromEnum s)))

instance Hashable MergePullRequestByFastForward where

instance NFData MergePullRequestByFastForward where

instance ToHeaders MergePullRequestByFastForward
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.MergePullRequestByFastForward"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON MergePullRequestByFastForward where
        toJSON MergePullRequestByFastForward'{..}
          = object
              (catMaybes
                 [("sourceCommitId" .=) <$> _mprbffSourceCommitId,
                  Just ("pullRequestId" .= _mprbffPullRequestId),
                  Just ("repositoryName" .= _mprbffRepositoryName)])

instance ToPath MergePullRequestByFastForward where
        toPath = const "/"

instance ToQuery MergePullRequestByFastForward where
        toQuery = const mempty

-- | /See:/ 'mergePullRequestByFastForwardResponse' smart constructor.
data MergePullRequestByFastForwardResponse = MergePullRequestByFastForwardResponse'
  { _mprbffrsPullRequest    :: !(Maybe PullRequest)
  , _mprbffrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MergePullRequestByFastForwardResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mprbffrsPullRequest' - Information about the specified pull request, including information about the merge.
--
-- * 'mprbffrsResponseStatus' - -- | The response status code.
mergePullRequestByFastForwardResponse
    :: Int -- ^ 'mprbffrsResponseStatus'
    -> MergePullRequestByFastForwardResponse
mergePullRequestByFastForwardResponse pResponseStatus_ =
  MergePullRequestByFastForwardResponse'
    {_mprbffrsPullRequest = Nothing, _mprbffrsResponseStatus = pResponseStatus_}


-- | Information about the specified pull request, including information about the merge.
mprbffrsPullRequest :: Lens' MergePullRequestByFastForwardResponse (Maybe PullRequest)
mprbffrsPullRequest = lens _mprbffrsPullRequest (\ s a -> s{_mprbffrsPullRequest = a})

-- | -- | The response status code.
mprbffrsResponseStatus :: Lens' MergePullRequestByFastForwardResponse Int
mprbffrsResponseStatus = lens _mprbffrsResponseStatus (\ s a -> s{_mprbffrsResponseStatus = a})

instance NFData MergePullRequestByFastForwardResponse
         where
