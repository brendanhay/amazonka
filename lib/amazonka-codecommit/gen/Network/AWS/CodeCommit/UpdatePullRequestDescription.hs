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
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestDescription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of the description of a pull request.
--
--
module Network.AWS.CodeCommit.UpdatePullRequestDescription
    (
    -- * Creating a Request
      updatePullRequestDescription
    , UpdatePullRequestDescription
    -- * Request Lenses
    , uprdPullRequestId
    , uprdDescription

    -- * Destructuring the Response
    , updatePullRequestDescriptionResponse
    , UpdatePullRequestDescriptionResponse
    -- * Response Lenses
    , uprdrsResponseStatus
    , uprdrsPullRequest
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePullRequestDescription' smart constructor.
data UpdatePullRequestDescription = UpdatePullRequestDescription'
  { _uprdPullRequestId :: !Text
  , _uprdDescription   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePullRequestDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprdPullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- * 'uprdDescription' - The updated content of the description for the pull request. This content will replace the existing description.
updatePullRequestDescription
    :: Text -- ^ 'uprdPullRequestId'
    -> Text -- ^ 'uprdDescription'
    -> UpdatePullRequestDescription
updatePullRequestDescription pPullRequestId_ pDescription_ =
  UpdatePullRequestDescription'
    {_uprdPullRequestId = pPullRequestId_, _uprdDescription = pDescription_}


-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
uprdPullRequestId :: Lens' UpdatePullRequestDescription Text
uprdPullRequestId = lens _uprdPullRequestId (\ s a -> s{_uprdPullRequestId = a})

-- | The updated content of the description for the pull request. This content will replace the existing description.
uprdDescription :: Lens' UpdatePullRequestDescription Text
uprdDescription = lens _uprdDescription (\ s a -> s{_uprdDescription = a})

instance AWSRequest UpdatePullRequestDescription
         where
        type Rs UpdatePullRequestDescription =
             UpdatePullRequestDescriptionResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePullRequestDescriptionResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "pullRequest"))

instance Hashable UpdatePullRequestDescription where

instance NFData UpdatePullRequestDescription where

instance ToHeaders UpdatePullRequestDescription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.UpdatePullRequestDescription"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdatePullRequestDescription where
        toJSON UpdatePullRequestDescription'{..}
          = object
              (catMaybes
                 [Just ("pullRequestId" .= _uprdPullRequestId),
                  Just ("description" .= _uprdDescription)])

instance ToPath UpdatePullRequestDescription where
        toPath = const "/"

instance ToQuery UpdatePullRequestDescription where
        toQuery = const mempty

-- | /See:/ 'updatePullRequestDescriptionResponse' smart constructor.
data UpdatePullRequestDescriptionResponse = UpdatePullRequestDescriptionResponse'
  { _uprdrsResponseStatus :: !Int
  , _uprdrsPullRequest    :: !PullRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePullRequestDescriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprdrsResponseStatus' - -- | The response status code.
--
-- * 'uprdrsPullRequest' - Information about the updated pull request.
updatePullRequestDescriptionResponse
    :: Int -- ^ 'uprdrsResponseStatus'
    -> PullRequest -- ^ 'uprdrsPullRequest'
    -> UpdatePullRequestDescriptionResponse
updatePullRequestDescriptionResponse pResponseStatus_ pPullRequest_ =
  UpdatePullRequestDescriptionResponse'
    { _uprdrsResponseStatus = pResponseStatus_
    , _uprdrsPullRequest = pPullRequest_
    }


-- | -- | The response status code.
uprdrsResponseStatus :: Lens' UpdatePullRequestDescriptionResponse Int
uprdrsResponseStatus = lens _uprdrsResponseStatus (\ s a -> s{_uprdrsResponseStatus = a})

-- | Information about the updated pull request.
uprdrsPullRequest :: Lens' UpdatePullRequestDescriptionResponse PullRequest
uprdrsPullRequest = lens _uprdrsPullRequest (\ s a -> s{_uprdrsPullRequest = a})

instance NFData UpdatePullRequestDescriptionResponse
         where
