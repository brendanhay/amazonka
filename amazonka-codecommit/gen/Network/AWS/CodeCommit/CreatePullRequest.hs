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
-- Module      : Network.AWS.CodeCommit.CreatePullRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pull request in the specified repository.
--
--
module Network.AWS.CodeCommit.CreatePullRequest
    (
    -- * Creating a Request
      createPullRequest
    , CreatePullRequest
    -- * Request Lenses
    , cprClientRequestToken
    , cprDescription
    , cprTitle
    , cprTargets

    -- * Destructuring the Response
    , createPullRequestResponse
    , CreatePullRequestResponse
    -- * Response Lenses
    , cprrsResponseStatus
    , cprrsPullRequest
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPullRequest' smart constructor.
data CreatePullRequest = CreatePullRequest'
  { _cprClientRequestToken :: !(Maybe Text)
  , _cprDescription        :: !(Maybe Text)
  , _cprTitle              :: !Text
  , _cprTargets            :: ![Target]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePullRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprClientRequestToken' - A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
--
-- * 'cprDescription' - A description of the pull request.
--
-- * 'cprTitle' - The title of the pull request. This title will be used to identify the pull request to other users in the repository.
--
-- * 'cprTargets' - The targets for the pull request, including the source of the code to be reviewed (the source branch), and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
createPullRequest
    :: Text -- ^ 'cprTitle'
    -> CreatePullRequest
createPullRequest pTitle_ =
  CreatePullRequest'
    { _cprClientRequestToken = Nothing
    , _cprDescription = Nothing
    , _cprTitle = pTitle_
    , _cprTargets = mempty
    }


-- | A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
cprClientRequestToken :: Lens' CreatePullRequest (Maybe Text)
cprClientRequestToken = lens _cprClientRequestToken (\ s a -> s{_cprClientRequestToken = a})

-- | A description of the pull request.
cprDescription :: Lens' CreatePullRequest (Maybe Text)
cprDescription = lens _cprDescription (\ s a -> s{_cprDescription = a})

-- | The title of the pull request. This title will be used to identify the pull request to other users in the repository.
cprTitle :: Lens' CreatePullRequest Text
cprTitle = lens _cprTitle (\ s a -> s{_cprTitle = a})

-- | The targets for the pull request, including the source of the code to be reviewed (the source branch), and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
cprTargets :: Lens' CreatePullRequest [Target]
cprTargets = lens _cprTargets (\ s a -> s{_cprTargets = a}) . _Coerce

instance AWSRequest CreatePullRequest where
        type Rs CreatePullRequest = CreatePullRequestResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 CreatePullRequestResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "pullRequest"))

instance Hashable CreatePullRequest where

instance NFData CreatePullRequest where

instance ToHeaders CreatePullRequest where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.CreatePullRequest" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePullRequest where
        toJSON CreatePullRequest'{..}
          = object
              (catMaybes
                 [("clientRequestToken" .=) <$>
                    _cprClientRequestToken,
                  ("description" .=) <$> _cprDescription,
                  Just ("title" .= _cprTitle),
                  Just ("targets" .= _cprTargets)])

instance ToPath CreatePullRequest where
        toPath = const "/"

instance ToQuery CreatePullRequest where
        toQuery = const mempty

-- | /See:/ 'createPullRequestResponse' smart constructor.
data CreatePullRequestResponse = CreatePullRequestResponse'
  { _cprrsResponseStatus :: !Int
  , _cprrsPullRequest    :: !PullRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePullRequestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprrsResponseStatus' - -- | The response status code.
--
-- * 'cprrsPullRequest' - Information about the newly created pull request.
createPullRequestResponse
    :: Int -- ^ 'cprrsResponseStatus'
    -> PullRequest -- ^ 'cprrsPullRequest'
    -> CreatePullRequestResponse
createPullRequestResponse pResponseStatus_ pPullRequest_ =
  CreatePullRequestResponse'
    {_cprrsResponseStatus = pResponseStatus_, _cprrsPullRequest = pPullRequest_}


-- | -- | The response status code.
cprrsResponseStatus :: Lens' CreatePullRequestResponse Int
cprrsResponseStatus = lens _cprrsResponseStatus (\ s a -> s{_cprrsResponseStatus = a})

-- | Information about the newly created pull request.
cprrsPullRequest :: Lens' CreatePullRequestResponse PullRequest
cprrsPullRequest = lens _cprrsPullRequest (\ s a -> s{_cprrsPullRequest = a})

instance NFData CreatePullRequestResponse where
