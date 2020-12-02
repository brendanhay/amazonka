{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetPullRequestOverrideState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about whether approval rules have been set aside (overridden) for a pull request, and if so, the Amazon Resource Name (ARN) of the user or identity that overrode the rules and their requirements for the pull request.
module Network.AWS.CodeCommit.GetPullRequestOverrideState
  ( -- * Creating a Request
    getPullRequestOverrideState,
    GetPullRequestOverrideState,

    -- * Request Lenses
    gprosPullRequestId,
    gprosRevisionId,

    -- * Destructuring the Response
    getPullRequestOverrideStateResponse,
    GetPullRequestOverrideStateResponse,

    -- * Response Lenses
    gprosrsOverridden,
    gprosrsOverrider,
    gprosrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPullRequestOverrideState' smart constructor.
data GetPullRequestOverrideState = GetPullRequestOverrideState'
  { _gprosPullRequestId ::
      !Text,
    _gprosRevisionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPullRequestOverrideState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprosPullRequestId' - The ID of the pull request for which you want to get information about whether approval rules have been set aside (overridden).
--
-- * 'gprosRevisionId' - The system-generated ID of the revision for the pull request. To retrieve the most recent revision ID, use 'GetPullRequest' .
getPullRequestOverrideState ::
  -- | 'gprosPullRequestId'
  Text ->
  -- | 'gprosRevisionId'
  Text ->
  GetPullRequestOverrideState
getPullRequestOverrideState pPullRequestId_ pRevisionId_ =
  GetPullRequestOverrideState'
    { _gprosPullRequestId =
        pPullRequestId_,
      _gprosRevisionId = pRevisionId_
    }

-- | The ID of the pull request for which you want to get information about whether approval rules have been set aside (overridden).
gprosPullRequestId :: Lens' GetPullRequestOverrideState Text
gprosPullRequestId = lens _gprosPullRequestId (\s a -> s {_gprosPullRequestId = a})

-- | The system-generated ID of the revision for the pull request. To retrieve the most recent revision ID, use 'GetPullRequest' .
gprosRevisionId :: Lens' GetPullRequestOverrideState Text
gprosRevisionId = lens _gprosRevisionId (\s a -> s {_gprosRevisionId = a})

instance AWSRequest GetPullRequestOverrideState where
  type
    Rs GetPullRequestOverrideState =
      GetPullRequestOverrideStateResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          GetPullRequestOverrideStateResponse'
            <$> (x .?> "overridden")
            <*> (x .?> "overrider")
            <*> (pure (fromEnum s))
      )

instance Hashable GetPullRequestOverrideState

instance NFData GetPullRequestOverrideState

instance ToHeaders GetPullRequestOverrideState where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.GetPullRequestOverrideState" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetPullRequestOverrideState where
  toJSON GetPullRequestOverrideState' {..} =
    object
      ( catMaybes
          [ Just ("pullRequestId" .= _gprosPullRequestId),
            Just ("revisionId" .= _gprosRevisionId)
          ]
      )

instance ToPath GetPullRequestOverrideState where
  toPath = const "/"

instance ToQuery GetPullRequestOverrideState where
  toQuery = const mempty

-- | /See:/ 'getPullRequestOverrideStateResponse' smart constructor.
data GetPullRequestOverrideStateResponse = GetPullRequestOverrideStateResponse'
  { _gprosrsOverridden ::
      !(Maybe Bool),
    _gprosrsOverrider ::
      !(Maybe Text),
    _gprosrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPullRequestOverrideStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprosrsOverridden' - A Boolean value that indicates whether a pull request has had its rules set aside (TRUE) or whether all approval rules still apply (FALSE).
--
-- * 'gprosrsOverrider' - The Amazon Resource Name (ARN) of the user or identity that overrode the rules and their requirements for the pull request.
--
-- * 'gprosrsResponseStatus' - -- | The response status code.
getPullRequestOverrideStateResponse ::
  -- | 'gprosrsResponseStatus'
  Int ->
  GetPullRequestOverrideStateResponse
getPullRequestOverrideStateResponse pResponseStatus_ =
  GetPullRequestOverrideStateResponse'
    { _gprosrsOverridden =
        Nothing,
      _gprosrsOverrider = Nothing,
      _gprosrsResponseStatus = pResponseStatus_
    }

-- | A Boolean value that indicates whether a pull request has had its rules set aside (TRUE) or whether all approval rules still apply (FALSE).
gprosrsOverridden :: Lens' GetPullRequestOverrideStateResponse (Maybe Bool)
gprosrsOverridden = lens _gprosrsOverridden (\s a -> s {_gprosrsOverridden = a})

-- | The Amazon Resource Name (ARN) of the user or identity that overrode the rules and their requirements for the pull request.
gprosrsOverrider :: Lens' GetPullRequestOverrideStateResponse (Maybe Text)
gprosrsOverrider = lens _gprosrsOverrider (\s a -> s {_gprosrsOverrider = a})

-- | -- | The response status code.
gprosrsResponseStatus :: Lens' GetPullRequestOverrideStateResponse Int
gprosrsResponseStatus = lens _gprosrsResponseStatus (\s a -> s {_gprosrsResponseStatus = a})

instance NFData GetPullRequestOverrideStateResponse
