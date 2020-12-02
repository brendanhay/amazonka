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
-- Module      : Network.AWS.CodeCommit.OverridePullRequestApprovalRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets aside (overrides) all approval rule requirements for a specified pull request.
module Network.AWS.CodeCommit.OverridePullRequestApprovalRules
  ( -- * Creating a Request
    overridePullRequestApprovalRules,
    OverridePullRequestApprovalRules,

    -- * Request Lenses
    oprarPullRequestId,
    oprarRevisionId,
    oprarOverrideStatus,

    -- * Destructuring the Response
    overridePullRequestApprovalRulesResponse,
    OverridePullRequestApprovalRulesResponse,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'overridePullRequestApprovalRules' smart constructor.
data OverridePullRequestApprovalRules = OverridePullRequestApprovalRules'
  { _oprarPullRequestId ::
      !Text,
    _oprarRevisionId :: !Text,
    _oprarOverrideStatus ::
      !OverrideStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OverridePullRequestApprovalRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oprarPullRequestId' - The system-generated ID of the pull request for which you want to override all approval rule requirements. To get this information, use 'GetPullRequest' .
--
-- * 'oprarRevisionId' - The system-generated ID of the most recent revision of the pull request. You cannot override approval rules for anything but the most recent revision of a pull request. To get the revision ID, use GetPullRequest.
--
-- * 'oprarOverrideStatus' - Whether you want to set aside approval rule requirements for the pull request (OVERRIDE) or revoke a previous override and apply approval rule requirements (REVOKE). REVOKE status is not stored.
overridePullRequestApprovalRules ::
  -- | 'oprarPullRequestId'
  Text ->
  -- | 'oprarRevisionId'
  Text ->
  -- | 'oprarOverrideStatus'
  OverrideStatus ->
  OverridePullRequestApprovalRules
overridePullRequestApprovalRules
  pPullRequestId_
  pRevisionId_
  pOverrideStatus_ =
    OverridePullRequestApprovalRules'
      { _oprarPullRequestId =
          pPullRequestId_,
        _oprarRevisionId = pRevisionId_,
        _oprarOverrideStatus = pOverrideStatus_
      }

-- | The system-generated ID of the pull request for which you want to override all approval rule requirements. To get this information, use 'GetPullRequest' .
oprarPullRequestId :: Lens' OverridePullRequestApprovalRules Text
oprarPullRequestId = lens _oprarPullRequestId (\s a -> s {_oprarPullRequestId = a})

-- | The system-generated ID of the most recent revision of the pull request. You cannot override approval rules for anything but the most recent revision of a pull request. To get the revision ID, use GetPullRequest.
oprarRevisionId :: Lens' OverridePullRequestApprovalRules Text
oprarRevisionId = lens _oprarRevisionId (\s a -> s {_oprarRevisionId = a})

-- | Whether you want to set aside approval rule requirements for the pull request (OVERRIDE) or revoke a previous override and apply approval rule requirements (REVOKE). REVOKE status is not stored.
oprarOverrideStatus :: Lens' OverridePullRequestApprovalRules OverrideStatus
oprarOverrideStatus = lens _oprarOverrideStatus (\s a -> s {_oprarOverrideStatus = a})

instance AWSRequest OverridePullRequestApprovalRules where
  type
    Rs OverridePullRequestApprovalRules =
      OverridePullRequestApprovalRulesResponse
  request = postJSON codeCommit
  response = receiveNull OverridePullRequestApprovalRulesResponse'

instance Hashable OverridePullRequestApprovalRules

instance NFData OverridePullRequestApprovalRules

instance ToHeaders OverridePullRequestApprovalRules where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.OverridePullRequestApprovalRules" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON OverridePullRequestApprovalRules where
  toJSON OverridePullRequestApprovalRules' {..} =
    object
      ( catMaybes
          [ Just ("pullRequestId" .= _oprarPullRequestId),
            Just ("revisionId" .= _oprarRevisionId),
            Just ("overrideStatus" .= _oprarOverrideStatus)
          ]
      )

instance ToPath OverridePullRequestApprovalRules where
  toPath = const "/"

instance ToQuery OverridePullRequestApprovalRules where
  toQuery = const mempty

-- | /See:/ 'overridePullRequestApprovalRulesResponse' smart constructor.
data OverridePullRequestApprovalRulesResponse = OverridePullRequestApprovalRulesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OverridePullRequestApprovalRulesResponse' with the minimum fields required to make a request.
overridePullRequestApprovalRulesResponse ::
  OverridePullRequestApprovalRulesResponse
overridePullRequestApprovalRulesResponse =
  OverridePullRequestApprovalRulesResponse'

instance NFData OverridePullRequestApprovalRulesResponse
