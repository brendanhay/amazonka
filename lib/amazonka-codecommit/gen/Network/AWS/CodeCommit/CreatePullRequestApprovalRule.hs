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
-- Module      : Network.AWS.CodeCommit.CreatePullRequestApprovalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an approval rule for a pull request.
module Network.AWS.CodeCommit.CreatePullRequestApprovalRule
  ( -- * Creating a Request
    createPullRequestApprovalRule,
    CreatePullRequestApprovalRule,

    -- * Request Lenses
    cprarPullRequestId,
    cprarApprovalRuleName,
    cprarApprovalRuleContent,

    -- * Destructuring the Response
    createPullRequestApprovalRuleResponse,
    CreatePullRequestApprovalRuleResponse,

    -- * Response Lenses
    cprarrsResponseStatus,
    cprarrsApprovalRule,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPullRequestApprovalRule' smart constructor.
data CreatePullRequestApprovalRule = CreatePullRequestApprovalRule'
  { _cprarPullRequestId ::
      !Text,
    _cprarApprovalRuleName :: !Text,
    _cprarApprovalRuleContent ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePullRequestApprovalRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprarPullRequestId' - The system-generated ID of the pull request for which you want to create the approval rule.
--
-- * 'cprarApprovalRuleName' - The name for the approval rule.
--
-- * 'cprarApprovalRuleContent' - The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
createPullRequestApprovalRule ::
  -- | 'cprarPullRequestId'
  Text ->
  -- | 'cprarApprovalRuleName'
  Text ->
  -- | 'cprarApprovalRuleContent'
  Text ->
  CreatePullRequestApprovalRule
createPullRequestApprovalRule
  pPullRequestId_
  pApprovalRuleName_
  pApprovalRuleContent_ =
    CreatePullRequestApprovalRule'
      { _cprarPullRequestId =
          pPullRequestId_,
        _cprarApprovalRuleName = pApprovalRuleName_,
        _cprarApprovalRuleContent = pApprovalRuleContent_
      }

-- | The system-generated ID of the pull request for which you want to create the approval rule.
cprarPullRequestId :: Lens' CreatePullRequestApprovalRule Text
cprarPullRequestId = lens _cprarPullRequestId (\s a -> s {_cprarPullRequestId = a})

-- | The name for the approval rule.
cprarApprovalRuleName :: Lens' CreatePullRequestApprovalRule Text
cprarApprovalRuleName = lens _cprarApprovalRuleName (\s a -> s {_cprarApprovalRuleName = a})

-- | The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
cprarApprovalRuleContent :: Lens' CreatePullRequestApprovalRule Text
cprarApprovalRuleContent = lens _cprarApprovalRuleContent (\s a -> s {_cprarApprovalRuleContent = a})

instance AWSRequest CreatePullRequestApprovalRule where
  type
    Rs CreatePullRequestApprovalRule =
      CreatePullRequestApprovalRuleResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          CreatePullRequestApprovalRuleResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "approvalRule")
      )

instance Hashable CreatePullRequestApprovalRule

instance NFData CreatePullRequestApprovalRule

instance ToHeaders CreatePullRequestApprovalRule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.CreatePullRequestApprovalRule" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreatePullRequestApprovalRule where
  toJSON CreatePullRequestApprovalRule' {..} =
    object
      ( catMaybes
          [ Just ("pullRequestId" .= _cprarPullRequestId),
            Just ("approvalRuleName" .= _cprarApprovalRuleName),
            Just ("approvalRuleContent" .= _cprarApprovalRuleContent)
          ]
      )

instance ToPath CreatePullRequestApprovalRule where
  toPath = const "/"

instance ToQuery CreatePullRequestApprovalRule where
  toQuery = const mempty

-- | /See:/ 'createPullRequestApprovalRuleResponse' smart constructor.
data CreatePullRequestApprovalRuleResponse = CreatePullRequestApprovalRuleResponse'
  { _cprarrsResponseStatus ::
      !Int,
    _cprarrsApprovalRule ::
      !ApprovalRule
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePullRequestApprovalRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprarrsResponseStatus' - -- | The response status code.
--
-- * 'cprarrsApprovalRule' - Information about the created approval rule.
createPullRequestApprovalRuleResponse ::
  -- | 'cprarrsResponseStatus'
  Int ->
  -- | 'cprarrsApprovalRule'
  ApprovalRule ->
  CreatePullRequestApprovalRuleResponse
createPullRequestApprovalRuleResponse
  pResponseStatus_
  pApprovalRule_ =
    CreatePullRequestApprovalRuleResponse'
      { _cprarrsResponseStatus =
          pResponseStatus_,
        _cprarrsApprovalRule = pApprovalRule_
      }

-- | -- | The response status code.
cprarrsResponseStatus :: Lens' CreatePullRequestApprovalRuleResponse Int
cprarrsResponseStatus = lens _cprarrsResponseStatus (\s a -> s {_cprarrsResponseStatus = a})

-- | Information about the created approval rule.
cprarrsApprovalRule :: Lens' CreatePullRequestApprovalRuleResponse ApprovalRule
cprarrsApprovalRule = lens _cprarrsApprovalRule (\s a -> s {_cprarrsApprovalRule = a})

instance NFData CreatePullRequestApprovalRuleResponse
