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
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the structure of an approval rule created specifically for a pull request. For example, you can change the number of required approvers and the approval pool for approvers.
module Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent
  ( -- * Creating a Request
    updatePullRequestApprovalRuleContent,
    UpdatePullRequestApprovalRuleContent,

    -- * Request Lenses
    uprarcExistingRuleContentSha256,
    uprarcPullRequestId,
    uprarcApprovalRuleName,
    uprarcNewRuleContent,

    -- * Destructuring the Response
    updatePullRequestApprovalRuleContentResponse,
    UpdatePullRequestApprovalRuleContentResponse,

    -- * Response Lenses
    uprarcrsResponseStatus,
    uprarcrsApprovalRule,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePullRequestApprovalRuleContent' smart constructor.
data UpdatePullRequestApprovalRuleContent = UpdatePullRequestApprovalRuleContent'
  { _uprarcExistingRuleContentSha256 ::
      !(Maybe Text),
    _uprarcPullRequestId ::
      !Text,
    _uprarcApprovalRuleName ::
      !Text,
    _uprarcNewRuleContent ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePullRequestApprovalRuleContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprarcExistingRuleContentSha256' - The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
--
-- * 'uprarcPullRequestId' - The system-generated ID of the pull request.
--
-- * 'uprarcApprovalRuleName' - The name of the approval rule you want to update.
--
-- * 'uprarcNewRuleContent' - The updated content for the approval rule.
updatePullRequestApprovalRuleContent ::
  -- | 'uprarcPullRequestId'
  Text ->
  -- | 'uprarcApprovalRuleName'
  Text ->
  -- | 'uprarcNewRuleContent'
  Text ->
  UpdatePullRequestApprovalRuleContent
updatePullRequestApprovalRuleContent
  pPullRequestId_
  pApprovalRuleName_
  pNewRuleContent_ =
    UpdatePullRequestApprovalRuleContent'
      { _uprarcExistingRuleContentSha256 =
          Nothing,
        _uprarcPullRequestId = pPullRequestId_,
        _uprarcApprovalRuleName = pApprovalRuleName_,
        _uprarcNewRuleContent = pNewRuleContent_
      }

-- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
uprarcExistingRuleContentSha256 :: Lens' UpdatePullRequestApprovalRuleContent (Maybe Text)
uprarcExistingRuleContentSha256 = lens _uprarcExistingRuleContentSha256 (\s a -> s {_uprarcExistingRuleContentSha256 = a})

-- | The system-generated ID of the pull request.
uprarcPullRequestId :: Lens' UpdatePullRequestApprovalRuleContent Text
uprarcPullRequestId = lens _uprarcPullRequestId (\s a -> s {_uprarcPullRequestId = a})

-- | The name of the approval rule you want to update.
uprarcApprovalRuleName :: Lens' UpdatePullRequestApprovalRuleContent Text
uprarcApprovalRuleName = lens _uprarcApprovalRuleName (\s a -> s {_uprarcApprovalRuleName = a})

-- | The updated content for the approval rule.
uprarcNewRuleContent :: Lens' UpdatePullRequestApprovalRuleContent Text
uprarcNewRuleContent = lens _uprarcNewRuleContent (\s a -> s {_uprarcNewRuleContent = a})

instance AWSRequest UpdatePullRequestApprovalRuleContent where
  type
    Rs UpdatePullRequestApprovalRuleContent =
      UpdatePullRequestApprovalRuleContentResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          UpdatePullRequestApprovalRuleContentResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "approvalRule")
      )

instance Hashable UpdatePullRequestApprovalRuleContent

instance NFData UpdatePullRequestApprovalRuleContent

instance ToHeaders UpdatePullRequestApprovalRuleContent where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.UpdatePullRequestApprovalRuleContent" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdatePullRequestApprovalRuleContent where
  toJSON UpdatePullRequestApprovalRuleContent' {..} =
    object
      ( catMaybes
          [ ("existingRuleContentSha256" .=)
              <$> _uprarcExistingRuleContentSha256,
            Just ("pullRequestId" .= _uprarcPullRequestId),
            Just ("approvalRuleName" .= _uprarcApprovalRuleName),
            Just ("newRuleContent" .= _uprarcNewRuleContent)
          ]
      )

instance ToPath UpdatePullRequestApprovalRuleContent where
  toPath = const "/"

instance ToQuery UpdatePullRequestApprovalRuleContent where
  toQuery = const mempty

-- | /See:/ 'updatePullRequestApprovalRuleContentResponse' smart constructor.
data UpdatePullRequestApprovalRuleContentResponse = UpdatePullRequestApprovalRuleContentResponse'
  { _uprarcrsResponseStatus ::
      !Int,
    _uprarcrsApprovalRule ::
      !ApprovalRule
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdatePullRequestApprovalRuleContentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprarcrsResponseStatus' - -- | The response status code.
--
-- * 'uprarcrsApprovalRule' - Information about the updated approval rule.
updatePullRequestApprovalRuleContentResponse ::
  -- | 'uprarcrsResponseStatus'
  Int ->
  -- | 'uprarcrsApprovalRule'
  ApprovalRule ->
  UpdatePullRequestApprovalRuleContentResponse
updatePullRequestApprovalRuleContentResponse
  pResponseStatus_
  pApprovalRule_ =
    UpdatePullRequestApprovalRuleContentResponse'
      { _uprarcrsResponseStatus =
          pResponseStatus_,
        _uprarcrsApprovalRule = pApprovalRule_
      }

-- | -- | The response status code.
uprarcrsResponseStatus :: Lens' UpdatePullRequestApprovalRuleContentResponse Int
uprarcrsResponseStatus = lens _uprarcrsResponseStatus (\s a -> s {_uprarcrsResponseStatus = a})

-- | Information about the updated approval rule.
uprarcrsApprovalRule :: Lens' UpdatePullRequestApprovalRuleContentResponse ApprovalRule
uprarcrsApprovalRule = lens _uprarcrsApprovalRule (\s a -> s {_uprarcrsApprovalRule = a})

instance NFData UpdatePullRequestApprovalRuleContentResponse
