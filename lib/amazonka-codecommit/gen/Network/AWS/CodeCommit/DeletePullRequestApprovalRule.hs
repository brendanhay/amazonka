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
-- Module      : Network.AWS.CodeCommit.DeletePullRequestApprovalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an approval rule from a specified pull request. Approval rules can be deleted from a pull request only if the pull request is open, and if the approval rule was created specifically for a pull request and not generated from an approval rule template associated with the repository where the pull request was created. You cannot delete an approval rule from a merged or closed pull request.
module Network.AWS.CodeCommit.DeletePullRequestApprovalRule
  ( -- * Creating a Request
    deletePullRequestApprovalRule,
    DeletePullRequestApprovalRule,

    -- * Request Lenses
    dprarPullRequestId,
    dprarApprovalRuleName,

    -- * Destructuring the Response
    deletePullRequestApprovalRuleResponse,
    DeletePullRequestApprovalRuleResponse,

    -- * Response Lenses
    dprarrsResponseStatus,
    dprarrsApprovalRuleId,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePullRequestApprovalRule' smart constructor.
data DeletePullRequestApprovalRule = DeletePullRequestApprovalRule'
  { _dprarPullRequestId ::
      !Text,
    _dprarApprovalRuleName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePullRequestApprovalRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprarPullRequestId' - The system-generated ID of the pull request that contains the approval rule you want to delete.
--
-- * 'dprarApprovalRuleName' - The name of the approval rule you want to delete.
deletePullRequestApprovalRule ::
  -- | 'dprarPullRequestId'
  Text ->
  -- | 'dprarApprovalRuleName'
  Text ->
  DeletePullRequestApprovalRule
deletePullRequestApprovalRule pPullRequestId_ pApprovalRuleName_ =
  DeletePullRequestApprovalRule'
    { _dprarPullRequestId =
        pPullRequestId_,
      _dprarApprovalRuleName = pApprovalRuleName_
    }

-- | The system-generated ID of the pull request that contains the approval rule you want to delete.
dprarPullRequestId :: Lens' DeletePullRequestApprovalRule Text
dprarPullRequestId = lens _dprarPullRequestId (\s a -> s {_dprarPullRequestId = a})

-- | The name of the approval rule you want to delete.
dprarApprovalRuleName :: Lens' DeletePullRequestApprovalRule Text
dprarApprovalRuleName = lens _dprarApprovalRuleName (\s a -> s {_dprarApprovalRuleName = a})

instance AWSRequest DeletePullRequestApprovalRule where
  type
    Rs DeletePullRequestApprovalRule =
      DeletePullRequestApprovalRuleResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          DeletePullRequestApprovalRuleResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "approvalRuleId")
      )

instance Hashable DeletePullRequestApprovalRule

instance NFData DeletePullRequestApprovalRule

instance ToHeaders DeletePullRequestApprovalRule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.DeletePullRequestApprovalRule" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeletePullRequestApprovalRule where
  toJSON DeletePullRequestApprovalRule' {..} =
    object
      ( catMaybes
          [ Just ("pullRequestId" .= _dprarPullRequestId),
            Just ("approvalRuleName" .= _dprarApprovalRuleName)
          ]
      )

instance ToPath DeletePullRequestApprovalRule where
  toPath = const "/"

instance ToQuery DeletePullRequestApprovalRule where
  toQuery = const mempty

-- | /See:/ 'deletePullRequestApprovalRuleResponse' smart constructor.
data DeletePullRequestApprovalRuleResponse = DeletePullRequestApprovalRuleResponse'
  { _dprarrsResponseStatus ::
      !Int,
    _dprarrsApprovalRuleId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePullRequestApprovalRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprarrsResponseStatus' - -- | The response status code.
--
-- * 'dprarrsApprovalRuleId' - The ID of the deleted approval rule.
deletePullRequestApprovalRuleResponse ::
  -- | 'dprarrsResponseStatus'
  Int ->
  -- | 'dprarrsApprovalRuleId'
  Text ->
  DeletePullRequestApprovalRuleResponse
deletePullRequestApprovalRuleResponse
  pResponseStatus_
  pApprovalRuleId_ =
    DeletePullRequestApprovalRuleResponse'
      { _dprarrsResponseStatus =
          pResponseStatus_,
        _dprarrsApprovalRuleId = pApprovalRuleId_
      }

-- | -- | The response status code.
dprarrsResponseStatus :: Lens' DeletePullRequestApprovalRuleResponse Int
dprarrsResponseStatus = lens _dprarrsResponseStatus (\s a -> s {_dprarrsResponseStatus = a})

-- | The ID of the deleted approval rule.
dprarrsApprovalRuleId :: Lens' DeletePullRequestApprovalRuleResponse Text
dprarrsApprovalRuleId = lens _dprarrsApprovalRuleId (\s a -> s {_dprarrsApprovalRuleId = a})

instance NFData DeletePullRequestApprovalRuleResponse
