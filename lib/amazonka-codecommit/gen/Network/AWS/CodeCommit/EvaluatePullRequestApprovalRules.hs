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
-- Module      : Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates whether a pull request has met all the conditions specified in its associated approval rules.
module Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
  ( -- * Creating a Request
    evaluatePullRequestApprovalRules,
    EvaluatePullRequestApprovalRules,

    -- * Request Lenses
    eprarPullRequestId,
    eprarRevisionId,

    -- * Destructuring the Response
    evaluatePullRequestApprovalRulesResponse,
    EvaluatePullRequestApprovalRulesResponse,

    -- * Response Lenses
    eprarrsResponseStatus,
    eprarrsEvaluation,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'evaluatePullRequestApprovalRules' smart constructor.
data EvaluatePullRequestApprovalRules = EvaluatePullRequestApprovalRules'
  { _eprarPullRequestId ::
      !Text,
    _eprarRevisionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluatePullRequestApprovalRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eprarPullRequestId' - The system-generated ID of the pull request you want to evaluate.
--
-- * 'eprarRevisionId' - The system-generated ID for the pull request revision. To retrieve the most recent revision ID for a pull request, use 'GetPullRequest' .
evaluatePullRequestApprovalRules ::
  -- | 'eprarPullRequestId'
  Text ->
  -- | 'eprarRevisionId'
  Text ->
  EvaluatePullRequestApprovalRules
evaluatePullRequestApprovalRules pPullRequestId_ pRevisionId_ =
  EvaluatePullRequestApprovalRules'
    { _eprarPullRequestId =
        pPullRequestId_,
      _eprarRevisionId = pRevisionId_
    }

-- | The system-generated ID of the pull request you want to evaluate.
eprarPullRequestId :: Lens' EvaluatePullRequestApprovalRules Text
eprarPullRequestId = lens _eprarPullRequestId (\s a -> s {_eprarPullRequestId = a})

-- | The system-generated ID for the pull request revision. To retrieve the most recent revision ID for a pull request, use 'GetPullRequest' .
eprarRevisionId :: Lens' EvaluatePullRequestApprovalRules Text
eprarRevisionId = lens _eprarRevisionId (\s a -> s {_eprarRevisionId = a})

instance AWSRequest EvaluatePullRequestApprovalRules where
  type
    Rs EvaluatePullRequestApprovalRules =
      EvaluatePullRequestApprovalRulesResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          EvaluatePullRequestApprovalRulesResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "evaluation")
      )

instance Hashable EvaluatePullRequestApprovalRules

instance NFData EvaluatePullRequestApprovalRules

instance ToHeaders EvaluatePullRequestApprovalRules where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.EvaluatePullRequestApprovalRules" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON EvaluatePullRequestApprovalRules where
  toJSON EvaluatePullRequestApprovalRules' {..} =
    object
      ( catMaybes
          [ Just ("pullRequestId" .= _eprarPullRequestId),
            Just ("revisionId" .= _eprarRevisionId)
          ]
      )

instance ToPath EvaluatePullRequestApprovalRules where
  toPath = const "/"

instance ToQuery EvaluatePullRequestApprovalRules where
  toQuery = const mempty

-- | /See:/ 'evaluatePullRequestApprovalRulesResponse' smart constructor.
data EvaluatePullRequestApprovalRulesResponse = EvaluatePullRequestApprovalRulesResponse'
  { _eprarrsResponseStatus ::
      !Int,
    _eprarrsEvaluation ::
      !Evaluation
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluatePullRequestApprovalRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eprarrsResponseStatus' - -- | The response status code.
--
-- * 'eprarrsEvaluation' - The result of the evaluation, including the names of the rules whose conditions have been met (if any), the names of the rules whose conditions have not been met (if any), whether the pull request is in the approved state, and whether the pull request approval rule has been set aside by an override.
evaluatePullRequestApprovalRulesResponse ::
  -- | 'eprarrsResponseStatus'
  Int ->
  -- | 'eprarrsEvaluation'
  Evaluation ->
  EvaluatePullRequestApprovalRulesResponse
evaluatePullRequestApprovalRulesResponse
  pResponseStatus_
  pEvaluation_ =
    EvaluatePullRequestApprovalRulesResponse'
      { _eprarrsResponseStatus =
          pResponseStatus_,
        _eprarrsEvaluation = pEvaluation_
      }

-- | -- | The response status code.
eprarrsResponseStatus :: Lens' EvaluatePullRequestApprovalRulesResponse Int
eprarrsResponseStatus = lens _eprarrsResponseStatus (\s a -> s {_eprarrsResponseStatus = a})

-- | The result of the evaluation, including the names of the rules whose conditions have been met (if any), the names of the rules whose conditions have not been met (if any), whether the pull request is in the approved state, and whether the pull request approval rule has been set aside by an override.
eprarrsEvaluation :: Lens' EvaluatePullRequestApprovalRulesResponse Evaluation
eprarrsEvaluation = lens _eprarrsEvaluation (\s a -> s {_eprarrsEvaluation = a})

instance NFData EvaluatePullRequestApprovalRulesResponse
