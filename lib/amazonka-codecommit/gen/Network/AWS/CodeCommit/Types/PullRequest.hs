{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequest where

import Network.AWS.CodeCommit.Types.ApprovalRule
import Network.AWS.CodeCommit.Types.PullRequestStatusEnum
import Network.AWS.CodeCommit.Types.PullRequestTarget
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a pull request.
--
--
--
-- /See:/ 'pullRequest' smart constructor.
data PullRequest = PullRequest'
  { _prApprovalRules ::
      !(Maybe [ApprovalRule]),
    _prAuthorARN :: !(Maybe Text),
    _prPullRequestId :: !(Maybe Text),
    _prCreationDate :: !(Maybe POSIX),
    _prPullRequestStatus :: !(Maybe PullRequestStatusEnum),
    _prTitle :: !(Maybe Text),
    _prClientRequestToken :: !(Maybe Text),
    _prLastActivityDate :: !(Maybe POSIX),
    _prRevisionId :: !(Maybe Text),
    _prPullRequestTargets :: !(Maybe [PullRequestTarget]),
    _prDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PullRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prApprovalRules' - The approval rules applied to the pull request.
--
-- * 'prAuthorARN' - The Amazon Resource Name (ARN) of the user who created the pull request.
--
-- * 'prPullRequestId' - The system-generated ID of the pull request.
--
-- * 'prCreationDate' - The date and time the pull request was originally created, in timestamp format.
--
-- * 'prPullRequestStatus' - The status of the pull request. Pull request status can only change from @OPEN@ to @CLOSED@ .
--
-- * 'prTitle' - The user-defined title of the pull request. This title is displayed in the list of pull requests to other repository users.
--
-- * 'prClientRequestToken' - A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- * 'prLastActivityDate' - The day and time of the last user or system activity on the pull request, in timestamp format.
--
-- * 'prRevisionId' - The system-generated revision ID for the pull request.
--
-- * 'prPullRequestTargets' - The targets of the pull request, including the source branch and destination branch for the pull request.
--
-- * 'prDescription' - The user-defined description of the pull request. This description can be used to clarify what should be reviewed and other details of the request.
pullRequest ::
  PullRequest
pullRequest =
  PullRequest'
    { _prApprovalRules = Nothing,
      _prAuthorARN = Nothing,
      _prPullRequestId = Nothing,
      _prCreationDate = Nothing,
      _prPullRequestStatus = Nothing,
      _prTitle = Nothing,
      _prClientRequestToken = Nothing,
      _prLastActivityDate = Nothing,
      _prRevisionId = Nothing,
      _prPullRequestTargets = Nothing,
      _prDescription = Nothing
    }

-- | The approval rules applied to the pull request.
prApprovalRules :: Lens' PullRequest [ApprovalRule]
prApprovalRules = lens _prApprovalRules (\s a -> s {_prApprovalRules = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the user who created the pull request.
prAuthorARN :: Lens' PullRequest (Maybe Text)
prAuthorARN = lens _prAuthorARN (\s a -> s {_prAuthorARN = a})

-- | The system-generated ID of the pull request.
prPullRequestId :: Lens' PullRequest (Maybe Text)
prPullRequestId = lens _prPullRequestId (\s a -> s {_prPullRequestId = a})

-- | The date and time the pull request was originally created, in timestamp format.
prCreationDate :: Lens' PullRequest (Maybe UTCTime)
prCreationDate = lens _prCreationDate (\s a -> s {_prCreationDate = a}) . mapping _Time

-- | The status of the pull request. Pull request status can only change from @OPEN@ to @CLOSED@ .
prPullRequestStatus :: Lens' PullRequest (Maybe PullRequestStatusEnum)
prPullRequestStatus = lens _prPullRequestStatus (\s a -> s {_prPullRequestStatus = a})

-- | The user-defined title of the pull request. This title is displayed in the list of pull requests to other repository users.
prTitle :: Lens' PullRequest (Maybe Text)
prTitle = lens _prTitle (\s a -> s {_prTitle = a})

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
prClientRequestToken :: Lens' PullRequest (Maybe Text)
prClientRequestToken = lens _prClientRequestToken (\s a -> s {_prClientRequestToken = a})

-- | The day and time of the last user or system activity on the pull request, in timestamp format.
prLastActivityDate :: Lens' PullRequest (Maybe UTCTime)
prLastActivityDate = lens _prLastActivityDate (\s a -> s {_prLastActivityDate = a}) . mapping _Time

-- | The system-generated revision ID for the pull request.
prRevisionId :: Lens' PullRequest (Maybe Text)
prRevisionId = lens _prRevisionId (\s a -> s {_prRevisionId = a})

-- | The targets of the pull request, including the source branch and destination branch for the pull request.
prPullRequestTargets :: Lens' PullRequest [PullRequestTarget]
prPullRequestTargets = lens _prPullRequestTargets (\s a -> s {_prPullRequestTargets = a}) . _Default . _Coerce

-- | The user-defined description of the pull request. This description can be used to clarify what should be reviewed and other details of the request.
prDescription :: Lens' PullRequest (Maybe Text)
prDescription = lens _prDescription (\s a -> s {_prDescription = a})

instance FromJSON PullRequest where
  parseJSON =
    withObject
      "PullRequest"
      ( \x ->
          PullRequest'
            <$> (x .:? "approvalRules" .!= mempty)
            <*> (x .:? "authorArn")
            <*> (x .:? "pullRequestId")
            <*> (x .:? "creationDate")
            <*> (x .:? "pullRequestStatus")
            <*> (x .:? "title")
            <*> (x .:? "clientRequestToken")
            <*> (x .:? "lastActivityDate")
            <*> (x .:? "revisionId")
            <*> (x .:? "pullRequestTargets" .!= mempty)
            <*> (x .:? "description")
      )

instance Hashable PullRequest

instance NFData PullRequest
