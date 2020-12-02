{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.WebhookFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.WebhookFilter where

import Network.AWS.CodeBuild.Types.WebhookFilterType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter used to determine which webhooks trigger a build.
--
--
--
-- /See:/ 'webhookFilter' smart constructor.
data WebhookFilter = WebhookFilter'
  { _wfExcludeMatchedPattern ::
      !(Maybe Bool),
    _wfType :: !WebhookFilterType,
    _wfPattern :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebhookFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wfExcludeMatchedPattern' - Used to indicate that the @pattern@ determines which webhook events do not trigger a build. If true, then a webhook event that does not match the @pattern@ triggers a build. If false, then a webhook event that matches the @pattern@ triggers a build.
--
-- * 'wfType' - The type of webhook filter. There are six webhook filter types: @EVENT@ , @ACTOR_ACCOUNT_ID@ , @HEAD_REF@ , @BASE_REF@ , @FILE_PATH@ , and @COMMIT_MESSAGE@ .      * EVENT     * A webhook event triggers a build when the provided @pattern@ matches one of five event types: @PUSH@ , @PULL_REQUEST_CREATED@ , @PULL_REQUEST_UPDATED@ , @PULL_REQUEST_REOPENED@ , and @PULL_REQUEST_MERGED@ . The @EVENT@ patterns are specified as a comma-separated string. For example, @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ filters all push, pull request created, and pull request updated events.      * ACTOR_ACCOUNT_ID     * A webhook event triggers a build when a GitHub, GitHub Enterprise, or Bitbucket account ID matches the regular expression @pattern@ .      * HEAD_REF     * A webhook event triggers a build when the head reference matches the regular expression @pattern@ . For example, @refs/heads/branch-name@ and @refs/tags/tag-name@ .  Works with GitHub and GitHub Enterprise push, GitHub and GitHub Enterprise pull request, Bitbucket push, and Bitbucket pull request events.      * BASE_REF     * A webhook event triggers a build when the base reference matches the regular expression @pattern@ . For example, @refs/heads/branch-name@ .      * FILE_PATH     * A webhook triggers a build when the path of a changed file matches the regular expression @pattern@ .      * COMMIT_MESSAGE    * A webhook triggers a build when the head commit message matches the regular expression @pattern@ .
--
-- * 'wfPattern' - For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string that specifies one or more events. For example, the webhook filter @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull request created, and pull request updated events to trigger a build.  For a @WebHookFilter@ that uses any of the other filter types, a regular expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@ for its @type@ and the pattern @^refs/heads/@ triggers a build when the head reference is a branch with a reference name @refs/heads/branch-name@ .
webhookFilter ::
  -- | 'wfType'
  WebhookFilterType ->
  -- | 'wfPattern'
  Text ->
  WebhookFilter
webhookFilter pType_ pPattern_ =
  WebhookFilter'
    { _wfExcludeMatchedPattern = Nothing,
      _wfType = pType_,
      _wfPattern = pPattern_
    }

-- | Used to indicate that the @pattern@ determines which webhook events do not trigger a build. If true, then a webhook event that does not match the @pattern@ triggers a build. If false, then a webhook event that matches the @pattern@ triggers a build.
wfExcludeMatchedPattern :: Lens' WebhookFilter (Maybe Bool)
wfExcludeMatchedPattern = lens _wfExcludeMatchedPattern (\s a -> s {_wfExcludeMatchedPattern = a})

-- | The type of webhook filter. There are six webhook filter types: @EVENT@ , @ACTOR_ACCOUNT_ID@ , @HEAD_REF@ , @BASE_REF@ , @FILE_PATH@ , and @COMMIT_MESSAGE@ .      * EVENT     * A webhook event triggers a build when the provided @pattern@ matches one of five event types: @PUSH@ , @PULL_REQUEST_CREATED@ , @PULL_REQUEST_UPDATED@ , @PULL_REQUEST_REOPENED@ , and @PULL_REQUEST_MERGED@ . The @EVENT@ patterns are specified as a comma-separated string. For example, @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ filters all push, pull request created, and pull request updated events.      * ACTOR_ACCOUNT_ID     * A webhook event triggers a build when a GitHub, GitHub Enterprise, or Bitbucket account ID matches the regular expression @pattern@ .      * HEAD_REF     * A webhook event triggers a build when the head reference matches the regular expression @pattern@ . For example, @refs/heads/branch-name@ and @refs/tags/tag-name@ .  Works with GitHub and GitHub Enterprise push, GitHub and GitHub Enterprise pull request, Bitbucket push, and Bitbucket pull request events.      * BASE_REF     * A webhook event triggers a build when the base reference matches the regular expression @pattern@ . For example, @refs/heads/branch-name@ .      * FILE_PATH     * A webhook triggers a build when the path of a changed file matches the regular expression @pattern@ .      * COMMIT_MESSAGE    * A webhook triggers a build when the head commit message matches the regular expression @pattern@ .
wfType :: Lens' WebhookFilter WebhookFilterType
wfType = lens _wfType (\s a -> s {_wfType = a})

-- | For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string that specifies one or more events. For example, the webhook filter @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull request created, and pull request updated events to trigger a build.  For a @WebHookFilter@ that uses any of the other filter types, a regular expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@ for its @type@ and the pattern @^refs/heads/@ triggers a build when the head reference is a branch with a reference name @refs/heads/branch-name@ .
wfPattern :: Lens' WebhookFilter Text
wfPattern = lens _wfPattern (\s a -> s {_wfPattern = a})

instance FromJSON WebhookFilter where
  parseJSON =
    withObject
      "WebhookFilter"
      ( \x ->
          WebhookFilter'
            <$> (x .:? "excludeMatchedPattern")
            <*> (x .: "type")
            <*> (x .: "pattern")
      )

instance Hashable WebhookFilter

instance NFData WebhookFilter

instance ToJSON WebhookFilter where
  toJSON WebhookFilter' {..} =
    object
      ( catMaybes
          [ ("excludeMatchedPattern" .=) <$> _wfExcludeMatchedPattern,
            Just ("type" .= _wfType),
            Just ("pattern" .= _wfPattern)
          ]
      )
