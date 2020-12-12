{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.WebhookFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.WebhookFilter
  ( WebhookFilter (..),

    -- * Smart constructor
    mkWebhookFilter,

    -- * Lenses
    wfExcludeMatchedPattern,
    wfType,
    wfPattern,
  )
where

import Network.AWS.CodeBuild.Types.WebhookFilterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A filter used to determine which webhooks trigger a build.
--
-- /See:/ 'mkWebhookFilter' smart constructor.
data WebhookFilter = WebhookFilter'
  { excludeMatchedPattern ::
      Lude.Maybe Lude.Bool,
    type' :: WebhookFilterType,
    pattern' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WebhookFilter' with the minimum fields required to make a request.
--
-- * 'excludeMatchedPattern' - Used to indicate that the @pattern@ determines which webhook events do not trigger a build. If true, then a webhook event that does not match the @pattern@ triggers a build. If false, then a webhook event that matches the @pattern@ triggers a build.
-- * 'pattern'' - For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string that specifies one or more events. For example, the webhook filter @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull request created, and pull request updated events to trigger a build.
--
-- For a @WebHookFilter@ that uses any of the other filter types, a regular expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@ for its @type@ and the pattern @^refs/heads/@ triggers a build when the head reference is a branch with a reference name @refs/heads/branch-name@ .
-- * 'type'' - The type of webhook filter. There are six webhook filter types: @EVENT@ , @ACTOR_ACCOUNT_ID@ , @HEAD_REF@ , @BASE_REF@ , @FILE_PATH@ , and @COMMIT_MESSAGE@ .
--
--
--     * EVENT
--
--     * A webhook event triggers a build when the provided @pattern@ matches one of five event types: @PUSH@ , @PULL_REQUEST_CREATED@ , @PULL_REQUEST_UPDATED@ , @PULL_REQUEST_REOPENED@ , and @PULL_REQUEST_MERGED@ . The @EVENT@ patterns are specified as a comma-separated string. For example, @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ filters all push, pull request created, and pull request updated events.
--
--
--     * ACTOR_ACCOUNT_ID
--
--     * A webhook event triggers a build when a GitHub, GitHub Enterprise, or Bitbucket account ID matches the regular expression @pattern@ .
--
--
--     * HEAD_REF
--
--     * A webhook event triggers a build when the head reference matches the regular expression @pattern@ . For example, @refs/heads/branch-name@ and @refs/tags/tag-name@ .
-- Works with GitHub and GitHub Enterprise push, GitHub and GitHub Enterprise pull request, Bitbucket push, and Bitbucket pull request events.
--
--
--     * BASE_REF
--
--     * A webhook event triggers a build when the base reference matches the regular expression @pattern@ . For example, @refs/heads/branch-name@ .
--
--
--     * FILE_PATH
--
--     * A webhook triggers a build when the path of a changed file matches the regular expression @pattern@ .
--
--
--     * COMMIT_MESSAGE
--
--     * A webhook triggers a build when the head commit message matches the regular expression @pattern@ .
mkWebhookFilter ::
  -- | 'type''
  WebhookFilterType ->
  -- | 'pattern''
  Lude.Text ->
  WebhookFilter
mkWebhookFilter pType_ pPattern_ =
  WebhookFilter'
    { excludeMatchedPattern = Lude.Nothing,
      type' = pType_,
      pattern' = pPattern_
    }

-- | Used to indicate that the @pattern@ determines which webhook events do not trigger a build. If true, then a webhook event that does not match the @pattern@ triggers a build. If false, then a webhook event that matches the @pattern@ triggers a build.
--
-- /Note:/ Consider using 'excludeMatchedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfExcludeMatchedPattern :: Lens.Lens' WebhookFilter (Lude.Maybe Lude.Bool)
wfExcludeMatchedPattern = Lens.lens (excludeMatchedPattern :: WebhookFilter -> Lude.Maybe Lude.Bool) (\s a -> s {excludeMatchedPattern = a} :: WebhookFilter)
{-# DEPRECATED wfExcludeMatchedPattern "Use generic-lens or generic-optics with 'excludeMatchedPattern' instead." #-}

-- | The type of webhook filter. There are six webhook filter types: @EVENT@ , @ACTOR_ACCOUNT_ID@ , @HEAD_REF@ , @BASE_REF@ , @FILE_PATH@ , and @COMMIT_MESSAGE@ .
--
--
--     * EVENT
--
--     * A webhook event triggers a build when the provided @pattern@ matches one of five event types: @PUSH@ , @PULL_REQUEST_CREATED@ , @PULL_REQUEST_UPDATED@ , @PULL_REQUEST_REOPENED@ , and @PULL_REQUEST_MERGED@ . The @EVENT@ patterns are specified as a comma-separated string. For example, @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ filters all push, pull request created, and pull request updated events.
--
--
--     * ACTOR_ACCOUNT_ID
--
--     * A webhook event triggers a build when a GitHub, GitHub Enterprise, or Bitbucket account ID matches the regular expression @pattern@ .
--
--
--     * HEAD_REF
--
--     * A webhook event triggers a build when the head reference matches the regular expression @pattern@ . For example, @refs/heads/branch-name@ and @refs/tags/tag-name@ .
-- Works with GitHub and GitHub Enterprise push, GitHub and GitHub Enterprise pull request, Bitbucket push, and Bitbucket pull request events.
--
--
--     * BASE_REF
--
--     * A webhook event triggers a build when the base reference matches the regular expression @pattern@ . For example, @refs/heads/branch-name@ .
--
--
--     * FILE_PATH
--
--     * A webhook triggers a build when the path of a changed file matches the regular expression @pattern@ .
--
--
--     * COMMIT_MESSAGE
--
--     * A webhook triggers a build when the head commit message matches the regular expression @pattern@ .
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfType :: Lens.Lens' WebhookFilter WebhookFilterType
wfType = Lens.lens (type' :: WebhookFilter -> WebhookFilterType) (\s a -> s {type' = a} :: WebhookFilter)
{-# DEPRECATED wfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string that specifies one or more events. For example, the webhook filter @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull request created, and pull request updated events to trigger a build.
--
-- For a @WebHookFilter@ that uses any of the other filter types, a regular expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@ for its @type@ and the pattern @^refs/heads/@ triggers a build when the head reference is a branch with a reference name @refs/heads/branch-name@ .
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfPattern :: Lens.Lens' WebhookFilter Lude.Text
wfPattern = Lens.lens (pattern' :: WebhookFilter -> Lude.Text) (\s a -> s {pattern' = a} :: WebhookFilter)
{-# DEPRECATED wfPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

instance Lude.FromJSON WebhookFilter where
  parseJSON =
    Lude.withObject
      "WebhookFilter"
      ( \x ->
          WebhookFilter'
            Lude.<$> (x Lude..:? "excludeMatchedPattern")
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..: "pattern")
      )

instance Lude.ToJSON WebhookFilter where
  toJSON WebhookFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("excludeMatchedPattern" Lude..=) Lude.<$> excludeMatchedPattern,
            Lude.Just ("type" Lude..= type'),
            Lude.Just ("pattern" Lude..= pattern')
          ]
      )
