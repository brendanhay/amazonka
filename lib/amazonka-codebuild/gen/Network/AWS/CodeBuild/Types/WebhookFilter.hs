{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.WebhookFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.WebhookFilter
  ( WebhookFilter (..)
  -- * Smart constructor
  , mkWebhookFilter
  -- * Lenses
  , wfType
  , wfPattern
  , wfExcludeMatchedPattern
  ) where

import qualified Network.AWS.CodeBuild.Types.WebhookFilterType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A filter used to determine which webhooks trigger a build. 
--
-- /See:/ 'mkWebhookFilter' smart constructor.
data WebhookFilter = WebhookFilter'
  { type' :: Types.WebhookFilterType
    -- ^ The type of webhook filter. There are six webhook filter types: @EVENT@ , @ACTOR_ACCOUNT_ID@ , @HEAD_REF@ , @BASE_REF@ , @FILE_PATH@ , and @COMMIT_MESSAGE@ . 
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
  , pattern' :: Core.Text
    -- ^ For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string that specifies one or more events. For example, the webhook filter @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull request created, and pull request updated events to trigger a build. 
--
-- For a @WebHookFilter@ that uses any of the other filter types, a regular expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@ for its @type@ and the pattern @^refs/heads/@ triggers a build when the head reference is a branch with a reference name @refs/heads/branch-name@ . 
  , excludeMatchedPattern :: Core.Maybe Core.Bool
    -- ^ Used to indicate that the @pattern@ determines which webhook events do not trigger a build. If true, then a webhook event that does not match the @pattern@ triggers a build. If false, then a webhook event that matches the @pattern@ triggers a build. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WebhookFilter' value with any optional fields omitted.
mkWebhookFilter
    :: Types.WebhookFilterType -- ^ 'type\''
    -> Core.Text -- ^ 'pattern\''
    -> WebhookFilter
mkWebhookFilter type' pattern'
  = WebhookFilter'{type', pattern',
                   excludeMatchedPattern = Core.Nothing}

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
wfType :: Lens.Lens' WebhookFilter Types.WebhookFilterType
wfType = Lens.field @"type'"
{-# INLINEABLE wfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string that specifies one or more events. For example, the webhook filter @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull request created, and pull request updated events to trigger a build. 
--
-- For a @WebHookFilter@ that uses any of the other filter types, a regular expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@ for its @type@ and the pattern @^refs/heads/@ triggers a build when the head reference is a branch with a reference name @refs/heads/branch-name@ . 
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfPattern :: Lens.Lens' WebhookFilter Core.Text
wfPattern = Lens.field @"pattern'"
{-# INLINEABLE wfPattern #-}
{-# DEPRECATED pattern' "Use generic-lens or generic-optics with 'pattern'' instead"  #-}

-- | Used to indicate that the @pattern@ determines which webhook events do not trigger a build. If true, then a webhook event that does not match the @pattern@ triggers a build. If false, then a webhook event that matches the @pattern@ triggers a build. 
--
-- /Note:/ Consider using 'excludeMatchedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfExcludeMatchedPattern :: Lens.Lens' WebhookFilter (Core.Maybe Core.Bool)
wfExcludeMatchedPattern = Lens.field @"excludeMatchedPattern"
{-# INLINEABLE wfExcludeMatchedPattern #-}
{-# DEPRECATED excludeMatchedPattern "Use generic-lens or generic-optics with 'excludeMatchedPattern' instead"  #-}

instance Core.FromJSON WebhookFilter where
        toJSON WebhookFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  Core.Just ("pattern" Core..= pattern'),
                  ("excludeMatchedPattern" Core..=) Core.<$> excludeMatchedPattern])

instance Core.FromJSON WebhookFilter where
        parseJSON
          = Core.withObject "WebhookFilter" Core.$
              \ x ->
                WebhookFilter' Core.<$>
                  (x Core..: "type") Core.<*> x Core..: "pattern" Core.<*>
                    x Core..:? "excludeMatchedPattern"
