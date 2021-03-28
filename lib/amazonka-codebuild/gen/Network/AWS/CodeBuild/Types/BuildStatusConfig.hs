{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildStatusConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.BuildStatusConfig
  ( BuildStatusConfig (..)
  -- * Smart constructor
  , mkBuildStatusConfig
  -- * Lenses
  , bscContext
  , bscTargetUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information that defines how the AWS CodeBuild build project reports the build status to the source provider. 
--
-- /See:/ 'mkBuildStatusConfig' smart constructor.
data BuildStatusConfig = BuildStatusConfig'
  { context :: Core.Maybe Core.Text
    -- ^ Specifies the context of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.
--
--
--     * Bitbucket
--
--     * This parameter is used for the @name@ parameter in the Bitbucket commit status. For more information, see <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build> in the Bitbucket API documentation.
--
--
--     * GitHub/GitHub Enterprise Server
--
--     * This parameter is used for the @context@ parameter in the GitHub commit status. For more information, see <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status> in the GitHub developer guide.
--
--
  , targetUrl :: Core.Maybe Core.Text
    -- ^ Specifies the target url of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.
--
--
--     * Bitbucket
--
--     * This parameter is used for the @url@ parameter in the Bitbucket commit status. For more information, see <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build> in the Bitbucket API documentation.
--
--
--     * GitHub/GitHub Enterprise Server
--
--     * This parameter is used for the @target_url@ parameter in the GitHub commit status. For more information, see <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status> in the GitHub developer guide.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BuildStatusConfig' value with any optional fields omitted.
mkBuildStatusConfig
    :: BuildStatusConfig
mkBuildStatusConfig
  = BuildStatusConfig'{context = Core.Nothing,
                       targetUrl = Core.Nothing}

-- | Specifies the context of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.
--
--
--     * Bitbucket
--
--     * This parameter is used for the @name@ parameter in the Bitbucket commit status. For more information, see <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build> in the Bitbucket API documentation.
--
--
--     * GitHub/GitHub Enterprise Server
--
--     * This parameter is used for the @context@ parameter in the GitHub commit status. For more information, see <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status> in the GitHub developer guide.
--
--
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bscContext :: Lens.Lens' BuildStatusConfig (Core.Maybe Core.Text)
bscContext = Lens.field @"context"
{-# INLINEABLE bscContext #-}
{-# DEPRECATED context "Use generic-lens or generic-optics with 'context' instead"  #-}

-- | Specifies the target url of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.
--
--
--     * Bitbucket
--
--     * This parameter is used for the @url@ parameter in the Bitbucket commit status. For more information, see <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build> in the Bitbucket API documentation.
--
--
--     * GitHub/GitHub Enterprise Server
--
--     * This parameter is used for the @target_url@ parameter in the GitHub commit status. For more information, see <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status> in the GitHub developer guide.
--
--
--
-- /Note:/ Consider using 'targetUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bscTargetUrl :: Lens.Lens' BuildStatusConfig (Core.Maybe Core.Text)
bscTargetUrl = Lens.field @"targetUrl"
{-# INLINEABLE bscTargetUrl #-}
{-# DEPRECATED targetUrl "Use generic-lens or generic-optics with 'targetUrl' instead"  #-}

instance Core.FromJSON BuildStatusConfig where
        toJSON BuildStatusConfig{..}
          = Core.object
              (Core.catMaybes
                 [("context" Core..=) Core.<$> context,
                  ("targetUrl" Core..=) Core.<$> targetUrl])

instance Core.FromJSON BuildStatusConfig where
        parseJSON
          = Core.withObject "BuildStatusConfig" Core.$
              \ x ->
                BuildStatusConfig' Core.<$>
                  (x Core..:? "context") Core.<*> x Core..:? "targetUrl"
