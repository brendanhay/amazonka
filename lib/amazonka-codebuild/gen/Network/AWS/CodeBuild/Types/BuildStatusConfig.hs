{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildStatusConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildStatusConfig
  ( BuildStatusConfig (..),

    -- * Smart constructor
    mkBuildStatusConfig,

    -- * Lenses
    bscContext,
    bscTargetURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information that defines how the AWS CodeBuild build project reports the build status to the source provider.
--
-- /See:/ 'mkBuildStatusConfig' smart constructor.
data BuildStatusConfig = BuildStatusConfig'
  { context ::
      Lude.Maybe Lude.Text,
    targetURL :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildStatusConfig' with the minimum fields required to make a request.
--
-- * 'context' - Specifies the context of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.
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
-- * 'targetURL' - Specifies the target url of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.
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
mkBuildStatusConfig ::
  BuildStatusConfig
mkBuildStatusConfig =
  BuildStatusConfig'
    { context = Lude.Nothing,
      targetURL = Lude.Nothing
    }

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
bscContext :: Lens.Lens' BuildStatusConfig (Lude.Maybe Lude.Text)
bscContext = Lens.lens (context :: BuildStatusConfig -> Lude.Maybe Lude.Text) (\s a -> s {context = a} :: BuildStatusConfig)
{-# DEPRECATED bscContext "Use generic-lens or generic-optics with 'context' instead." #-}

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
-- /Note:/ Consider using 'targetURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bscTargetURL :: Lens.Lens' BuildStatusConfig (Lude.Maybe Lude.Text)
bscTargetURL = Lens.lens (targetURL :: BuildStatusConfig -> Lude.Maybe Lude.Text) (\s a -> s {targetURL = a} :: BuildStatusConfig)
{-# DEPRECATED bscTargetURL "Use generic-lens or generic-optics with 'targetURL' instead." #-}

instance Lude.FromJSON BuildStatusConfig where
  parseJSON =
    Lude.withObject
      "BuildStatusConfig"
      ( \x ->
          BuildStatusConfig'
            Lude.<$> (x Lude..:? "context") Lude.<*> (x Lude..:? "targetUrl")
      )

instance Lude.ToJSON BuildStatusConfig where
  toJSON BuildStatusConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("context" Lude..=) Lude.<$> context,
            ("targetUrl" Lude..=) Lude.<$> targetURL
          ]
      )
