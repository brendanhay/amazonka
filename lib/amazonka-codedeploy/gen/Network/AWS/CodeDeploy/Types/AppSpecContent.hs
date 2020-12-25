{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AppSpecContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AppSpecContent
  ( AppSpecContent (..),

    -- * Smart constructor
    mkAppSpecContent,

    -- * Lenses
    ascContent,
    ascSha256,
  )
where

import qualified Network.AWS.CodeDeploy.Types.Content as Types
import qualified Network.AWS.CodeDeploy.Types.RawStringSha256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A revision for an AWS Lambda or Amazon ECS deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda and Amazon ECS deployments, the revision is the same as the AppSpec file. This method replaces the deprecated @RawString@ data type.
--
-- /See:/ 'mkAppSpecContent' smart constructor.
data AppSpecContent = AppSpecContent'
  { -- | The YAML-formatted or JSON-formatted revision string.
    --
    -- For an AWS Lambda deployment, the content includes a Lambda function name, the alias for its original version, and the alias for its replacement version. The deployment shifts traffic from the original version of the Lambda function to the replacement version.
    -- For an Amazon ECS deployment, the content includes the task name, information about the load balancer that serves traffic to the container, and more.
    -- For both types of deployments, the content can specify Lambda functions that run at specified hooks, such as @BeforeInstall@ , during a deployment.
    content :: Core.Maybe Types.Content,
    -- | The SHA256 hash value of the revision content.
    sha256 :: Core.Maybe Types.RawStringSha256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AppSpecContent' value with any optional fields omitted.
mkAppSpecContent ::
  AppSpecContent
mkAppSpecContent =
  AppSpecContent' {content = Core.Nothing, sha256 = Core.Nothing}

-- | The YAML-formatted or JSON-formatted revision string.
--
-- For an AWS Lambda deployment, the content includes a Lambda function name, the alias for its original version, and the alias for its replacement version. The deployment shifts traffic from the original version of the Lambda function to the replacement version.
-- For an Amazon ECS deployment, the content includes the task name, information about the load balancer that serves traffic to the container, and more.
-- For both types of deployments, the content can specify Lambda functions that run at specified hooks, such as @BeforeInstall@ , during a deployment.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascContent :: Lens.Lens' AppSpecContent (Core.Maybe Types.Content)
ascContent = Lens.field @"content"
{-# DEPRECATED ascContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The SHA256 hash value of the revision content.
--
-- /Note:/ Consider using 'sha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascSha256 :: Lens.Lens' AppSpecContent (Core.Maybe Types.RawStringSha256)
ascSha256 = Lens.field @"sha256"
{-# DEPRECATED ascSha256 "Use generic-lens or generic-optics with 'sha256' instead." #-}

instance Core.FromJSON AppSpecContent where
  toJSON AppSpecContent {..} =
    Core.object
      ( Core.catMaybes
          [ ("content" Core..=) Core.<$> content,
            ("sha256" Core..=) Core.<$> sha256
          ]
      )

instance Core.FromJSON AppSpecContent where
  parseJSON =
    Core.withObject "AppSpecContent" Core.$
      \x ->
        AppSpecContent'
          Core.<$> (x Core..:? "content") Core.<*> (x Core..:? "sha256")
