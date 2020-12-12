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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A revision for an AWS Lambda or Amazon ECS deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda and Amazon ECS deployments, the revision is the same as the AppSpec file. This method replaces the deprecated @RawString@ data type.
--
-- /See:/ 'mkAppSpecContent' smart constructor.
data AppSpecContent = AppSpecContent'
  { content ::
      Lude.Maybe Lude.Text,
    sha256 :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppSpecContent' with the minimum fields required to make a request.
--
-- * 'content' - The YAML-formatted or JSON-formatted revision string.
--
-- For an AWS Lambda deployment, the content includes a Lambda function name, the alias for its original version, and the alias for its replacement version. The deployment shifts traffic from the original version of the Lambda function to the replacement version.
-- For an Amazon ECS deployment, the content includes the task name, information about the load balancer that serves traffic to the container, and more.
-- For both types of deployments, the content can specify Lambda functions that run at specified hooks, such as @BeforeInstall@ , during a deployment.
-- * 'sha256' - The SHA256 hash value of the revision content.
mkAppSpecContent ::
  AppSpecContent
mkAppSpecContent =
  AppSpecContent' {content = Lude.Nothing, sha256 = Lude.Nothing}

-- | The YAML-formatted or JSON-formatted revision string.
--
-- For an AWS Lambda deployment, the content includes a Lambda function name, the alias for its original version, and the alias for its replacement version. The deployment shifts traffic from the original version of the Lambda function to the replacement version.
-- For an Amazon ECS deployment, the content includes the task name, information about the load balancer that serves traffic to the container, and more.
-- For both types of deployments, the content can specify Lambda functions that run at specified hooks, such as @BeforeInstall@ , during a deployment.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascContent :: Lens.Lens' AppSpecContent (Lude.Maybe Lude.Text)
ascContent = Lens.lens (content :: AppSpecContent -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: AppSpecContent)
{-# DEPRECATED ascContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The SHA256 hash value of the revision content.
--
-- /Note:/ Consider using 'sha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascSha256 :: Lens.Lens' AppSpecContent (Lude.Maybe Lude.Text)
ascSha256 = Lens.lens (sha256 :: AppSpecContent -> Lude.Maybe Lude.Text) (\s a -> s {sha256 = a} :: AppSpecContent)
{-# DEPRECATED ascSha256 "Use generic-lens or generic-optics with 'sha256' instead." #-}

instance Lude.FromJSON AppSpecContent where
  parseJSON =
    Lude.withObject
      "AppSpecContent"
      ( \x ->
          AppSpecContent'
            Lude.<$> (x Lude..:? "content") Lude.<*> (x Lude..:? "sha256")
      )

instance Lude.ToJSON AppSpecContent where
  toJSON AppSpecContent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("content" Lude..=) Lude.<$> content,
            ("sha256" Lude..=) Lude.<$> sha256
          ]
      )
