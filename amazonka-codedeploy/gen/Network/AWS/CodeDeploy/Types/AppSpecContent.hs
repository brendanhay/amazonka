{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AppSpecContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AppSpecContent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A revision for an AWS Lambda or Amazon ECS deployment that is a
-- YAML-formatted or JSON-formatted string. For AWS Lambda and Amazon ECS
-- deployments, the revision is the same as the AppSpec file. This method
-- replaces the deprecated @RawString@ data type.
--
-- /See:/ 'newAppSpecContent' smart constructor.
data AppSpecContent = AppSpecContent'
  { -- | The YAML-formatted or JSON-formatted revision string.
    --
    -- For an AWS Lambda deployment, the content includes a Lambda function
    -- name, the alias for its original version, and the alias for its
    -- replacement version. The deployment shifts traffic from the original
    -- version of the Lambda function to the replacement version.
    --
    -- For an Amazon ECS deployment, the content includes the task name,
    -- information about the load balancer that serves traffic to the
    -- container, and more.
    --
    -- For both types of deployments, the content can specify Lambda functions
    -- that run at specified hooks, such as @BeforeInstall@, during a
    -- deployment.
    content :: Core.Maybe Core.Text,
    -- | The SHA256 hash value of the revision content.
    sha256 :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AppSpecContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'appSpecContent_content' - The YAML-formatted or JSON-formatted revision string.
--
-- For an AWS Lambda deployment, the content includes a Lambda function
-- name, the alias for its original version, and the alias for its
-- replacement version. The deployment shifts traffic from the original
-- version of the Lambda function to the replacement version.
--
-- For an Amazon ECS deployment, the content includes the task name,
-- information about the load balancer that serves traffic to the
-- container, and more.
--
-- For both types of deployments, the content can specify Lambda functions
-- that run at specified hooks, such as @BeforeInstall@, during a
-- deployment.
--
-- 'sha256', 'appSpecContent_sha256' - The SHA256 hash value of the revision content.
newAppSpecContent ::
  AppSpecContent
newAppSpecContent =
  AppSpecContent'
    { content = Core.Nothing,
      sha256 = Core.Nothing
    }

-- | The YAML-formatted or JSON-formatted revision string.
--
-- For an AWS Lambda deployment, the content includes a Lambda function
-- name, the alias for its original version, and the alias for its
-- replacement version. The deployment shifts traffic from the original
-- version of the Lambda function to the replacement version.
--
-- For an Amazon ECS deployment, the content includes the task name,
-- information about the load balancer that serves traffic to the
-- container, and more.
--
-- For both types of deployments, the content can specify Lambda functions
-- that run at specified hooks, such as @BeforeInstall@, during a
-- deployment.
appSpecContent_content :: Lens.Lens' AppSpecContent (Core.Maybe Core.Text)
appSpecContent_content = Lens.lens (\AppSpecContent' {content} -> content) (\s@AppSpecContent' {} a -> s {content = a} :: AppSpecContent)

-- | The SHA256 hash value of the revision content.
appSpecContent_sha256 :: Lens.Lens' AppSpecContent (Core.Maybe Core.Text)
appSpecContent_sha256 = Lens.lens (\AppSpecContent' {sha256} -> sha256) (\s@AppSpecContent' {} a -> s {sha256 = a} :: AppSpecContent)

instance Core.FromJSON AppSpecContent where
  parseJSON =
    Core.withObject
      "AppSpecContent"
      ( \x ->
          AppSpecContent'
            Core.<$> (x Core..:? "content")
            Core.<*> (x Core..:? "sha256")
      )

instance Core.Hashable AppSpecContent

instance Core.NFData AppSpecContent

instance Core.ToJSON AppSpecContent where
  toJSON AppSpecContent' {..} =
    Core.object
      ( Core.catMaybes
          [ ("content" Core..=) Core.<$> content,
            ("sha256" Core..=) Core.<$> sha256
          ]
      )
