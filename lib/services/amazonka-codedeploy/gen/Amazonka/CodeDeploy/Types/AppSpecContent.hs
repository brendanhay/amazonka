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
-- Module      : Amazonka.CodeDeploy.Types.AppSpecContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.AppSpecContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A revision for an Lambda or Amazon ECS deployment that is a
-- YAML-formatted or JSON-formatted string. For Lambda and Amazon ECS
-- deployments, the revision is the same as the AppSpec file. This method
-- replaces the deprecated @RawString@ data type.
--
-- /See:/ 'newAppSpecContent' smart constructor.
data AppSpecContent = AppSpecContent'
  { -- | The YAML-formatted or JSON-formatted revision string.
    --
    -- For an Lambda deployment, the content includes a Lambda function name,
    -- the alias for its original version, and the alias for its replacement
    -- version. The deployment shifts traffic from the original version of the
    -- Lambda function to the replacement version.
    --
    -- For an Amazon ECS deployment, the content includes the task name,
    -- information about the load balancer that serves traffic to the
    -- container, and more.
    --
    -- For both types of deployments, the content can specify Lambda functions
    -- that run at specified hooks, such as @BeforeInstall@, during a
    -- deployment.
    content :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 hash value of the revision content.
    sha256 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- For an Lambda deployment, the content includes a Lambda function name,
-- the alias for its original version, and the alias for its replacement
-- version. The deployment shifts traffic from the original version of the
-- Lambda function to the replacement version.
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
    { content = Prelude.Nothing,
      sha256 = Prelude.Nothing
    }

-- | The YAML-formatted or JSON-formatted revision string.
--
-- For an Lambda deployment, the content includes a Lambda function name,
-- the alias for its original version, and the alias for its replacement
-- version. The deployment shifts traffic from the original version of the
-- Lambda function to the replacement version.
--
-- For an Amazon ECS deployment, the content includes the task name,
-- information about the load balancer that serves traffic to the
-- container, and more.
--
-- For both types of deployments, the content can specify Lambda functions
-- that run at specified hooks, such as @BeforeInstall@, during a
-- deployment.
appSpecContent_content :: Lens.Lens' AppSpecContent (Prelude.Maybe Prelude.Text)
appSpecContent_content = Lens.lens (\AppSpecContent' {content} -> content) (\s@AppSpecContent' {} a -> s {content = a} :: AppSpecContent)

-- | The SHA256 hash value of the revision content.
appSpecContent_sha256 :: Lens.Lens' AppSpecContent (Prelude.Maybe Prelude.Text)
appSpecContent_sha256 = Lens.lens (\AppSpecContent' {sha256} -> sha256) (\s@AppSpecContent' {} a -> s {sha256 = a} :: AppSpecContent)

instance Data.FromJSON AppSpecContent where
  parseJSON =
    Data.withObject
      "AppSpecContent"
      ( \x ->
          AppSpecContent'
            Prelude.<$> (x Data..:? "content")
            Prelude.<*> (x Data..:? "sha256")
      )

instance Prelude.Hashable AppSpecContent where
  hashWithSalt _salt AppSpecContent' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` sha256

instance Prelude.NFData AppSpecContent where
  rnf AppSpecContent' {..} =
    Prelude.rnf content `Prelude.seq`
      Prelude.rnf sha256

instance Data.ToJSON AppSpecContent where
  toJSON AppSpecContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("content" Data..=) Prelude.<$> content,
            ("sha256" Data..=) Prelude.<$> sha256
          ]
      )
