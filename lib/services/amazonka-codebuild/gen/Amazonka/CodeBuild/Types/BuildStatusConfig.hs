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
-- Module      : Amazonka.CodeBuild.Types.BuildStatusConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildStatusConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information that defines how the CodeBuild build project
-- reports the build status to the source provider.
--
-- /See:/ 'newBuildStatusConfig' smart constructor.
data BuildStatusConfig = BuildStatusConfig'
  { -- | Specifies the context of the build status CodeBuild sends to the source
    -- provider. The usage of this parameter depends on the source provider.
    --
    -- [Bitbucket]
    --     This parameter is used for the @name@ parameter in the Bitbucket
    --     commit status. For more information, see
    --     <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build>
    --     in the Bitbucket API documentation.
    --
    -- [GitHub\/GitHub Enterprise Server]
    --     This parameter is used for the @context@ parameter in the GitHub
    --     commit status. For more information, see
    --     <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status>
    --     in the GitHub developer guide.
    context :: Prelude.Maybe Prelude.Text,
    -- | Specifies the target url of the build status CodeBuild sends to the
    -- source provider. The usage of this parameter depends on the source
    -- provider.
    --
    -- [Bitbucket]
    --     This parameter is used for the @url@ parameter in the Bitbucket
    --     commit status. For more information, see
    --     <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build>
    --     in the Bitbucket API documentation.
    --
    -- [GitHub\/GitHub Enterprise Server]
    --     This parameter is used for the @target_url@ parameter in the GitHub
    --     commit status. For more information, see
    --     <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status>
    --     in the GitHub developer guide.
    targetUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildStatusConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'context', 'buildStatusConfig_context' - Specifies the context of the build status CodeBuild sends to the source
-- provider. The usage of this parameter depends on the source provider.
--
-- [Bitbucket]
--     This parameter is used for the @name@ parameter in the Bitbucket
--     commit status. For more information, see
--     <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build>
--     in the Bitbucket API documentation.
--
-- [GitHub\/GitHub Enterprise Server]
--     This parameter is used for the @context@ parameter in the GitHub
--     commit status. For more information, see
--     <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status>
--     in the GitHub developer guide.
--
-- 'targetUrl', 'buildStatusConfig_targetUrl' - Specifies the target url of the build status CodeBuild sends to the
-- source provider. The usage of this parameter depends on the source
-- provider.
--
-- [Bitbucket]
--     This parameter is used for the @url@ parameter in the Bitbucket
--     commit status. For more information, see
--     <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build>
--     in the Bitbucket API documentation.
--
-- [GitHub\/GitHub Enterprise Server]
--     This parameter is used for the @target_url@ parameter in the GitHub
--     commit status. For more information, see
--     <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status>
--     in the GitHub developer guide.
newBuildStatusConfig ::
  BuildStatusConfig
newBuildStatusConfig =
  BuildStatusConfig'
    { context = Prelude.Nothing,
      targetUrl = Prelude.Nothing
    }

-- | Specifies the context of the build status CodeBuild sends to the source
-- provider. The usage of this parameter depends on the source provider.
--
-- [Bitbucket]
--     This parameter is used for the @name@ parameter in the Bitbucket
--     commit status. For more information, see
--     <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build>
--     in the Bitbucket API documentation.
--
-- [GitHub\/GitHub Enterprise Server]
--     This parameter is used for the @context@ parameter in the GitHub
--     commit status. For more information, see
--     <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status>
--     in the GitHub developer guide.
buildStatusConfig_context :: Lens.Lens' BuildStatusConfig (Prelude.Maybe Prelude.Text)
buildStatusConfig_context = Lens.lens (\BuildStatusConfig' {context} -> context) (\s@BuildStatusConfig' {} a -> s {context = a} :: BuildStatusConfig)

-- | Specifies the target url of the build status CodeBuild sends to the
-- source provider. The usage of this parameter depends on the source
-- provider.
--
-- [Bitbucket]
--     This parameter is used for the @url@ parameter in the Bitbucket
--     commit status. For more information, see
--     <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build>
--     in the Bitbucket API documentation.
--
-- [GitHub\/GitHub Enterprise Server]
--     This parameter is used for the @target_url@ parameter in the GitHub
--     commit status. For more information, see
--     <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status>
--     in the GitHub developer guide.
buildStatusConfig_targetUrl :: Lens.Lens' BuildStatusConfig (Prelude.Maybe Prelude.Text)
buildStatusConfig_targetUrl = Lens.lens (\BuildStatusConfig' {targetUrl} -> targetUrl) (\s@BuildStatusConfig' {} a -> s {targetUrl = a} :: BuildStatusConfig)

instance Data.FromJSON BuildStatusConfig where
  parseJSON =
    Data.withObject
      "BuildStatusConfig"
      ( \x ->
          BuildStatusConfig'
            Prelude.<$> (x Data..:? "context")
            Prelude.<*> (x Data..:? "targetUrl")
      )

instance Prelude.Hashable BuildStatusConfig where
  hashWithSalt _salt BuildStatusConfig' {..} =
    _salt `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` targetUrl

instance Prelude.NFData BuildStatusConfig where
  rnf BuildStatusConfig' {..} =
    Prelude.rnf context
      `Prelude.seq` Prelude.rnf targetUrl

instance Data.ToJSON BuildStatusConfig where
  toJSON BuildStatusConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("context" Data..=) Prelude.<$> context,
            ("targetUrl" Data..=) Prelude.<$> targetUrl
          ]
      )
