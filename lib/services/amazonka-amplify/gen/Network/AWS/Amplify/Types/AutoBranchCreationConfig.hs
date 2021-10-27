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
-- Module      : Network.AWS.Amplify.Types.AutoBranchCreationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Amplify.Types.AutoBranchCreationConfig where

import Network.AWS.Amplify.Types.Stage
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the automated branch creation configuration.
--
-- /See:/ 'newAutoBranchCreationConfig' smart constructor.
data AutoBranchCreationConfig = AutoBranchCreationConfig'
  { -- | The framework for the autocreated branch.
    framework :: Prelude.Maybe Prelude.Text,
    -- | Describes the current stage for the autocreated branch.
    stage :: Prelude.Maybe Stage,
    -- | Enables pull request previews for the autocreated branch.
    enablePullRequestPreview :: Prelude.Maybe Prelude.Bool,
    -- | The basic authorization credentials for the autocreated branch.
    basicAuthCredentials :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The build specification (build spec) for the autocreated branch.
    buildSpec :: Prelude.Maybe Prelude.Text,
    -- | Enables performance mode for the branch.
    --
    -- Performance mode optimizes for faster hosting performance by keeping
    -- content cached at the edge for a longer interval. When performance mode
    -- is enabled, hosting configuration or code changes can take up to 10
    -- minutes to roll out.
    enablePerformanceMode :: Prelude.Maybe Prelude.Bool,
    -- | The environment variables for the autocreated branch.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Enables auto building for the autocreated branch.
    enableAutoBuild :: Prelude.Maybe Prelude.Bool,
    -- | Enables basic authorization for the autocreated branch.
    enableBasicAuth :: Prelude.Maybe Prelude.Bool,
    -- | The Amplify environment name for the pull request.
    pullRequestEnvironmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoBranchCreationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framework', 'autoBranchCreationConfig_framework' - The framework for the autocreated branch.
--
-- 'stage', 'autoBranchCreationConfig_stage' - Describes the current stage for the autocreated branch.
--
-- 'enablePullRequestPreview', 'autoBranchCreationConfig_enablePullRequestPreview' - Enables pull request previews for the autocreated branch.
--
-- 'basicAuthCredentials', 'autoBranchCreationConfig_basicAuthCredentials' - The basic authorization credentials for the autocreated branch.
--
-- 'buildSpec', 'autoBranchCreationConfig_buildSpec' - The build specification (build spec) for the autocreated branch.
--
-- 'enablePerformanceMode', 'autoBranchCreationConfig_enablePerformanceMode' - Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
--
-- 'environmentVariables', 'autoBranchCreationConfig_environmentVariables' - The environment variables for the autocreated branch.
--
-- 'enableAutoBuild', 'autoBranchCreationConfig_enableAutoBuild' - Enables auto building for the autocreated branch.
--
-- 'enableBasicAuth', 'autoBranchCreationConfig_enableBasicAuth' - Enables basic authorization for the autocreated branch.
--
-- 'pullRequestEnvironmentName', 'autoBranchCreationConfig_pullRequestEnvironmentName' - The Amplify environment name for the pull request.
newAutoBranchCreationConfig ::
  AutoBranchCreationConfig
newAutoBranchCreationConfig =
  AutoBranchCreationConfig'
    { framework =
        Prelude.Nothing,
      stage = Prelude.Nothing,
      enablePullRequestPreview = Prelude.Nothing,
      basicAuthCredentials = Prelude.Nothing,
      buildSpec = Prelude.Nothing,
      enablePerformanceMode = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      enableAutoBuild = Prelude.Nothing,
      enableBasicAuth = Prelude.Nothing,
      pullRequestEnvironmentName = Prelude.Nothing
    }

-- | The framework for the autocreated branch.
autoBranchCreationConfig_framework :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Text)
autoBranchCreationConfig_framework = Lens.lens (\AutoBranchCreationConfig' {framework} -> framework) (\s@AutoBranchCreationConfig' {} a -> s {framework = a} :: AutoBranchCreationConfig)

-- | Describes the current stage for the autocreated branch.
autoBranchCreationConfig_stage :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Stage)
autoBranchCreationConfig_stage = Lens.lens (\AutoBranchCreationConfig' {stage} -> stage) (\s@AutoBranchCreationConfig' {} a -> s {stage = a} :: AutoBranchCreationConfig)

-- | Enables pull request previews for the autocreated branch.
autoBranchCreationConfig_enablePullRequestPreview :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Bool)
autoBranchCreationConfig_enablePullRequestPreview = Lens.lens (\AutoBranchCreationConfig' {enablePullRequestPreview} -> enablePullRequestPreview) (\s@AutoBranchCreationConfig' {} a -> s {enablePullRequestPreview = a} :: AutoBranchCreationConfig)

-- | The basic authorization credentials for the autocreated branch.
autoBranchCreationConfig_basicAuthCredentials :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Text)
autoBranchCreationConfig_basicAuthCredentials = Lens.lens (\AutoBranchCreationConfig' {basicAuthCredentials} -> basicAuthCredentials) (\s@AutoBranchCreationConfig' {} a -> s {basicAuthCredentials = a} :: AutoBranchCreationConfig) Prelude.. Lens.mapping Core._Sensitive

-- | The build specification (build spec) for the autocreated branch.
autoBranchCreationConfig_buildSpec :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Text)
autoBranchCreationConfig_buildSpec = Lens.lens (\AutoBranchCreationConfig' {buildSpec} -> buildSpec) (\s@AutoBranchCreationConfig' {} a -> s {buildSpec = a} :: AutoBranchCreationConfig)

-- | Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
autoBranchCreationConfig_enablePerformanceMode :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Bool)
autoBranchCreationConfig_enablePerformanceMode = Lens.lens (\AutoBranchCreationConfig' {enablePerformanceMode} -> enablePerformanceMode) (\s@AutoBranchCreationConfig' {} a -> s {enablePerformanceMode = a} :: AutoBranchCreationConfig)

-- | The environment variables for the autocreated branch.
autoBranchCreationConfig_environmentVariables :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
autoBranchCreationConfig_environmentVariables = Lens.lens (\AutoBranchCreationConfig' {environmentVariables} -> environmentVariables) (\s@AutoBranchCreationConfig' {} a -> s {environmentVariables = a} :: AutoBranchCreationConfig) Prelude.. Lens.mapping Lens.coerced

-- | Enables auto building for the autocreated branch.
autoBranchCreationConfig_enableAutoBuild :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Bool)
autoBranchCreationConfig_enableAutoBuild = Lens.lens (\AutoBranchCreationConfig' {enableAutoBuild} -> enableAutoBuild) (\s@AutoBranchCreationConfig' {} a -> s {enableAutoBuild = a} :: AutoBranchCreationConfig)

-- | Enables basic authorization for the autocreated branch.
autoBranchCreationConfig_enableBasicAuth :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Bool)
autoBranchCreationConfig_enableBasicAuth = Lens.lens (\AutoBranchCreationConfig' {enableBasicAuth} -> enableBasicAuth) (\s@AutoBranchCreationConfig' {} a -> s {enableBasicAuth = a} :: AutoBranchCreationConfig)

-- | The Amplify environment name for the pull request.
autoBranchCreationConfig_pullRequestEnvironmentName :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Text)
autoBranchCreationConfig_pullRequestEnvironmentName = Lens.lens (\AutoBranchCreationConfig' {pullRequestEnvironmentName} -> pullRequestEnvironmentName) (\s@AutoBranchCreationConfig' {} a -> s {pullRequestEnvironmentName = a} :: AutoBranchCreationConfig)

instance Core.FromJSON AutoBranchCreationConfig where
  parseJSON =
    Core.withObject
      "AutoBranchCreationConfig"
      ( \x ->
          AutoBranchCreationConfig'
            Prelude.<$> (x Core..:? "framework")
            Prelude.<*> (x Core..:? "stage")
            Prelude.<*> (x Core..:? "enablePullRequestPreview")
            Prelude.<*> (x Core..:? "basicAuthCredentials")
            Prelude.<*> (x Core..:? "buildSpec")
            Prelude.<*> (x Core..:? "enablePerformanceMode")
            Prelude.<*> ( x Core..:? "environmentVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "enableAutoBuild")
            Prelude.<*> (x Core..:? "enableBasicAuth")
            Prelude.<*> (x Core..:? "pullRequestEnvironmentName")
      )

instance Prelude.Hashable AutoBranchCreationConfig

instance Prelude.NFData AutoBranchCreationConfig

instance Core.ToJSON AutoBranchCreationConfig where
  toJSON AutoBranchCreationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("framework" Core..=) Prelude.<$> framework,
            ("stage" Core..=) Prelude.<$> stage,
            ("enablePullRequestPreview" Core..=)
              Prelude.<$> enablePullRequestPreview,
            ("basicAuthCredentials" Core..=)
              Prelude.<$> basicAuthCredentials,
            ("buildSpec" Core..=) Prelude.<$> buildSpec,
            ("enablePerformanceMode" Core..=)
              Prelude.<$> enablePerformanceMode,
            ("environmentVariables" Core..=)
              Prelude.<$> environmentVariables,
            ("enableAutoBuild" Core..=)
              Prelude.<$> enableAutoBuild,
            ("enableBasicAuth" Core..=)
              Prelude.<$> enableBasicAuth,
            ("pullRequestEnvironmentName" Core..=)
              Prelude.<$> pullRequestEnvironmentName
          ]
      )
