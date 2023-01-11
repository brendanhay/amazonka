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
-- Module      : Amazonka.Amplify.Types.AutoBranchCreationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.AutoBranchCreationConfig where

import Amazonka.Amplify.Types.Stage
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the automated branch creation configuration.
--
-- /See:/ 'newAutoBranchCreationConfig' smart constructor.
data AutoBranchCreationConfig = AutoBranchCreationConfig'
  { -- | The basic authorization credentials for the autocreated branch. You must
    -- base64-encode the authorization credentials and provide them in the
    -- format @user:password@.
    basicAuthCredentials :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The build specification (build spec) for the autocreated branch.
    buildSpec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Enables auto building for the autocreated branch.
    enableAutoBuild :: Prelude.Maybe Prelude.Bool,
    -- | Enables basic authorization for the autocreated branch.
    enableBasicAuth :: Prelude.Maybe Prelude.Bool,
    -- | Enables performance mode for the branch.
    --
    -- Performance mode optimizes for faster hosting performance by keeping
    -- content cached at the edge for a longer interval. When performance mode
    -- is enabled, hosting configuration or code changes can take up to 10
    -- minutes to roll out.
    enablePerformanceMode :: Prelude.Maybe Prelude.Bool,
    -- | Enables pull request previews for the autocreated branch.
    enablePullRequestPreview :: Prelude.Maybe Prelude.Bool,
    -- | The environment variables for the autocreated branch.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The framework for the autocreated branch.
    framework :: Prelude.Maybe Prelude.Text,
    -- | The Amplify environment name for the pull request.
    pullRequestEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | Describes the current stage for the autocreated branch.
    stage :: Prelude.Maybe Stage
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
-- 'basicAuthCredentials', 'autoBranchCreationConfig_basicAuthCredentials' - The basic authorization credentials for the autocreated branch. You must
-- base64-encode the authorization credentials and provide them in the
-- format @user:password@.
--
-- 'buildSpec', 'autoBranchCreationConfig_buildSpec' - The build specification (build spec) for the autocreated branch.
--
-- 'enableAutoBuild', 'autoBranchCreationConfig_enableAutoBuild' - Enables auto building for the autocreated branch.
--
-- 'enableBasicAuth', 'autoBranchCreationConfig_enableBasicAuth' - Enables basic authorization for the autocreated branch.
--
-- 'enablePerformanceMode', 'autoBranchCreationConfig_enablePerformanceMode' - Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
--
-- 'enablePullRequestPreview', 'autoBranchCreationConfig_enablePullRequestPreview' - Enables pull request previews for the autocreated branch.
--
-- 'environmentVariables', 'autoBranchCreationConfig_environmentVariables' - The environment variables for the autocreated branch.
--
-- 'framework', 'autoBranchCreationConfig_framework' - The framework for the autocreated branch.
--
-- 'pullRequestEnvironmentName', 'autoBranchCreationConfig_pullRequestEnvironmentName' - The Amplify environment name for the pull request.
--
-- 'stage', 'autoBranchCreationConfig_stage' - Describes the current stage for the autocreated branch.
newAutoBranchCreationConfig ::
  AutoBranchCreationConfig
newAutoBranchCreationConfig =
  AutoBranchCreationConfig'
    { basicAuthCredentials =
        Prelude.Nothing,
      buildSpec = Prelude.Nothing,
      enableAutoBuild = Prelude.Nothing,
      enableBasicAuth = Prelude.Nothing,
      enablePerformanceMode = Prelude.Nothing,
      enablePullRequestPreview = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      framework = Prelude.Nothing,
      pullRequestEnvironmentName = Prelude.Nothing,
      stage = Prelude.Nothing
    }

-- | The basic authorization credentials for the autocreated branch. You must
-- base64-encode the authorization credentials and provide them in the
-- format @user:password@.
autoBranchCreationConfig_basicAuthCredentials :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Text)
autoBranchCreationConfig_basicAuthCredentials = Lens.lens (\AutoBranchCreationConfig' {basicAuthCredentials} -> basicAuthCredentials) (\s@AutoBranchCreationConfig' {} a -> s {basicAuthCredentials = a} :: AutoBranchCreationConfig) Prelude.. Lens.mapping Data._Sensitive

-- | The build specification (build spec) for the autocreated branch.
autoBranchCreationConfig_buildSpec :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Text)
autoBranchCreationConfig_buildSpec = Lens.lens (\AutoBranchCreationConfig' {buildSpec} -> buildSpec) (\s@AutoBranchCreationConfig' {} a -> s {buildSpec = a} :: AutoBranchCreationConfig) Prelude.. Lens.mapping Data._Sensitive

-- | Enables auto building for the autocreated branch.
autoBranchCreationConfig_enableAutoBuild :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Bool)
autoBranchCreationConfig_enableAutoBuild = Lens.lens (\AutoBranchCreationConfig' {enableAutoBuild} -> enableAutoBuild) (\s@AutoBranchCreationConfig' {} a -> s {enableAutoBuild = a} :: AutoBranchCreationConfig)

-- | Enables basic authorization for the autocreated branch.
autoBranchCreationConfig_enableBasicAuth :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Bool)
autoBranchCreationConfig_enableBasicAuth = Lens.lens (\AutoBranchCreationConfig' {enableBasicAuth} -> enableBasicAuth) (\s@AutoBranchCreationConfig' {} a -> s {enableBasicAuth = a} :: AutoBranchCreationConfig)

-- | Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
autoBranchCreationConfig_enablePerformanceMode :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Bool)
autoBranchCreationConfig_enablePerformanceMode = Lens.lens (\AutoBranchCreationConfig' {enablePerformanceMode} -> enablePerformanceMode) (\s@AutoBranchCreationConfig' {} a -> s {enablePerformanceMode = a} :: AutoBranchCreationConfig)

-- | Enables pull request previews for the autocreated branch.
autoBranchCreationConfig_enablePullRequestPreview :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Bool)
autoBranchCreationConfig_enablePullRequestPreview = Lens.lens (\AutoBranchCreationConfig' {enablePullRequestPreview} -> enablePullRequestPreview) (\s@AutoBranchCreationConfig' {} a -> s {enablePullRequestPreview = a} :: AutoBranchCreationConfig)

-- | The environment variables for the autocreated branch.
autoBranchCreationConfig_environmentVariables :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
autoBranchCreationConfig_environmentVariables = Lens.lens (\AutoBranchCreationConfig' {environmentVariables} -> environmentVariables) (\s@AutoBranchCreationConfig' {} a -> s {environmentVariables = a} :: AutoBranchCreationConfig) Prelude.. Lens.mapping Lens.coerced

-- | The framework for the autocreated branch.
autoBranchCreationConfig_framework :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Text)
autoBranchCreationConfig_framework = Lens.lens (\AutoBranchCreationConfig' {framework} -> framework) (\s@AutoBranchCreationConfig' {} a -> s {framework = a} :: AutoBranchCreationConfig)

-- | The Amplify environment name for the pull request.
autoBranchCreationConfig_pullRequestEnvironmentName :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Prelude.Text)
autoBranchCreationConfig_pullRequestEnvironmentName = Lens.lens (\AutoBranchCreationConfig' {pullRequestEnvironmentName} -> pullRequestEnvironmentName) (\s@AutoBranchCreationConfig' {} a -> s {pullRequestEnvironmentName = a} :: AutoBranchCreationConfig)

-- | Describes the current stage for the autocreated branch.
autoBranchCreationConfig_stage :: Lens.Lens' AutoBranchCreationConfig (Prelude.Maybe Stage)
autoBranchCreationConfig_stage = Lens.lens (\AutoBranchCreationConfig' {stage} -> stage) (\s@AutoBranchCreationConfig' {} a -> s {stage = a} :: AutoBranchCreationConfig)

instance Data.FromJSON AutoBranchCreationConfig where
  parseJSON =
    Data.withObject
      "AutoBranchCreationConfig"
      ( \x ->
          AutoBranchCreationConfig'
            Prelude.<$> (x Data..:? "basicAuthCredentials")
            Prelude.<*> (x Data..:? "buildSpec")
            Prelude.<*> (x Data..:? "enableAutoBuild")
            Prelude.<*> (x Data..:? "enableBasicAuth")
            Prelude.<*> (x Data..:? "enablePerformanceMode")
            Prelude.<*> (x Data..:? "enablePullRequestPreview")
            Prelude.<*> ( x Data..:? "environmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "framework")
            Prelude.<*> (x Data..:? "pullRequestEnvironmentName")
            Prelude.<*> (x Data..:? "stage")
      )

instance Prelude.Hashable AutoBranchCreationConfig where
  hashWithSalt _salt AutoBranchCreationConfig' {..} =
    _salt `Prelude.hashWithSalt` basicAuthCredentials
      `Prelude.hashWithSalt` buildSpec
      `Prelude.hashWithSalt` enableAutoBuild
      `Prelude.hashWithSalt` enableBasicAuth
      `Prelude.hashWithSalt` enablePerformanceMode
      `Prelude.hashWithSalt` enablePullRequestPreview
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` framework
      `Prelude.hashWithSalt` pullRequestEnvironmentName
      `Prelude.hashWithSalt` stage

instance Prelude.NFData AutoBranchCreationConfig where
  rnf AutoBranchCreationConfig' {..} =
    Prelude.rnf basicAuthCredentials
      `Prelude.seq` Prelude.rnf buildSpec
      `Prelude.seq` Prelude.rnf enableAutoBuild
      `Prelude.seq` Prelude.rnf enableBasicAuth
      `Prelude.seq` Prelude.rnf enablePerformanceMode
      `Prelude.seq` Prelude.rnf enablePullRequestPreview
      `Prelude.seq` Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf framework
      `Prelude.seq` Prelude.rnf pullRequestEnvironmentName
      `Prelude.seq` Prelude.rnf stage

instance Data.ToJSON AutoBranchCreationConfig where
  toJSON AutoBranchCreationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("basicAuthCredentials" Data..=)
              Prelude.<$> basicAuthCredentials,
            ("buildSpec" Data..=) Prelude.<$> buildSpec,
            ("enableAutoBuild" Data..=)
              Prelude.<$> enableAutoBuild,
            ("enableBasicAuth" Data..=)
              Prelude.<$> enableBasicAuth,
            ("enablePerformanceMode" Data..=)
              Prelude.<$> enablePerformanceMode,
            ("enablePullRequestPreview" Data..=)
              Prelude.<$> enablePullRequestPreview,
            ("environmentVariables" Data..=)
              Prelude.<$> environmentVariables,
            ("framework" Data..=) Prelude.<$> framework,
            ("pullRequestEnvironmentName" Data..=)
              Prelude.<$> pullRequestEnvironmentName,
            ("stage" Data..=) Prelude.<$> stage
          ]
      )
