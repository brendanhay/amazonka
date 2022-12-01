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
-- Module      : Amazonka.Amplify.Types.App
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.App where

import Amazonka.Amplify.Types.AutoBranchCreationConfig
import Amazonka.Amplify.Types.CustomRule
import Amazonka.Amplify.Types.Platform
import Amazonka.Amplify.Types.ProductionBranch
import Amazonka.Amplify.Types.RepositoryCloneMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the different branches of a repository for building,
-- deploying, and hosting an Amplify app.
--
-- /See:/ 'newApp' smart constructor.
data App = App'
  { -- | The tag for the Amplify app.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This is for internal use.
    --
    -- The Amplify service uses this parameter to specify the authentication
    -- protocol to use to access the Git repository for an Amplify app. Amplify
    -- specifies @TOKEN@ for a GitHub repository, @SIGV4@ for an Amazon Web
    -- Services CodeCommit repository, and @SSH@ for GitLab and Bitbucket
    -- repositories.
    repositoryCloneMethod :: Prelude.Maybe RepositoryCloneMethod,
    -- | The AWS Identity and Access Management (IAM) service role for the Amazon
    -- Resource Name (ARN) of the Amplify app.
    iamServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Describes the automated branch creation glob patterns for the Amplify
    -- app.
    autoBranchCreationPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Describes the custom HTTP headers for the Amplify app.
    customHeaders :: Prelude.Maybe Prelude.Text,
    -- | Automatically disconnect a branch in the Amplify Console when you delete
    -- a branch from your Git repository.
    enableBranchAutoDeletion :: Prelude.Maybe Prelude.Bool,
    -- | The basic authorization credentials for branches for the Amplify app.
    -- You must base64-encode the authorization credentials and provide them in
    -- the format @user:password@.
    basicAuthCredentials :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Describes the information about a production branch of the Amplify app.
    productionBranch :: Prelude.Maybe ProductionBranch,
    -- | Describes the custom redirect and rewrite rules for the Amplify app.
    customRules :: Prelude.Maybe [CustomRule],
    -- | Enables automated branch creation for the Amplify app.
    enableAutoBranchCreation :: Prelude.Maybe Prelude.Bool,
    -- | Describes the content of the build specification (build spec) for the
    -- Amplify app.
    buildSpec :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Describes the automated branch creation configuration for the Amplify
    -- app.
    autoBranchCreationConfig :: Prelude.Maybe AutoBranchCreationConfig,
    -- | The unique ID of the Amplify app.
    appId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amplify app.
    appArn :: Prelude.Text,
    -- | The name for the Amplify app.
    name :: Prelude.Text,
    -- | The description for the Amplify app.
    description :: Prelude.Text,
    -- | The Git repository for the Amplify app.
    repository :: Prelude.Text,
    -- | The platform for the Amplify app. For a static app, set the platform
    -- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
    -- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
    -- original SSR support only, set the platform type to @WEB_DYNAMIC@.
    platform :: Platform,
    -- | Creates a date and time for the Amplify app.
    createTime :: Core.POSIX,
    -- | Updates the date and time for the Amplify app.
    updateTime :: Core.POSIX,
    -- | The environment variables for the Amplify app.
    environmentVariables :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The default domain for the Amplify app.
    defaultDomain :: Prelude.Text,
    -- | Enables the auto-building of branches for the Amplify app.
    enableBranchAutoBuild :: Prelude.Bool,
    -- | Enables basic authorization for the Amplify app\'s branches.
    enableBasicAuth :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'App' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'app_tags' - The tag for the Amplify app.
--
-- 'repositoryCloneMethod', 'app_repositoryCloneMethod' - This is for internal use.
--
-- The Amplify service uses this parameter to specify the authentication
-- protocol to use to access the Git repository for an Amplify app. Amplify
-- specifies @TOKEN@ for a GitHub repository, @SIGV4@ for an Amazon Web
-- Services CodeCommit repository, and @SSH@ for GitLab and Bitbucket
-- repositories.
--
-- 'iamServiceRoleArn', 'app_iamServiceRoleArn' - The AWS Identity and Access Management (IAM) service role for the Amazon
-- Resource Name (ARN) of the Amplify app.
--
-- 'autoBranchCreationPatterns', 'app_autoBranchCreationPatterns' - Describes the automated branch creation glob patterns for the Amplify
-- app.
--
-- 'customHeaders', 'app_customHeaders' - Describes the custom HTTP headers for the Amplify app.
--
-- 'enableBranchAutoDeletion', 'app_enableBranchAutoDeletion' - Automatically disconnect a branch in the Amplify Console when you delete
-- a branch from your Git repository.
--
-- 'basicAuthCredentials', 'app_basicAuthCredentials' - The basic authorization credentials for branches for the Amplify app.
-- You must base64-encode the authorization credentials and provide them in
-- the format @user:password@.
--
-- 'productionBranch', 'app_productionBranch' - Describes the information about a production branch of the Amplify app.
--
-- 'customRules', 'app_customRules' - Describes the custom redirect and rewrite rules for the Amplify app.
--
-- 'enableAutoBranchCreation', 'app_enableAutoBranchCreation' - Enables automated branch creation for the Amplify app.
--
-- 'buildSpec', 'app_buildSpec' - Describes the content of the build specification (build spec) for the
-- Amplify app.
--
-- 'autoBranchCreationConfig', 'app_autoBranchCreationConfig' - Describes the automated branch creation configuration for the Amplify
-- app.
--
-- 'appId', 'app_appId' - The unique ID of the Amplify app.
--
-- 'appArn', 'app_appArn' - The Amazon Resource Name (ARN) of the Amplify app.
--
-- 'name', 'app_name' - The name for the Amplify app.
--
-- 'description', 'app_description' - The description for the Amplify app.
--
-- 'repository', 'app_repository' - The Git repository for the Amplify app.
--
-- 'platform', 'app_platform' - The platform for the Amplify app. For a static app, set the platform
-- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
-- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
-- original SSR support only, set the platform type to @WEB_DYNAMIC@.
--
-- 'createTime', 'app_createTime' - Creates a date and time for the Amplify app.
--
-- 'updateTime', 'app_updateTime' - Updates the date and time for the Amplify app.
--
-- 'environmentVariables', 'app_environmentVariables' - The environment variables for the Amplify app.
--
-- 'defaultDomain', 'app_defaultDomain' - The default domain for the Amplify app.
--
-- 'enableBranchAutoBuild', 'app_enableBranchAutoBuild' - Enables the auto-building of branches for the Amplify app.
--
-- 'enableBasicAuth', 'app_enableBasicAuth' - Enables basic authorization for the Amplify app\'s branches.
newApp ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'platform'
  Platform ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'defaultDomain'
  Prelude.Text ->
  -- | 'enableBranchAutoBuild'
  Prelude.Bool ->
  -- | 'enableBasicAuth'
  Prelude.Bool ->
  App
newApp
  pAppId_
  pAppArn_
  pName_
  pDescription_
  pRepository_
  pPlatform_
  pCreateTime_
  pUpdateTime_
  pDefaultDomain_
  pEnableBranchAutoBuild_
  pEnableBasicAuth_ =
    App'
      { tags = Prelude.Nothing,
        repositoryCloneMethod = Prelude.Nothing,
        iamServiceRoleArn = Prelude.Nothing,
        autoBranchCreationPatterns = Prelude.Nothing,
        customHeaders = Prelude.Nothing,
        enableBranchAutoDeletion = Prelude.Nothing,
        basicAuthCredentials = Prelude.Nothing,
        productionBranch = Prelude.Nothing,
        customRules = Prelude.Nothing,
        enableAutoBranchCreation = Prelude.Nothing,
        buildSpec = Prelude.Nothing,
        autoBranchCreationConfig = Prelude.Nothing,
        appId = pAppId_,
        appArn = pAppArn_,
        name = pName_,
        description = pDescription_,
        repository = pRepository_,
        platform = pPlatform_,
        createTime = Core._Time Lens.# pCreateTime_,
        updateTime = Core._Time Lens.# pUpdateTime_,
        environmentVariables = Prelude.mempty,
        defaultDomain = pDefaultDomain_,
        enableBranchAutoBuild = pEnableBranchAutoBuild_,
        enableBasicAuth = pEnableBasicAuth_
      }

-- | The tag for the Amplify app.
app_tags :: Lens.Lens' App (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
app_tags = Lens.lens (\App' {tags} -> tags) (\s@App' {} a -> s {tags = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | This is for internal use.
--
-- The Amplify service uses this parameter to specify the authentication
-- protocol to use to access the Git repository for an Amplify app. Amplify
-- specifies @TOKEN@ for a GitHub repository, @SIGV4@ for an Amazon Web
-- Services CodeCommit repository, and @SSH@ for GitLab and Bitbucket
-- repositories.
app_repositoryCloneMethod :: Lens.Lens' App (Prelude.Maybe RepositoryCloneMethod)
app_repositoryCloneMethod = Lens.lens (\App' {repositoryCloneMethod} -> repositoryCloneMethod) (\s@App' {} a -> s {repositoryCloneMethod = a} :: App)

-- | The AWS Identity and Access Management (IAM) service role for the Amazon
-- Resource Name (ARN) of the Amplify app.
app_iamServiceRoleArn :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_iamServiceRoleArn = Lens.lens (\App' {iamServiceRoleArn} -> iamServiceRoleArn) (\s@App' {} a -> s {iamServiceRoleArn = a} :: App)

-- | Describes the automated branch creation glob patterns for the Amplify
-- app.
app_autoBranchCreationPatterns :: Lens.Lens' App (Prelude.Maybe [Prelude.Text])
app_autoBranchCreationPatterns = Lens.lens (\App' {autoBranchCreationPatterns} -> autoBranchCreationPatterns) (\s@App' {} a -> s {autoBranchCreationPatterns = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | Describes the custom HTTP headers for the Amplify app.
app_customHeaders :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_customHeaders = Lens.lens (\App' {customHeaders} -> customHeaders) (\s@App' {} a -> s {customHeaders = a} :: App)

-- | Automatically disconnect a branch in the Amplify Console when you delete
-- a branch from your Git repository.
app_enableBranchAutoDeletion :: Lens.Lens' App (Prelude.Maybe Prelude.Bool)
app_enableBranchAutoDeletion = Lens.lens (\App' {enableBranchAutoDeletion} -> enableBranchAutoDeletion) (\s@App' {} a -> s {enableBranchAutoDeletion = a} :: App)

-- | The basic authorization credentials for branches for the Amplify app.
-- You must base64-encode the authorization credentials and provide them in
-- the format @user:password@.
app_basicAuthCredentials :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_basicAuthCredentials = Lens.lens (\App' {basicAuthCredentials} -> basicAuthCredentials) (\s@App' {} a -> s {basicAuthCredentials = a} :: App) Prelude.. Lens.mapping Core._Sensitive

-- | Describes the information about a production branch of the Amplify app.
app_productionBranch :: Lens.Lens' App (Prelude.Maybe ProductionBranch)
app_productionBranch = Lens.lens (\App' {productionBranch} -> productionBranch) (\s@App' {} a -> s {productionBranch = a} :: App)

-- | Describes the custom redirect and rewrite rules for the Amplify app.
app_customRules :: Lens.Lens' App (Prelude.Maybe [CustomRule])
app_customRules = Lens.lens (\App' {customRules} -> customRules) (\s@App' {} a -> s {customRules = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | Enables automated branch creation for the Amplify app.
app_enableAutoBranchCreation :: Lens.Lens' App (Prelude.Maybe Prelude.Bool)
app_enableAutoBranchCreation = Lens.lens (\App' {enableAutoBranchCreation} -> enableAutoBranchCreation) (\s@App' {} a -> s {enableAutoBranchCreation = a} :: App)

-- | Describes the content of the build specification (build spec) for the
-- Amplify app.
app_buildSpec :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_buildSpec = Lens.lens (\App' {buildSpec} -> buildSpec) (\s@App' {} a -> s {buildSpec = a} :: App) Prelude.. Lens.mapping Core._Sensitive

-- | Describes the automated branch creation configuration for the Amplify
-- app.
app_autoBranchCreationConfig :: Lens.Lens' App (Prelude.Maybe AutoBranchCreationConfig)
app_autoBranchCreationConfig = Lens.lens (\App' {autoBranchCreationConfig} -> autoBranchCreationConfig) (\s@App' {} a -> s {autoBranchCreationConfig = a} :: App)

-- | The unique ID of the Amplify app.
app_appId :: Lens.Lens' App Prelude.Text
app_appId = Lens.lens (\App' {appId} -> appId) (\s@App' {} a -> s {appId = a} :: App)

-- | The Amazon Resource Name (ARN) of the Amplify app.
app_appArn :: Lens.Lens' App Prelude.Text
app_appArn = Lens.lens (\App' {appArn} -> appArn) (\s@App' {} a -> s {appArn = a} :: App)

-- | The name for the Amplify app.
app_name :: Lens.Lens' App Prelude.Text
app_name = Lens.lens (\App' {name} -> name) (\s@App' {} a -> s {name = a} :: App)

-- | The description for the Amplify app.
app_description :: Lens.Lens' App Prelude.Text
app_description = Lens.lens (\App' {description} -> description) (\s@App' {} a -> s {description = a} :: App)

-- | The Git repository for the Amplify app.
app_repository :: Lens.Lens' App Prelude.Text
app_repository = Lens.lens (\App' {repository} -> repository) (\s@App' {} a -> s {repository = a} :: App)

-- | The platform for the Amplify app. For a static app, set the platform
-- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
-- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
-- original SSR support only, set the platform type to @WEB_DYNAMIC@.
app_platform :: Lens.Lens' App Platform
app_platform = Lens.lens (\App' {platform} -> platform) (\s@App' {} a -> s {platform = a} :: App)

-- | Creates a date and time for the Amplify app.
app_createTime :: Lens.Lens' App Prelude.UTCTime
app_createTime = Lens.lens (\App' {createTime} -> createTime) (\s@App' {} a -> s {createTime = a} :: App) Prelude.. Core._Time

-- | Updates the date and time for the Amplify app.
app_updateTime :: Lens.Lens' App Prelude.UTCTime
app_updateTime = Lens.lens (\App' {updateTime} -> updateTime) (\s@App' {} a -> s {updateTime = a} :: App) Prelude.. Core._Time

-- | The environment variables for the Amplify app.
app_environmentVariables :: Lens.Lens' App (Prelude.HashMap Prelude.Text Prelude.Text)
app_environmentVariables = Lens.lens (\App' {environmentVariables} -> environmentVariables) (\s@App' {} a -> s {environmentVariables = a} :: App) Prelude.. Lens.coerced

-- | The default domain for the Amplify app.
app_defaultDomain :: Lens.Lens' App Prelude.Text
app_defaultDomain = Lens.lens (\App' {defaultDomain} -> defaultDomain) (\s@App' {} a -> s {defaultDomain = a} :: App)

-- | Enables the auto-building of branches for the Amplify app.
app_enableBranchAutoBuild :: Lens.Lens' App Prelude.Bool
app_enableBranchAutoBuild = Lens.lens (\App' {enableBranchAutoBuild} -> enableBranchAutoBuild) (\s@App' {} a -> s {enableBranchAutoBuild = a} :: App)

-- | Enables basic authorization for the Amplify app\'s branches.
app_enableBasicAuth :: Lens.Lens' App Prelude.Bool
app_enableBasicAuth = Lens.lens (\App' {enableBasicAuth} -> enableBasicAuth) (\s@App' {} a -> s {enableBasicAuth = a} :: App)

instance Core.FromJSON App where
  parseJSON =
    Core.withObject
      "App"
      ( \x ->
          App'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "repositoryCloneMethod")
            Prelude.<*> (x Core..:? "iamServiceRoleArn")
            Prelude.<*> ( x Core..:? "autoBranchCreationPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "customHeaders")
            Prelude.<*> (x Core..:? "enableBranchAutoDeletion")
            Prelude.<*> (x Core..:? "basicAuthCredentials")
            Prelude.<*> (x Core..:? "productionBranch")
            Prelude.<*> (x Core..:? "customRules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "enableAutoBranchCreation")
            Prelude.<*> (x Core..:? "buildSpec")
            Prelude.<*> (x Core..:? "autoBranchCreationConfig")
            Prelude.<*> (x Core..: "appId")
            Prelude.<*> (x Core..: "appArn")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "description")
            Prelude.<*> (x Core..: "repository")
            Prelude.<*> (x Core..: "platform")
            Prelude.<*> (x Core..: "createTime")
            Prelude.<*> (x Core..: "updateTime")
            Prelude.<*> ( x Core..:? "environmentVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "defaultDomain")
            Prelude.<*> (x Core..: "enableBranchAutoBuild")
            Prelude.<*> (x Core..: "enableBasicAuth")
      )

instance Prelude.Hashable App where
  hashWithSalt _salt App' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` repositoryCloneMethod
      `Prelude.hashWithSalt` iamServiceRoleArn
      `Prelude.hashWithSalt` autoBranchCreationPatterns
      `Prelude.hashWithSalt` customHeaders
      `Prelude.hashWithSalt` enableBranchAutoDeletion
      `Prelude.hashWithSalt` basicAuthCredentials
      `Prelude.hashWithSalt` productionBranch
      `Prelude.hashWithSalt` customRules
      `Prelude.hashWithSalt` enableAutoBranchCreation
      `Prelude.hashWithSalt` buildSpec
      `Prelude.hashWithSalt` autoBranchCreationConfig
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` defaultDomain
      `Prelude.hashWithSalt` enableBranchAutoBuild
      `Prelude.hashWithSalt` enableBasicAuth

instance Prelude.NFData App where
  rnf App' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf repositoryCloneMethod
      `Prelude.seq` Prelude.rnf iamServiceRoleArn
      `Prelude.seq` Prelude.rnf autoBranchCreationPatterns
      `Prelude.seq` Prelude.rnf customHeaders
      `Prelude.seq` Prelude.rnf enableBranchAutoDeletion
      `Prelude.seq` Prelude.rnf basicAuthCredentials
      `Prelude.seq` Prelude.rnf productionBranch
      `Prelude.seq` Prelude.rnf customRules
      `Prelude.seq` Prelude.rnf enableAutoBranchCreation
      `Prelude.seq` Prelude.rnf buildSpec
      `Prelude.seq` Prelude.rnf autoBranchCreationConfig
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf
        environmentVariables
      `Prelude.seq` Prelude.rnf defaultDomain
      `Prelude.seq` Prelude.rnf
        enableBranchAutoBuild
      `Prelude.seq` Prelude.rnf
        enableBasicAuth
