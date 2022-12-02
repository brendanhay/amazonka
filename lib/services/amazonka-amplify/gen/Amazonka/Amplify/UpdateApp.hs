{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Amplify.UpdateApp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amplify app.
module Amazonka.Amplify.UpdateApp
  ( -- * Creating a Request
    UpdateApp (..),
    newUpdateApp,

    -- * Request Lenses
    updateApp_iamServiceRoleArn,
    updateApp_accessToken,
    updateApp_name,
    updateApp_autoBranchCreationPatterns,
    updateApp_customHeaders,
    updateApp_enableBranchAutoBuild,
    updateApp_repository,
    updateApp_enableBranchAutoDeletion,
    updateApp_basicAuthCredentials,
    updateApp_description,
    updateApp_platform,
    updateApp_oauthToken,
    updateApp_environmentVariables,
    updateApp_customRules,
    updateApp_enableBasicAuth,
    updateApp_enableAutoBranchCreation,
    updateApp_buildSpec,
    updateApp_autoBranchCreationConfig,
    updateApp_appId,

    -- * Destructuring the Response
    UpdateAppResponse (..),
    newUpdateAppResponse,

    -- * Response Lenses
    updateAppResponse_httpStatus,
    updateAppResponse_app,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the update app request.
--
-- /See:/ 'newUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | The AWS Identity and Access Management (IAM) service role for an Amplify
    -- app.
    iamServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The personal access token for a GitHub repository for an Amplify app.
    -- The personal access token is used to authorize access to a GitHub
    -- repository using the Amplify GitHub App. The token is not stored.
    --
    -- Use @accessToken@ for GitHub repositories only. To authorize access to a
    -- repository provider such as Bitbucket or CodeCommit, use @oauthToken@.
    --
    -- You must specify either @accessToken@ or @oauthToken@ when you update an
    -- app.
    --
    -- Existing Amplify apps deployed from a GitHub repository using OAuth
    -- continue to work with CI\/CD. However, we strongly recommend that you
    -- migrate these apps to use the GitHub App. For more information, see
    -- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
    -- in the /Amplify User Guide/ .
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name for an Amplify app.
    name :: Prelude.Maybe Prelude.Text,
    -- | Describes the automated branch creation glob patterns for an Amplify
    -- app.
    autoBranchCreationPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The custom HTTP headers for an Amplify app.
    customHeaders :: Prelude.Maybe Prelude.Text,
    -- | Enables branch auto-building for an Amplify app.
    enableBranchAutoBuild :: Prelude.Maybe Prelude.Bool,
    -- | The name of the repository for an Amplify app
    repository :: Prelude.Maybe Prelude.Text,
    -- | Automatically disconnects a branch in the Amplify Console when you
    -- delete a branch from your Git repository.
    enableBranchAutoDeletion :: Prelude.Maybe Prelude.Bool,
    -- | The basic authorization credentials for an Amplify app. You must
    -- base64-encode the authorization credentials and provide them in the
    -- format @user:password@.
    basicAuthCredentials :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The description for an Amplify app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The platform for the Amplify app. For a static app, set the platform
    -- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
    -- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
    -- original SSR support only, set the platform type to @WEB_DYNAMIC@.
    platform :: Prelude.Maybe Platform,
    -- | The OAuth token for a third-party source control system for an Amplify
    -- app. The OAuth token is used to create a webhook and a read-only deploy
    -- key using SSH cloning. The OAuth token is not stored.
    --
    -- Use @oauthToken@ for repository providers other than GitHub, such as
    -- Bitbucket or CodeCommit.
    --
    -- To authorize access to GitHub as your repository provider, use
    -- @accessToken@.
    --
    -- You must specify either @oauthToken@ or @accessToken@ when you update an
    -- app.
    --
    -- Existing Amplify apps deployed from a GitHub repository using OAuth
    -- continue to work with CI\/CD. However, we strongly recommend that you
    -- migrate these apps to use the GitHub App. For more information, see
    -- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
    -- in the /Amplify User Guide/ .
    oauthToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The environment variables for an Amplify app.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The custom redirect and rewrite rules for an Amplify app.
    customRules :: Prelude.Maybe [CustomRule],
    -- | Enables basic authorization for an Amplify app.
    enableBasicAuth :: Prelude.Maybe Prelude.Bool,
    -- | Enables automated branch creation for an Amplify app.
    enableAutoBranchCreation :: Prelude.Maybe Prelude.Bool,
    -- | The build specification (build spec) for an Amplify app.
    buildSpec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The automated branch creation configuration for an Amplify app.
    autoBranchCreationConfig :: Prelude.Maybe AutoBranchCreationConfig,
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamServiceRoleArn', 'updateApp_iamServiceRoleArn' - The AWS Identity and Access Management (IAM) service role for an Amplify
-- app.
--
-- 'accessToken', 'updateApp_accessToken' - The personal access token for a GitHub repository for an Amplify app.
-- The personal access token is used to authorize access to a GitHub
-- repository using the Amplify GitHub App. The token is not stored.
--
-- Use @accessToken@ for GitHub repositories only. To authorize access to a
-- repository provider such as Bitbucket or CodeCommit, use @oauthToken@.
--
-- You must specify either @accessToken@ or @oauthToken@ when you update an
-- app.
--
-- Existing Amplify apps deployed from a GitHub repository using OAuth
-- continue to work with CI\/CD. However, we strongly recommend that you
-- migrate these apps to use the GitHub App. For more information, see
-- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
-- in the /Amplify User Guide/ .
--
-- 'name', 'updateApp_name' - The name for an Amplify app.
--
-- 'autoBranchCreationPatterns', 'updateApp_autoBranchCreationPatterns' - Describes the automated branch creation glob patterns for an Amplify
-- app.
--
-- 'customHeaders', 'updateApp_customHeaders' - The custom HTTP headers for an Amplify app.
--
-- 'enableBranchAutoBuild', 'updateApp_enableBranchAutoBuild' - Enables branch auto-building for an Amplify app.
--
-- 'repository', 'updateApp_repository' - The name of the repository for an Amplify app
--
-- 'enableBranchAutoDeletion', 'updateApp_enableBranchAutoDeletion' - Automatically disconnects a branch in the Amplify Console when you
-- delete a branch from your Git repository.
--
-- 'basicAuthCredentials', 'updateApp_basicAuthCredentials' - The basic authorization credentials for an Amplify app. You must
-- base64-encode the authorization credentials and provide them in the
-- format @user:password@.
--
-- 'description', 'updateApp_description' - The description for an Amplify app.
--
-- 'platform', 'updateApp_platform' - The platform for the Amplify app. For a static app, set the platform
-- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
-- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
-- original SSR support only, set the platform type to @WEB_DYNAMIC@.
--
-- 'oauthToken', 'updateApp_oauthToken' - The OAuth token for a third-party source control system for an Amplify
-- app. The OAuth token is used to create a webhook and a read-only deploy
-- key using SSH cloning. The OAuth token is not stored.
--
-- Use @oauthToken@ for repository providers other than GitHub, such as
-- Bitbucket or CodeCommit.
--
-- To authorize access to GitHub as your repository provider, use
-- @accessToken@.
--
-- You must specify either @oauthToken@ or @accessToken@ when you update an
-- app.
--
-- Existing Amplify apps deployed from a GitHub repository using OAuth
-- continue to work with CI\/CD. However, we strongly recommend that you
-- migrate these apps to use the GitHub App. For more information, see
-- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
-- in the /Amplify User Guide/ .
--
-- 'environmentVariables', 'updateApp_environmentVariables' - The environment variables for an Amplify app.
--
-- 'customRules', 'updateApp_customRules' - The custom redirect and rewrite rules for an Amplify app.
--
-- 'enableBasicAuth', 'updateApp_enableBasicAuth' - Enables basic authorization for an Amplify app.
--
-- 'enableAutoBranchCreation', 'updateApp_enableAutoBranchCreation' - Enables automated branch creation for an Amplify app.
--
-- 'buildSpec', 'updateApp_buildSpec' - The build specification (build spec) for an Amplify app.
--
-- 'autoBranchCreationConfig', 'updateApp_autoBranchCreationConfig' - The automated branch creation configuration for an Amplify app.
--
-- 'appId', 'updateApp_appId' - The unique ID for an Amplify app.
newUpdateApp ::
  -- | 'appId'
  Prelude.Text ->
  UpdateApp
newUpdateApp pAppId_ =
  UpdateApp'
    { iamServiceRoleArn = Prelude.Nothing,
      accessToken = Prelude.Nothing,
      name = Prelude.Nothing,
      autoBranchCreationPatterns = Prelude.Nothing,
      customHeaders = Prelude.Nothing,
      enableBranchAutoBuild = Prelude.Nothing,
      repository = Prelude.Nothing,
      enableBranchAutoDeletion = Prelude.Nothing,
      basicAuthCredentials = Prelude.Nothing,
      description = Prelude.Nothing,
      platform = Prelude.Nothing,
      oauthToken = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      customRules = Prelude.Nothing,
      enableBasicAuth = Prelude.Nothing,
      enableAutoBranchCreation = Prelude.Nothing,
      buildSpec = Prelude.Nothing,
      autoBranchCreationConfig = Prelude.Nothing,
      appId = pAppId_
    }

-- | The AWS Identity and Access Management (IAM) service role for an Amplify
-- app.
updateApp_iamServiceRoleArn :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_iamServiceRoleArn = Lens.lens (\UpdateApp' {iamServiceRoleArn} -> iamServiceRoleArn) (\s@UpdateApp' {} a -> s {iamServiceRoleArn = a} :: UpdateApp)

-- | The personal access token for a GitHub repository for an Amplify app.
-- The personal access token is used to authorize access to a GitHub
-- repository using the Amplify GitHub App. The token is not stored.
--
-- Use @accessToken@ for GitHub repositories only. To authorize access to a
-- repository provider such as Bitbucket or CodeCommit, use @oauthToken@.
--
-- You must specify either @accessToken@ or @oauthToken@ when you update an
-- app.
--
-- Existing Amplify apps deployed from a GitHub repository using OAuth
-- continue to work with CI\/CD. However, we strongly recommend that you
-- migrate these apps to use the GitHub App. For more information, see
-- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
-- in the /Amplify User Guide/ .
updateApp_accessToken :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_accessToken = Lens.lens (\UpdateApp' {accessToken} -> accessToken) (\s@UpdateApp' {} a -> s {accessToken = a} :: UpdateApp) Prelude.. Lens.mapping Data._Sensitive

-- | The name for an Amplify app.
updateApp_name :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_name = Lens.lens (\UpdateApp' {name} -> name) (\s@UpdateApp' {} a -> s {name = a} :: UpdateApp)

-- | Describes the automated branch creation glob patterns for an Amplify
-- app.
updateApp_autoBranchCreationPatterns :: Lens.Lens' UpdateApp (Prelude.Maybe [Prelude.Text])
updateApp_autoBranchCreationPatterns = Lens.lens (\UpdateApp' {autoBranchCreationPatterns} -> autoBranchCreationPatterns) (\s@UpdateApp' {} a -> s {autoBranchCreationPatterns = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | The custom HTTP headers for an Amplify app.
updateApp_customHeaders :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_customHeaders = Lens.lens (\UpdateApp' {customHeaders} -> customHeaders) (\s@UpdateApp' {} a -> s {customHeaders = a} :: UpdateApp)

-- | Enables branch auto-building for an Amplify app.
updateApp_enableBranchAutoBuild :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableBranchAutoBuild = Lens.lens (\UpdateApp' {enableBranchAutoBuild} -> enableBranchAutoBuild) (\s@UpdateApp' {} a -> s {enableBranchAutoBuild = a} :: UpdateApp)

-- | The name of the repository for an Amplify app
updateApp_repository :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_repository = Lens.lens (\UpdateApp' {repository} -> repository) (\s@UpdateApp' {} a -> s {repository = a} :: UpdateApp)

-- | Automatically disconnects a branch in the Amplify Console when you
-- delete a branch from your Git repository.
updateApp_enableBranchAutoDeletion :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableBranchAutoDeletion = Lens.lens (\UpdateApp' {enableBranchAutoDeletion} -> enableBranchAutoDeletion) (\s@UpdateApp' {} a -> s {enableBranchAutoDeletion = a} :: UpdateApp)

-- | The basic authorization credentials for an Amplify app. You must
-- base64-encode the authorization credentials and provide them in the
-- format @user:password@.
updateApp_basicAuthCredentials :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_basicAuthCredentials = Lens.lens (\UpdateApp' {basicAuthCredentials} -> basicAuthCredentials) (\s@UpdateApp' {} a -> s {basicAuthCredentials = a} :: UpdateApp) Prelude.. Lens.mapping Data._Sensitive

-- | The description for an Amplify app.
updateApp_description :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_description = Lens.lens (\UpdateApp' {description} -> description) (\s@UpdateApp' {} a -> s {description = a} :: UpdateApp)

-- | The platform for the Amplify app. For a static app, set the platform
-- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
-- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
-- original SSR support only, set the platform type to @WEB_DYNAMIC@.
updateApp_platform :: Lens.Lens' UpdateApp (Prelude.Maybe Platform)
updateApp_platform = Lens.lens (\UpdateApp' {platform} -> platform) (\s@UpdateApp' {} a -> s {platform = a} :: UpdateApp)

-- | The OAuth token for a third-party source control system for an Amplify
-- app. The OAuth token is used to create a webhook and a read-only deploy
-- key using SSH cloning. The OAuth token is not stored.
--
-- Use @oauthToken@ for repository providers other than GitHub, such as
-- Bitbucket or CodeCommit.
--
-- To authorize access to GitHub as your repository provider, use
-- @accessToken@.
--
-- You must specify either @oauthToken@ or @accessToken@ when you update an
-- app.
--
-- Existing Amplify apps deployed from a GitHub repository using OAuth
-- continue to work with CI\/CD. However, we strongly recommend that you
-- migrate these apps to use the GitHub App. For more information, see
-- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
-- in the /Amplify User Guide/ .
updateApp_oauthToken :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_oauthToken = Lens.lens (\UpdateApp' {oauthToken} -> oauthToken) (\s@UpdateApp' {} a -> s {oauthToken = a} :: UpdateApp) Prelude.. Lens.mapping Data._Sensitive

-- | The environment variables for an Amplify app.
updateApp_environmentVariables :: Lens.Lens' UpdateApp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateApp_environmentVariables = Lens.lens (\UpdateApp' {environmentVariables} -> environmentVariables) (\s@UpdateApp' {} a -> s {environmentVariables = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | The custom redirect and rewrite rules for an Amplify app.
updateApp_customRules :: Lens.Lens' UpdateApp (Prelude.Maybe [CustomRule])
updateApp_customRules = Lens.lens (\UpdateApp' {customRules} -> customRules) (\s@UpdateApp' {} a -> s {customRules = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | Enables basic authorization for an Amplify app.
updateApp_enableBasicAuth :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableBasicAuth = Lens.lens (\UpdateApp' {enableBasicAuth} -> enableBasicAuth) (\s@UpdateApp' {} a -> s {enableBasicAuth = a} :: UpdateApp)

-- | Enables automated branch creation for an Amplify app.
updateApp_enableAutoBranchCreation :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableAutoBranchCreation = Lens.lens (\UpdateApp' {enableAutoBranchCreation} -> enableAutoBranchCreation) (\s@UpdateApp' {} a -> s {enableAutoBranchCreation = a} :: UpdateApp)

-- | The build specification (build spec) for an Amplify app.
updateApp_buildSpec :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_buildSpec = Lens.lens (\UpdateApp' {buildSpec} -> buildSpec) (\s@UpdateApp' {} a -> s {buildSpec = a} :: UpdateApp) Prelude.. Lens.mapping Data._Sensitive

-- | The automated branch creation configuration for an Amplify app.
updateApp_autoBranchCreationConfig :: Lens.Lens' UpdateApp (Prelude.Maybe AutoBranchCreationConfig)
updateApp_autoBranchCreationConfig = Lens.lens (\UpdateApp' {autoBranchCreationConfig} -> autoBranchCreationConfig) (\s@UpdateApp' {} a -> s {autoBranchCreationConfig = a} :: UpdateApp)

-- | The unique ID for an Amplify app.
updateApp_appId :: Lens.Lens' UpdateApp Prelude.Text
updateApp_appId = Lens.lens (\UpdateApp' {appId} -> appId) (\s@UpdateApp' {} a -> s {appId = a} :: UpdateApp)

instance Core.AWSRequest UpdateApp where
  type AWSResponse UpdateApp = UpdateAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "app")
      )

instance Prelude.Hashable UpdateApp where
  hashWithSalt _salt UpdateApp' {..} =
    _salt `Prelude.hashWithSalt` iamServiceRoleArn
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` autoBranchCreationPatterns
      `Prelude.hashWithSalt` customHeaders
      `Prelude.hashWithSalt` enableBranchAutoBuild
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` enableBranchAutoDeletion
      `Prelude.hashWithSalt` basicAuthCredentials
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` oauthToken
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` customRules
      `Prelude.hashWithSalt` enableBasicAuth
      `Prelude.hashWithSalt` enableAutoBranchCreation
      `Prelude.hashWithSalt` buildSpec
      `Prelude.hashWithSalt` autoBranchCreationConfig
      `Prelude.hashWithSalt` appId

instance Prelude.NFData UpdateApp where
  rnf UpdateApp' {..} =
    Prelude.rnf iamServiceRoleArn
      `Prelude.seq` Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf autoBranchCreationPatterns
      `Prelude.seq` Prelude.rnf customHeaders
      `Prelude.seq` Prelude.rnf enableBranchAutoBuild
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf enableBranchAutoDeletion
      `Prelude.seq` Prelude.rnf basicAuthCredentials
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf oauthToken
      `Prelude.seq` Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf customRules
      `Prelude.seq` Prelude.rnf enableBasicAuth
      `Prelude.seq` Prelude.rnf enableAutoBranchCreation
      `Prelude.seq` Prelude.rnf buildSpec
      `Prelude.seq` Prelude.rnf
        autoBranchCreationConfig
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders UpdateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iamServiceRoleArn" Data..=)
              Prelude.<$> iamServiceRoleArn,
            ("accessToken" Data..=) Prelude.<$> accessToken,
            ("name" Data..=) Prelude.<$> name,
            ("autoBranchCreationPatterns" Data..=)
              Prelude.<$> autoBranchCreationPatterns,
            ("customHeaders" Data..=) Prelude.<$> customHeaders,
            ("enableBranchAutoBuild" Data..=)
              Prelude.<$> enableBranchAutoBuild,
            ("repository" Data..=) Prelude.<$> repository,
            ("enableBranchAutoDeletion" Data..=)
              Prelude.<$> enableBranchAutoDeletion,
            ("basicAuthCredentials" Data..=)
              Prelude.<$> basicAuthCredentials,
            ("description" Data..=) Prelude.<$> description,
            ("platform" Data..=) Prelude.<$> platform,
            ("oauthToken" Data..=) Prelude.<$> oauthToken,
            ("environmentVariables" Data..=)
              Prelude.<$> environmentVariables,
            ("customRules" Data..=) Prelude.<$> customRules,
            ("enableBasicAuth" Data..=)
              Prelude.<$> enableBasicAuth,
            ("enableAutoBranchCreation" Data..=)
              Prelude.<$> enableAutoBranchCreation,
            ("buildSpec" Data..=) Prelude.<$> buildSpec,
            ("autoBranchCreationConfig" Data..=)
              Prelude.<$> autoBranchCreationConfig
          ]
      )

instance Data.ToPath UpdateApp where
  toPath UpdateApp' {..} =
    Prelude.mconcat ["/apps/", Data.toBS appId]

instance Data.ToQuery UpdateApp where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for an Amplify app update request.
--
-- /See:/ 'newUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Represents the updated Amplify app.
    app :: App
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAppResponse_httpStatus' - The response's http status code.
--
-- 'app', 'updateAppResponse_app' - Represents the updated Amplify app.
newUpdateAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'app'
  App ->
  UpdateAppResponse
newUpdateAppResponse pHttpStatus_ pApp_ =
  UpdateAppResponse'
    { httpStatus = pHttpStatus_,
      app = pApp_
    }

-- | The response's http status code.
updateAppResponse_httpStatus :: Lens.Lens' UpdateAppResponse Prelude.Int
updateAppResponse_httpStatus = Lens.lens (\UpdateAppResponse' {httpStatus} -> httpStatus) (\s@UpdateAppResponse' {} a -> s {httpStatus = a} :: UpdateAppResponse)

-- | Represents the updated Amplify app.
updateAppResponse_app :: Lens.Lens' UpdateAppResponse App
updateAppResponse_app = Lens.lens (\UpdateAppResponse' {app} -> app) (\s@UpdateAppResponse' {} a -> s {app = a} :: UpdateAppResponse)

instance Prelude.NFData UpdateAppResponse where
  rnf UpdateAppResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf app
