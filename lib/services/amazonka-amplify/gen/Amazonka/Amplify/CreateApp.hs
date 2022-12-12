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
-- Module      : Amazonka.Amplify.CreateApp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amplify app.
module Amazonka.Amplify.CreateApp
  ( -- * Creating a Request
    CreateApp (..),
    newCreateApp,

    -- * Request Lenses
    createApp_accessToken,
    createApp_autoBranchCreationConfig,
    createApp_autoBranchCreationPatterns,
    createApp_basicAuthCredentials,
    createApp_buildSpec,
    createApp_customHeaders,
    createApp_customRules,
    createApp_description,
    createApp_enableAutoBranchCreation,
    createApp_enableBasicAuth,
    createApp_enableBranchAutoBuild,
    createApp_enableBranchAutoDeletion,
    createApp_environmentVariables,
    createApp_iamServiceRoleArn,
    createApp_oauthToken,
    createApp_platform,
    createApp_repository,
    createApp_tags,
    createApp_name,

    -- * Destructuring the Response
    CreateAppResponse (..),
    newCreateAppResponse,

    -- * Response Lenses
    createAppResponse_httpStatus,
    createAppResponse_app,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure used to create apps in Amplify.
--
-- /See:/ 'newCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | The personal access token for a GitHub repository for an Amplify app.
    -- The personal access token is used to authorize access to a GitHub
    -- repository using the Amplify GitHub App. The token is not stored.
    --
    -- Use @accessToken@ for GitHub repositories only. To authorize access to a
    -- repository provider such as Bitbucket or CodeCommit, use @oauthToken@.
    --
    -- You must specify either @accessToken@ or @oauthToken@ when you create a
    -- new app.
    --
    -- Existing Amplify apps deployed from a GitHub repository using OAuth
    -- continue to work with CI\/CD. However, we strongly recommend that you
    -- migrate these apps to use the GitHub App. For more information, see
    -- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
    -- in the /Amplify User Guide/ .
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The automated branch creation configuration for an Amplify app.
    autoBranchCreationConfig :: Prelude.Maybe AutoBranchCreationConfig,
    -- | The automated branch creation glob patterns for an Amplify app.
    autoBranchCreationPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The credentials for basic authorization for an Amplify app. You must
    -- base64-encode the authorization credentials and provide them in the
    -- format @user:password@.
    basicAuthCredentials :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The build specification (build spec) for an Amplify app.
    buildSpec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The custom HTTP headers for an Amplify app.
    customHeaders :: Prelude.Maybe Prelude.Text,
    -- | The custom rewrite and redirect rules for an Amplify app.
    customRules :: Prelude.Maybe [CustomRule],
    -- | The description for an Amplify app.
    description :: Prelude.Maybe Prelude.Text,
    -- | Enables automated branch creation for an Amplify app.
    enableAutoBranchCreation :: Prelude.Maybe Prelude.Bool,
    -- | Enables basic authorization for an Amplify app. This will apply to all
    -- branches that are part of this app.
    enableBasicAuth :: Prelude.Maybe Prelude.Bool,
    -- | Enables the auto building of branches for an Amplify app.
    enableBranchAutoBuild :: Prelude.Maybe Prelude.Bool,
    -- | Automatically disconnects a branch in the Amplify Console when you
    -- delete a branch from your Git repository.
    enableBranchAutoDeletion :: Prelude.Maybe Prelude.Bool,
    -- | The environment variables map for an Amplify app.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The AWS Identity and Access Management (IAM) service role for an Amplify
    -- app.
    iamServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The OAuth token for a third-party source control system for an Amplify
    -- app. The OAuth token is used to create a webhook and a read-only deploy
    -- key using SSH cloning. The OAuth token is not stored.
    --
    -- Use @oauthToken@ for repository providers other than GitHub, such as
    -- Bitbucket or CodeCommit. To authorize access to GitHub as your
    -- repository provider, use @accessToken@.
    --
    -- You must specify either @oauthToken@ or @accessToken@ when you create a
    -- new app.
    --
    -- Existing Amplify apps deployed from a GitHub repository using OAuth
    -- continue to work with CI\/CD. However, we strongly recommend that you
    -- migrate these apps to use the GitHub App. For more information, see
    -- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
    -- in the /Amplify User Guide/ .
    oauthToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The platform for the Amplify app. For a static app, set the platform
    -- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
    -- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
    -- original SSR support only, set the platform type to @WEB_DYNAMIC@.
    platform :: Prelude.Maybe Platform,
    -- | The repository for an Amplify app.
    repository :: Prelude.Maybe Prelude.Text,
    -- | The tag for an Amplify app.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name for an Amplify app.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'createApp_accessToken' - The personal access token for a GitHub repository for an Amplify app.
-- The personal access token is used to authorize access to a GitHub
-- repository using the Amplify GitHub App. The token is not stored.
--
-- Use @accessToken@ for GitHub repositories only. To authorize access to a
-- repository provider such as Bitbucket or CodeCommit, use @oauthToken@.
--
-- You must specify either @accessToken@ or @oauthToken@ when you create a
-- new app.
--
-- Existing Amplify apps deployed from a GitHub repository using OAuth
-- continue to work with CI\/CD. However, we strongly recommend that you
-- migrate these apps to use the GitHub App. For more information, see
-- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
-- in the /Amplify User Guide/ .
--
-- 'autoBranchCreationConfig', 'createApp_autoBranchCreationConfig' - The automated branch creation configuration for an Amplify app.
--
-- 'autoBranchCreationPatterns', 'createApp_autoBranchCreationPatterns' - The automated branch creation glob patterns for an Amplify app.
--
-- 'basicAuthCredentials', 'createApp_basicAuthCredentials' - The credentials for basic authorization for an Amplify app. You must
-- base64-encode the authorization credentials and provide them in the
-- format @user:password@.
--
-- 'buildSpec', 'createApp_buildSpec' - The build specification (build spec) for an Amplify app.
--
-- 'customHeaders', 'createApp_customHeaders' - The custom HTTP headers for an Amplify app.
--
-- 'customRules', 'createApp_customRules' - The custom rewrite and redirect rules for an Amplify app.
--
-- 'description', 'createApp_description' - The description for an Amplify app.
--
-- 'enableAutoBranchCreation', 'createApp_enableAutoBranchCreation' - Enables automated branch creation for an Amplify app.
--
-- 'enableBasicAuth', 'createApp_enableBasicAuth' - Enables basic authorization for an Amplify app. This will apply to all
-- branches that are part of this app.
--
-- 'enableBranchAutoBuild', 'createApp_enableBranchAutoBuild' - Enables the auto building of branches for an Amplify app.
--
-- 'enableBranchAutoDeletion', 'createApp_enableBranchAutoDeletion' - Automatically disconnects a branch in the Amplify Console when you
-- delete a branch from your Git repository.
--
-- 'environmentVariables', 'createApp_environmentVariables' - The environment variables map for an Amplify app.
--
-- 'iamServiceRoleArn', 'createApp_iamServiceRoleArn' - The AWS Identity and Access Management (IAM) service role for an Amplify
-- app.
--
-- 'oauthToken', 'createApp_oauthToken' - The OAuth token for a third-party source control system for an Amplify
-- app. The OAuth token is used to create a webhook and a read-only deploy
-- key using SSH cloning. The OAuth token is not stored.
--
-- Use @oauthToken@ for repository providers other than GitHub, such as
-- Bitbucket or CodeCommit. To authorize access to GitHub as your
-- repository provider, use @accessToken@.
--
-- You must specify either @oauthToken@ or @accessToken@ when you create a
-- new app.
--
-- Existing Amplify apps deployed from a GitHub repository using OAuth
-- continue to work with CI\/CD. However, we strongly recommend that you
-- migrate these apps to use the GitHub App. For more information, see
-- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
-- in the /Amplify User Guide/ .
--
-- 'platform', 'createApp_platform' - The platform for the Amplify app. For a static app, set the platform
-- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
-- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
-- original SSR support only, set the platform type to @WEB_DYNAMIC@.
--
-- 'repository', 'createApp_repository' - The repository for an Amplify app.
--
-- 'tags', 'createApp_tags' - The tag for an Amplify app.
--
-- 'name', 'createApp_name' - The name for an Amplify app.
newCreateApp ::
  -- | 'name'
  Prelude.Text ->
  CreateApp
newCreateApp pName_ =
  CreateApp'
    { accessToken = Prelude.Nothing,
      autoBranchCreationConfig = Prelude.Nothing,
      autoBranchCreationPatterns = Prelude.Nothing,
      basicAuthCredentials = Prelude.Nothing,
      buildSpec = Prelude.Nothing,
      customHeaders = Prelude.Nothing,
      customRules = Prelude.Nothing,
      description = Prelude.Nothing,
      enableAutoBranchCreation = Prelude.Nothing,
      enableBasicAuth = Prelude.Nothing,
      enableBranchAutoBuild = Prelude.Nothing,
      enableBranchAutoDeletion = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      iamServiceRoleArn = Prelude.Nothing,
      oauthToken = Prelude.Nothing,
      platform = Prelude.Nothing,
      repository = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The personal access token for a GitHub repository for an Amplify app.
-- The personal access token is used to authorize access to a GitHub
-- repository using the Amplify GitHub App. The token is not stored.
--
-- Use @accessToken@ for GitHub repositories only. To authorize access to a
-- repository provider such as Bitbucket or CodeCommit, use @oauthToken@.
--
-- You must specify either @accessToken@ or @oauthToken@ when you create a
-- new app.
--
-- Existing Amplify apps deployed from a GitHub repository using OAuth
-- continue to work with CI\/CD. However, we strongly recommend that you
-- migrate these apps to use the GitHub App. For more information, see
-- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
-- in the /Amplify User Guide/ .
createApp_accessToken :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_accessToken = Lens.lens (\CreateApp' {accessToken} -> accessToken) (\s@CreateApp' {} a -> s {accessToken = a} :: CreateApp) Prelude.. Lens.mapping Data._Sensitive

-- | The automated branch creation configuration for an Amplify app.
createApp_autoBranchCreationConfig :: Lens.Lens' CreateApp (Prelude.Maybe AutoBranchCreationConfig)
createApp_autoBranchCreationConfig = Lens.lens (\CreateApp' {autoBranchCreationConfig} -> autoBranchCreationConfig) (\s@CreateApp' {} a -> s {autoBranchCreationConfig = a} :: CreateApp)

-- | The automated branch creation glob patterns for an Amplify app.
createApp_autoBranchCreationPatterns :: Lens.Lens' CreateApp (Prelude.Maybe [Prelude.Text])
createApp_autoBranchCreationPatterns = Lens.lens (\CreateApp' {autoBranchCreationPatterns} -> autoBranchCreationPatterns) (\s@CreateApp' {} a -> s {autoBranchCreationPatterns = a} :: CreateApp) Prelude.. Lens.mapping Lens.coerced

-- | The credentials for basic authorization for an Amplify app. You must
-- base64-encode the authorization credentials and provide them in the
-- format @user:password@.
createApp_basicAuthCredentials :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_basicAuthCredentials = Lens.lens (\CreateApp' {basicAuthCredentials} -> basicAuthCredentials) (\s@CreateApp' {} a -> s {basicAuthCredentials = a} :: CreateApp) Prelude.. Lens.mapping Data._Sensitive

-- | The build specification (build spec) for an Amplify app.
createApp_buildSpec :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_buildSpec = Lens.lens (\CreateApp' {buildSpec} -> buildSpec) (\s@CreateApp' {} a -> s {buildSpec = a} :: CreateApp) Prelude.. Lens.mapping Data._Sensitive

-- | The custom HTTP headers for an Amplify app.
createApp_customHeaders :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_customHeaders = Lens.lens (\CreateApp' {customHeaders} -> customHeaders) (\s@CreateApp' {} a -> s {customHeaders = a} :: CreateApp)

-- | The custom rewrite and redirect rules for an Amplify app.
createApp_customRules :: Lens.Lens' CreateApp (Prelude.Maybe [CustomRule])
createApp_customRules = Lens.lens (\CreateApp' {customRules} -> customRules) (\s@CreateApp' {} a -> s {customRules = a} :: CreateApp) Prelude.. Lens.mapping Lens.coerced

-- | The description for an Amplify app.
createApp_description :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_description = Lens.lens (\CreateApp' {description} -> description) (\s@CreateApp' {} a -> s {description = a} :: CreateApp)

-- | Enables automated branch creation for an Amplify app.
createApp_enableAutoBranchCreation :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Bool)
createApp_enableAutoBranchCreation = Lens.lens (\CreateApp' {enableAutoBranchCreation} -> enableAutoBranchCreation) (\s@CreateApp' {} a -> s {enableAutoBranchCreation = a} :: CreateApp)

-- | Enables basic authorization for an Amplify app. This will apply to all
-- branches that are part of this app.
createApp_enableBasicAuth :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Bool)
createApp_enableBasicAuth = Lens.lens (\CreateApp' {enableBasicAuth} -> enableBasicAuth) (\s@CreateApp' {} a -> s {enableBasicAuth = a} :: CreateApp)

-- | Enables the auto building of branches for an Amplify app.
createApp_enableBranchAutoBuild :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Bool)
createApp_enableBranchAutoBuild = Lens.lens (\CreateApp' {enableBranchAutoBuild} -> enableBranchAutoBuild) (\s@CreateApp' {} a -> s {enableBranchAutoBuild = a} :: CreateApp)

-- | Automatically disconnects a branch in the Amplify Console when you
-- delete a branch from your Git repository.
createApp_enableBranchAutoDeletion :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Bool)
createApp_enableBranchAutoDeletion = Lens.lens (\CreateApp' {enableBranchAutoDeletion} -> enableBranchAutoDeletion) (\s@CreateApp' {} a -> s {enableBranchAutoDeletion = a} :: CreateApp)

-- | The environment variables map for an Amplify app.
createApp_environmentVariables :: Lens.Lens' CreateApp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApp_environmentVariables = Lens.lens (\CreateApp' {environmentVariables} -> environmentVariables) (\s@CreateApp' {} a -> s {environmentVariables = a} :: CreateApp) Prelude.. Lens.mapping Lens.coerced

-- | The AWS Identity and Access Management (IAM) service role for an Amplify
-- app.
createApp_iamServiceRoleArn :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_iamServiceRoleArn = Lens.lens (\CreateApp' {iamServiceRoleArn} -> iamServiceRoleArn) (\s@CreateApp' {} a -> s {iamServiceRoleArn = a} :: CreateApp)

-- | The OAuth token for a third-party source control system for an Amplify
-- app. The OAuth token is used to create a webhook and a read-only deploy
-- key using SSH cloning. The OAuth token is not stored.
--
-- Use @oauthToken@ for repository providers other than GitHub, such as
-- Bitbucket or CodeCommit. To authorize access to GitHub as your
-- repository provider, use @accessToken@.
--
-- You must specify either @oauthToken@ or @accessToken@ when you create a
-- new app.
--
-- Existing Amplify apps deployed from a GitHub repository using OAuth
-- continue to work with CI\/CD. However, we strongly recommend that you
-- migrate these apps to use the GitHub App. For more information, see
-- <https://docs.aws.amazon.com/amplify/latest/UserGuide/setting-up-GitHub-access.html#migrating-to-github-app-auth Migrating an existing OAuth app to the Amplify GitHub App>
-- in the /Amplify User Guide/ .
createApp_oauthToken :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_oauthToken = Lens.lens (\CreateApp' {oauthToken} -> oauthToken) (\s@CreateApp' {} a -> s {oauthToken = a} :: CreateApp) Prelude.. Lens.mapping Data._Sensitive

-- | The platform for the Amplify app. For a static app, set the platform
-- type to @WEB@. For a dynamic server-side rendered (SSR) app, set the
-- platform type to @WEB_COMPUTE@. For an app requiring Amplify Hosting\'s
-- original SSR support only, set the platform type to @WEB_DYNAMIC@.
createApp_platform :: Lens.Lens' CreateApp (Prelude.Maybe Platform)
createApp_platform = Lens.lens (\CreateApp' {platform} -> platform) (\s@CreateApp' {} a -> s {platform = a} :: CreateApp)

-- | The repository for an Amplify app.
createApp_repository :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_repository = Lens.lens (\CreateApp' {repository} -> repository) (\s@CreateApp' {} a -> s {repository = a} :: CreateApp)

-- | The tag for an Amplify app.
createApp_tags :: Lens.Lens' CreateApp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApp_tags = Lens.lens (\CreateApp' {tags} -> tags) (\s@CreateApp' {} a -> s {tags = a} :: CreateApp) Prelude.. Lens.mapping Lens.coerced

-- | The name for an Amplify app.
createApp_name :: Lens.Lens' CreateApp Prelude.Text
createApp_name = Lens.lens (\CreateApp' {name} -> name) (\s@CreateApp' {} a -> s {name = a} :: CreateApp)

instance Core.AWSRequest CreateApp where
  type AWSResponse CreateApp = CreateAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "app")
      )

instance Prelude.Hashable CreateApp where
  hashWithSalt _salt CreateApp' {..} =
    _salt `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` autoBranchCreationConfig
      `Prelude.hashWithSalt` autoBranchCreationPatterns
      `Prelude.hashWithSalt` basicAuthCredentials
      `Prelude.hashWithSalt` buildSpec
      `Prelude.hashWithSalt` customHeaders
      `Prelude.hashWithSalt` customRules
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enableAutoBranchCreation
      `Prelude.hashWithSalt` enableBasicAuth
      `Prelude.hashWithSalt` enableBranchAutoBuild
      `Prelude.hashWithSalt` enableBranchAutoDeletion
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` iamServiceRoleArn
      `Prelude.hashWithSalt` oauthToken
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateApp where
  rnf CreateApp' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf autoBranchCreationConfig
      `Prelude.seq` Prelude.rnf autoBranchCreationPatterns
      `Prelude.seq` Prelude.rnf basicAuthCredentials
      `Prelude.seq` Prelude.rnf buildSpec
      `Prelude.seq` Prelude.rnf customHeaders
      `Prelude.seq` Prelude.rnf customRules
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf enableAutoBranchCreation
      `Prelude.seq` Prelude.rnf enableBasicAuth
      `Prelude.seq` Prelude.rnf enableBranchAutoBuild
      `Prelude.seq` Prelude.rnf enableBranchAutoDeletion
      `Prelude.seq` Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf iamServiceRoleArn
      `Prelude.seq` Prelude.rnf oauthToken
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessToken" Data..=) Prelude.<$> accessToken,
            ("autoBranchCreationConfig" Data..=)
              Prelude.<$> autoBranchCreationConfig,
            ("autoBranchCreationPatterns" Data..=)
              Prelude.<$> autoBranchCreationPatterns,
            ("basicAuthCredentials" Data..=)
              Prelude.<$> basicAuthCredentials,
            ("buildSpec" Data..=) Prelude.<$> buildSpec,
            ("customHeaders" Data..=) Prelude.<$> customHeaders,
            ("customRules" Data..=) Prelude.<$> customRules,
            ("description" Data..=) Prelude.<$> description,
            ("enableAutoBranchCreation" Data..=)
              Prelude.<$> enableAutoBranchCreation,
            ("enableBasicAuth" Data..=)
              Prelude.<$> enableBasicAuth,
            ("enableBranchAutoBuild" Data..=)
              Prelude.<$> enableBranchAutoBuild,
            ("enableBranchAutoDeletion" Data..=)
              Prelude.<$> enableBranchAutoDeletion,
            ("environmentVariables" Data..=)
              Prelude.<$> environmentVariables,
            ("iamServiceRoleArn" Data..=)
              Prelude.<$> iamServiceRoleArn,
            ("oauthToken" Data..=) Prelude.<$> oauthToken,
            ("platform" Data..=) Prelude.<$> platform,
            ("repository" Data..=) Prelude.<$> repository,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateApp where
  toPath = Prelude.const "/apps"

instance Data.ToQuery CreateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    app :: App
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAppResponse_httpStatus' - The response's http status code.
--
-- 'app', 'createAppResponse_app' - Undocumented member.
newCreateAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'app'
  App ->
  CreateAppResponse
newCreateAppResponse pHttpStatus_ pApp_ =
  CreateAppResponse'
    { httpStatus = pHttpStatus_,
      app = pApp_
    }

-- | The response's http status code.
createAppResponse_httpStatus :: Lens.Lens' CreateAppResponse Prelude.Int
createAppResponse_httpStatus = Lens.lens (\CreateAppResponse' {httpStatus} -> httpStatus) (\s@CreateAppResponse' {} a -> s {httpStatus = a} :: CreateAppResponse)

-- | Undocumented member.
createAppResponse_app :: Lens.Lens' CreateAppResponse App
createAppResponse_app = Lens.lens (\CreateAppResponse' {app} -> app) (\s@CreateAppResponse' {} a -> s {app = a} :: CreateAppResponse)

instance Prelude.NFData CreateAppResponse where
  rnf CreateAppResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf app
