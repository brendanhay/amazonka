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
-- Module      : Network.AWS.Amplify.UpdateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amplify app.
module Network.AWS.Amplify.UpdateApp
  ( -- * Creating a Request
    UpdateApp (..),
    newUpdateApp,

    -- * Request Lenses
    updateApp_enableBranchAutoBuild,
    updateApp_oauthToken,
    updateApp_accessToken,
    updateApp_customHeaders,
    updateApp_platform,
    updateApp_basicAuthCredentials,
    updateApp_repository,
    updateApp_buildSpec,
    updateApp_enableBranchAutoDeletion,
    updateApp_customRules,
    updateApp_iamServiceRoleArn,
    updateApp_autoBranchCreationPatterns,
    updateApp_name,
    updateApp_autoBranchCreationConfig,
    updateApp_environmentVariables,
    updateApp_enableAutoBranchCreation,
    updateApp_enableBasicAuth,
    updateApp_description,
    updateApp_appId,

    -- * Destructuring the Response
    UpdateAppResponse (..),
    newUpdateAppResponse,

    -- * Response Lenses
    updateAppResponse_httpStatus,
    updateAppResponse_app,
  )
where

import Network.AWS.Amplify.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request structure for the update app request.
--
-- /See:/ 'newUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | Enables branch auto-building for an Amplify app.
    enableBranchAutoBuild :: Prelude.Maybe Prelude.Bool,
    -- | The OAuth token for a third-party source control system for an Amplify
    -- app. The token is used to create a webhook and a read-only deploy key.
    -- The OAuth token is not stored.
    oauthToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The personal access token for a third-party source control system for an
    -- Amplify app. The token is used to create webhook and a read-only deploy
    -- key. The token is not stored.
    accessToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The custom HTTP headers for an Amplify app.
    customHeaders :: Prelude.Maybe Prelude.Text,
    -- | The platform for an Amplify app.
    platform :: Prelude.Maybe Platform,
    -- | The basic authorization credentials for an Amplify app.
    basicAuthCredentials :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The name of the repository for an Amplify app
    repository :: Prelude.Maybe Prelude.Text,
    -- | The build specification (build spec) for an Amplify app.
    buildSpec :: Prelude.Maybe Prelude.Text,
    -- | Automatically disconnects a branch in the Amplify Console when you
    -- delete a branch from your Git repository.
    enableBranchAutoDeletion :: Prelude.Maybe Prelude.Bool,
    -- | The custom redirect and rewrite rules for an Amplify app.
    customRules :: Prelude.Maybe [CustomRule],
    -- | The AWS Identity and Access Management (IAM) service role for an Amplify
    -- app.
    iamServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Describes the automated branch creation glob patterns for an Amplify
    -- app.
    autoBranchCreationPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The name for an Amplify app.
    name :: Prelude.Maybe Prelude.Text,
    -- | The automated branch creation configuration for an Amplify app.
    autoBranchCreationConfig :: Prelude.Maybe AutoBranchCreationConfig,
    -- | The environment variables for an Amplify app.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Enables automated branch creation for an Amplify app.
    enableAutoBranchCreation :: Prelude.Maybe Prelude.Bool,
    -- | Enables basic authorization for an Amplify app.
    enableBasicAuth :: Prelude.Maybe Prelude.Bool,
    -- | The description for an Amplify app.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'enableBranchAutoBuild', 'updateApp_enableBranchAutoBuild' - Enables branch auto-building for an Amplify app.
--
-- 'oauthToken', 'updateApp_oauthToken' - The OAuth token for a third-party source control system for an Amplify
-- app. The token is used to create a webhook and a read-only deploy key.
-- The OAuth token is not stored.
--
-- 'accessToken', 'updateApp_accessToken' - The personal access token for a third-party source control system for an
-- Amplify app. The token is used to create webhook and a read-only deploy
-- key. The token is not stored.
--
-- 'customHeaders', 'updateApp_customHeaders' - The custom HTTP headers for an Amplify app.
--
-- 'platform', 'updateApp_platform' - The platform for an Amplify app.
--
-- 'basicAuthCredentials', 'updateApp_basicAuthCredentials' - The basic authorization credentials for an Amplify app.
--
-- 'repository', 'updateApp_repository' - The name of the repository for an Amplify app
--
-- 'buildSpec', 'updateApp_buildSpec' - The build specification (build spec) for an Amplify app.
--
-- 'enableBranchAutoDeletion', 'updateApp_enableBranchAutoDeletion' - Automatically disconnects a branch in the Amplify Console when you
-- delete a branch from your Git repository.
--
-- 'customRules', 'updateApp_customRules' - The custom redirect and rewrite rules for an Amplify app.
--
-- 'iamServiceRoleArn', 'updateApp_iamServiceRoleArn' - The AWS Identity and Access Management (IAM) service role for an Amplify
-- app.
--
-- 'autoBranchCreationPatterns', 'updateApp_autoBranchCreationPatterns' - Describes the automated branch creation glob patterns for an Amplify
-- app.
--
-- 'name', 'updateApp_name' - The name for an Amplify app.
--
-- 'autoBranchCreationConfig', 'updateApp_autoBranchCreationConfig' - The automated branch creation configuration for an Amplify app.
--
-- 'environmentVariables', 'updateApp_environmentVariables' - The environment variables for an Amplify app.
--
-- 'enableAutoBranchCreation', 'updateApp_enableAutoBranchCreation' - Enables automated branch creation for an Amplify app.
--
-- 'enableBasicAuth', 'updateApp_enableBasicAuth' - Enables basic authorization for an Amplify app.
--
-- 'description', 'updateApp_description' - The description for an Amplify app.
--
-- 'appId', 'updateApp_appId' - The unique ID for an Amplify app.
newUpdateApp ::
  -- | 'appId'
  Prelude.Text ->
  UpdateApp
newUpdateApp pAppId_ =
  UpdateApp'
    { enableBranchAutoBuild = Prelude.Nothing,
      oauthToken = Prelude.Nothing,
      accessToken = Prelude.Nothing,
      customHeaders = Prelude.Nothing,
      platform = Prelude.Nothing,
      basicAuthCredentials = Prelude.Nothing,
      repository = Prelude.Nothing,
      buildSpec = Prelude.Nothing,
      enableBranchAutoDeletion = Prelude.Nothing,
      customRules = Prelude.Nothing,
      iamServiceRoleArn = Prelude.Nothing,
      autoBranchCreationPatterns = Prelude.Nothing,
      name = Prelude.Nothing,
      autoBranchCreationConfig = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      enableAutoBranchCreation = Prelude.Nothing,
      enableBasicAuth = Prelude.Nothing,
      description = Prelude.Nothing,
      appId = pAppId_
    }

-- | Enables branch auto-building for an Amplify app.
updateApp_enableBranchAutoBuild :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableBranchAutoBuild = Lens.lens (\UpdateApp' {enableBranchAutoBuild} -> enableBranchAutoBuild) (\s@UpdateApp' {} a -> s {enableBranchAutoBuild = a} :: UpdateApp)

-- | The OAuth token for a third-party source control system for an Amplify
-- app. The token is used to create a webhook and a read-only deploy key.
-- The OAuth token is not stored.
updateApp_oauthToken :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_oauthToken = Lens.lens (\UpdateApp' {oauthToken} -> oauthToken) (\s@UpdateApp' {} a -> s {oauthToken = a} :: UpdateApp) Prelude.. Lens.mapping Core._Sensitive

-- | The personal access token for a third-party source control system for an
-- Amplify app. The token is used to create webhook and a read-only deploy
-- key. The token is not stored.
updateApp_accessToken :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_accessToken = Lens.lens (\UpdateApp' {accessToken} -> accessToken) (\s@UpdateApp' {} a -> s {accessToken = a} :: UpdateApp) Prelude.. Lens.mapping Core._Sensitive

-- | The custom HTTP headers for an Amplify app.
updateApp_customHeaders :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_customHeaders = Lens.lens (\UpdateApp' {customHeaders} -> customHeaders) (\s@UpdateApp' {} a -> s {customHeaders = a} :: UpdateApp)

-- | The platform for an Amplify app.
updateApp_platform :: Lens.Lens' UpdateApp (Prelude.Maybe Platform)
updateApp_platform = Lens.lens (\UpdateApp' {platform} -> platform) (\s@UpdateApp' {} a -> s {platform = a} :: UpdateApp)

-- | The basic authorization credentials for an Amplify app.
updateApp_basicAuthCredentials :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_basicAuthCredentials = Lens.lens (\UpdateApp' {basicAuthCredentials} -> basicAuthCredentials) (\s@UpdateApp' {} a -> s {basicAuthCredentials = a} :: UpdateApp) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the repository for an Amplify app
updateApp_repository :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_repository = Lens.lens (\UpdateApp' {repository} -> repository) (\s@UpdateApp' {} a -> s {repository = a} :: UpdateApp)

-- | The build specification (build spec) for an Amplify app.
updateApp_buildSpec :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_buildSpec = Lens.lens (\UpdateApp' {buildSpec} -> buildSpec) (\s@UpdateApp' {} a -> s {buildSpec = a} :: UpdateApp)

-- | Automatically disconnects a branch in the Amplify Console when you
-- delete a branch from your Git repository.
updateApp_enableBranchAutoDeletion :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableBranchAutoDeletion = Lens.lens (\UpdateApp' {enableBranchAutoDeletion} -> enableBranchAutoDeletion) (\s@UpdateApp' {} a -> s {enableBranchAutoDeletion = a} :: UpdateApp)

-- | The custom redirect and rewrite rules for an Amplify app.
updateApp_customRules :: Lens.Lens' UpdateApp (Prelude.Maybe [CustomRule])
updateApp_customRules = Lens.lens (\UpdateApp' {customRules} -> customRules) (\s@UpdateApp' {} a -> s {customRules = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | The AWS Identity and Access Management (IAM) service role for an Amplify
-- app.
updateApp_iamServiceRoleArn :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_iamServiceRoleArn = Lens.lens (\UpdateApp' {iamServiceRoleArn} -> iamServiceRoleArn) (\s@UpdateApp' {} a -> s {iamServiceRoleArn = a} :: UpdateApp)

-- | Describes the automated branch creation glob patterns for an Amplify
-- app.
updateApp_autoBranchCreationPatterns :: Lens.Lens' UpdateApp (Prelude.Maybe [Prelude.Text])
updateApp_autoBranchCreationPatterns = Lens.lens (\UpdateApp' {autoBranchCreationPatterns} -> autoBranchCreationPatterns) (\s@UpdateApp' {} a -> s {autoBranchCreationPatterns = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | The name for an Amplify app.
updateApp_name :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_name = Lens.lens (\UpdateApp' {name} -> name) (\s@UpdateApp' {} a -> s {name = a} :: UpdateApp)

-- | The automated branch creation configuration for an Amplify app.
updateApp_autoBranchCreationConfig :: Lens.Lens' UpdateApp (Prelude.Maybe AutoBranchCreationConfig)
updateApp_autoBranchCreationConfig = Lens.lens (\UpdateApp' {autoBranchCreationConfig} -> autoBranchCreationConfig) (\s@UpdateApp' {} a -> s {autoBranchCreationConfig = a} :: UpdateApp)

-- | The environment variables for an Amplify app.
updateApp_environmentVariables :: Lens.Lens' UpdateApp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateApp_environmentVariables = Lens.lens (\UpdateApp' {environmentVariables} -> environmentVariables) (\s@UpdateApp' {} a -> s {environmentVariables = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | Enables automated branch creation for an Amplify app.
updateApp_enableAutoBranchCreation :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableAutoBranchCreation = Lens.lens (\UpdateApp' {enableAutoBranchCreation} -> enableAutoBranchCreation) (\s@UpdateApp' {} a -> s {enableAutoBranchCreation = a} :: UpdateApp)

-- | Enables basic authorization for an Amplify app.
updateApp_enableBasicAuth :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableBasicAuth = Lens.lens (\UpdateApp' {enableBasicAuth} -> enableBasicAuth) (\s@UpdateApp' {} a -> s {enableBasicAuth = a} :: UpdateApp)

-- | The description for an Amplify app.
updateApp_description :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_description = Lens.lens (\UpdateApp' {description} -> description) (\s@UpdateApp' {} a -> s {description = a} :: UpdateApp)

-- | The unique ID for an Amplify app.
updateApp_appId :: Lens.Lens' UpdateApp Prelude.Text
updateApp_appId = Lens.lens (\UpdateApp' {appId} -> appId) (\s@UpdateApp' {} a -> s {appId = a} :: UpdateApp)

instance Core.AWSRequest UpdateApp where
  type AWSResponse UpdateApp = UpdateAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "app")
      )

instance Prelude.Hashable UpdateApp

instance Prelude.NFData UpdateApp

instance Core.ToHeaders UpdateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("enableBranchAutoBuild" Core..=)
              Prelude.<$> enableBranchAutoBuild,
            ("oauthToken" Core..=) Prelude.<$> oauthToken,
            ("accessToken" Core..=) Prelude.<$> accessToken,
            ("customHeaders" Core..=) Prelude.<$> customHeaders,
            ("platform" Core..=) Prelude.<$> platform,
            ("basicAuthCredentials" Core..=)
              Prelude.<$> basicAuthCredentials,
            ("repository" Core..=) Prelude.<$> repository,
            ("buildSpec" Core..=) Prelude.<$> buildSpec,
            ("enableBranchAutoDeletion" Core..=)
              Prelude.<$> enableBranchAutoDeletion,
            ("customRules" Core..=) Prelude.<$> customRules,
            ("iamServiceRoleArn" Core..=)
              Prelude.<$> iamServiceRoleArn,
            ("autoBranchCreationPatterns" Core..=)
              Prelude.<$> autoBranchCreationPatterns,
            ("name" Core..=) Prelude.<$> name,
            ("autoBranchCreationConfig" Core..=)
              Prelude.<$> autoBranchCreationConfig,
            ("environmentVariables" Core..=)
              Prelude.<$> environmentVariables,
            ("enableAutoBranchCreation" Core..=)
              Prelude.<$> enableAutoBranchCreation,
            ("enableBasicAuth" Core..=)
              Prelude.<$> enableBasicAuth,
            ("description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateApp where
  toPath UpdateApp' {..} =
    Prelude.mconcat ["/apps/", Core.toBS appId]

instance Core.ToQuery UpdateApp where
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

instance Prelude.NFData UpdateAppResponse
