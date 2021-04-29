{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.CreateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a running App for the specified UserProfile. Supported Apps are
-- JupyterServer and KernelGateway. This operation is automatically invoked
-- by Amazon SageMaker Studio upon access to the associated Domain, and
-- when new kernel configurations are selected by the user. A user may have
-- multiple Apps active simultaneously.
module Network.AWS.SageMaker.CreateApp
  ( -- * Creating a Request
    CreateApp (..),
    newCreateApp,

    -- * Request Lenses
    createApp_resourceSpec,
    createApp_tags,
    createApp_domainId,
    createApp_userProfileName,
    createApp_appType,
    createApp_appName,

    -- * Destructuring the Response
    CreateAppResponse (..),
    newCreateAppResponse,

    -- * Response Lenses
    createAppResponse_appArn,
    createAppResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | The instance type and the Amazon Resource Name (ARN) of the SageMaker
    -- image created on the instance.
    resourceSpec :: Prelude.Maybe ResourceSpec,
    -- | Each tag consists of a key and an optional value. Tag keys must be
    -- unique per resource.
    tags :: Prelude.Maybe [Tag],
    -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Text,
    -- | The type of app.
    appType :: AppType,
    -- | The name of the app.
    appName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSpec', 'createApp_resourceSpec' - The instance type and the Amazon Resource Name (ARN) of the SageMaker
-- image created on the instance.
--
-- 'tags', 'createApp_tags' - Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
--
-- 'domainId', 'createApp_domainId' - The domain ID.
--
-- 'userProfileName', 'createApp_userProfileName' - The user profile name.
--
-- 'appType', 'createApp_appType' - The type of app.
--
-- 'appName', 'createApp_appName' - The name of the app.
newCreateApp ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'userProfileName'
  Prelude.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Prelude.Text ->
  CreateApp
newCreateApp
  pDomainId_
  pUserProfileName_
  pAppType_
  pAppName_ =
    CreateApp'
      { resourceSpec = Prelude.Nothing,
        tags = Prelude.Nothing,
        domainId = pDomainId_,
        userProfileName = pUserProfileName_,
        appType = pAppType_,
        appName = pAppName_
      }

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker
-- image created on the instance.
createApp_resourceSpec :: Lens.Lens' CreateApp (Prelude.Maybe ResourceSpec)
createApp_resourceSpec = Lens.lens (\CreateApp' {resourceSpec} -> resourceSpec) (\s@CreateApp' {} a -> s {resourceSpec = a} :: CreateApp)

-- | Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
createApp_tags :: Lens.Lens' CreateApp (Prelude.Maybe [Tag])
createApp_tags = Lens.lens (\CreateApp' {tags} -> tags) (\s@CreateApp' {} a -> s {tags = a} :: CreateApp) Prelude.. Lens.mapping Prelude._Coerce

-- | The domain ID.
createApp_domainId :: Lens.Lens' CreateApp Prelude.Text
createApp_domainId = Lens.lens (\CreateApp' {domainId} -> domainId) (\s@CreateApp' {} a -> s {domainId = a} :: CreateApp)

-- | The user profile name.
createApp_userProfileName :: Lens.Lens' CreateApp Prelude.Text
createApp_userProfileName = Lens.lens (\CreateApp' {userProfileName} -> userProfileName) (\s@CreateApp' {} a -> s {userProfileName = a} :: CreateApp)

-- | The type of app.
createApp_appType :: Lens.Lens' CreateApp AppType
createApp_appType = Lens.lens (\CreateApp' {appType} -> appType) (\s@CreateApp' {} a -> s {appType = a} :: CreateApp)

-- | The name of the app.
createApp_appName :: Lens.Lens' CreateApp Prelude.Text
createApp_appName = Lens.lens (\CreateApp' {appName} -> appName) (\s@CreateApp' {} a -> s {appName = a} :: CreateApp)

instance Prelude.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Prelude.<$> (x Prelude..?> "AppArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApp

instance Prelude.NFData CreateApp

instance Prelude.ToHeaders CreateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.CreateApp" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceSpec" Prelude..=)
              Prelude.<$> resourceSpec,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("DomainId" Prelude..= domainId),
            Prelude.Just
              ("UserProfileName" Prelude..= userProfileName),
            Prelude.Just ("AppType" Prelude..= appType),
            Prelude.Just ("AppName" Prelude..= appName)
          ]
      )

instance Prelude.ToPath CreateApp where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The Amazon Resource Name (ARN) of the app.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'createAppResponse_appArn' - The Amazon Resource Name (ARN) of the app.
--
-- 'httpStatus', 'createAppResponse_httpStatus' - The response's http status code.
newCreateAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppResponse
newCreateAppResponse pHttpStatus_ =
  CreateAppResponse'
    { appArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the app.
createAppResponse_appArn :: Lens.Lens' CreateAppResponse (Prelude.Maybe Prelude.Text)
createAppResponse_appArn = Lens.lens (\CreateAppResponse' {appArn} -> appArn) (\s@CreateAppResponse' {} a -> s {appArn = a} :: CreateAppResponse)

-- | The response's http status code.
createAppResponse_httpStatus :: Lens.Lens' CreateAppResponse Prelude.Int
createAppResponse_httpStatus = Lens.lens (\CreateAppResponse' {httpStatus} -> httpStatus) (\s@CreateAppResponse' {} a -> s {httpStatus = a} :: CreateAppResponse)

instance Prelude.NFData CreateAppResponse
