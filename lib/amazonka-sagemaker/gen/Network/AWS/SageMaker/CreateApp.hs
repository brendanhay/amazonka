{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a running App for the specified UserProfile. Supported Apps are JupyterServer and KernelGateway. This operation is automatically invoked by Amazon SageMaker Studio upon access to the associated Domain, and when new kernel configurations are selected by the user. A user may have multiple Apps active simultaneously.
module Network.AWS.SageMaker.CreateApp
  ( -- * Creating a request
    CreateApp (..),
    mkCreateApp,

    -- ** Request lenses
    caResourceSpec,
    caUserProfileName,
    caAppName,
    caDomainId,
    caTags,
    caAppType,

    -- * Destructuring the response
    CreateAppResponse (..),
    mkCreateAppResponse,

    -- ** Response lenses
    carsAppARN,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
    resourceSpec :: Lude.Maybe ResourceSpec,
    -- | The user profile name.
    userProfileName :: Lude.Text,
    -- | The name of the app.
    appName :: Lude.Text,
    -- | The domain ID.
    domainId :: Lude.Text,
    -- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
    tags :: Lude.Maybe [Tag],
    -- | The type of app.
    appType :: AppType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApp' with the minimum fields required to make a request.
--
-- * 'resourceSpec' - The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
-- * 'userProfileName' - The user profile name.
-- * 'appName' - The name of the app.
-- * 'domainId' - The domain ID.
-- * 'tags' - Each tag consists of a key and an optional value. Tag keys must be unique per resource.
-- * 'appType' - The type of app.
mkCreateApp ::
  -- | 'userProfileName'
  Lude.Text ->
  -- | 'appName'
  Lude.Text ->
  -- | 'domainId'
  Lude.Text ->
  -- | 'appType'
  AppType ->
  CreateApp
mkCreateApp pUserProfileName_ pAppName_ pDomainId_ pAppType_ =
  CreateApp'
    { resourceSpec = Lude.Nothing,
      userProfileName = pUserProfileName_,
      appName = pAppName_,
      domainId = pDomainId_,
      tags = Lude.Nothing,
      appType = pAppType_
    }

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'resourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caResourceSpec :: Lens.Lens' CreateApp (Lude.Maybe ResourceSpec)
caResourceSpec = Lens.lens (resourceSpec :: CreateApp -> Lude.Maybe ResourceSpec) (\s a -> s {resourceSpec = a} :: CreateApp)
{-# DEPRECATED caResourceSpec "Use generic-lens or generic-optics with 'resourceSpec' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caUserProfileName :: Lens.Lens' CreateApp Lude.Text
caUserProfileName = Lens.lens (userProfileName :: CreateApp -> Lude.Text) (\s a -> s {userProfileName = a} :: CreateApp)
{-# DEPRECATED caUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppName :: Lens.Lens' CreateApp Lude.Text
caAppName = Lens.lens (appName :: CreateApp -> Lude.Text) (\s a -> s {appName = a} :: CreateApp)
{-# DEPRECATED caAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDomainId :: Lens.Lens' CreateApp Lude.Text
caDomainId = Lens.lens (domainId :: CreateApp -> Lude.Text) (\s a -> s {domainId = a} :: CreateApp)
{-# DEPRECATED caDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApp (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateApp -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateApp)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppType :: Lens.Lens' CreateApp AppType
caAppType = Lens.lens (appType :: CreateApp -> AppType) (\s a -> s {appType = a} :: CreateApp)
{-# DEPRECATED caAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

instance Lude.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Lude.<$> (x Lude..?> "AppArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("SageMaker.CreateApp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceSpec" Lude..=) Lude.<$> resourceSpec,
            Lude.Just ("UserProfileName" Lude..= userProfileName),
            Lude.Just ("AppName" Lude..= appName),
            Lude.Just ("DomainId" Lude..= domainId),
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("AppType" Lude..= appType)
          ]
      )

instance Lude.ToPath CreateApp where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The Amazon Resource Name (ARN) of the app.
    appARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAppResponse' with the minimum fields required to make a request.
--
-- * 'appARN' - The Amazon Resource Name (ARN) of the app.
-- * 'responseStatus' - The response status code.
mkCreateAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAppResponse
mkCreateAppResponse pResponseStatus_ =
  CreateAppResponse'
    { appARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the app.
--
-- /Note:/ Consider using 'appARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAppARN :: Lens.Lens' CreateAppResponse (Lude.Maybe Lude.Text)
carsAppARN = Lens.lens (appARN :: CreateAppResponse -> Lude.Maybe Lude.Text) (\s a -> s {appARN = a} :: CreateAppResponse)
{-# DEPRECATED carsAppARN "Use generic-lens or generic-optics with 'appARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAppResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAppResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
