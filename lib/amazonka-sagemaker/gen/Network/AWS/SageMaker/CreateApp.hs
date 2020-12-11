{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    caTags,
    caDomainId,
    caUserProfileName,
    caAppType,
    caAppName,

    -- * Destructuring the response
    CreateAppResponse (..),
    mkCreateAppResponse,

    -- ** Response lenses
    crersAppARN,
    crersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { resourceSpec ::
      Lude.Maybe ResourceSpec,
    tags :: Lude.Maybe [Tag],
    domainId :: Lude.Text,
    userProfileName :: Lude.Text,
    appType :: AppType,
    appName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApp' with the minimum fields required to make a request.
--
-- * 'appName' - The name of the app.
-- * 'appType' - The type of app.
-- * 'domainId' - The domain ID.
-- * 'resourceSpec' - The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
-- * 'tags' - Each tag consists of a key and an optional value. Tag keys must be unique per resource.
-- * 'userProfileName' - The user profile name.
mkCreateApp ::
  -- | 'domainId'
  Lude.Text ->
  -- | 'userProfileName'
  Lude.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Lude.Text ->
  CreateApp
mkCreateApp pDomainId_ pUserProfileName_ pAppType_ pAppName_ =
  CreateApp'
    { resourceSpec = Lude.Nothing,
      tags = Lude.Nothing,
      domainId = pDomainId_,
      userProfileName = pUserProfileName_,
      appType = pAppType_,
      appName = pAppName_
    }

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'resourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caResourceSpec :: Lens.Lens' CreateApp (Lude.Maybe ResourceSpec)
caResourceSpec = Lens.lens (resourceSpec :: CreateApp -> Lude.Maybe ResourceSpec) (\s a -> s {resourceSpec = a} :: CreateApp)
{-# DEPRECATED caResourceSpec "Use generic-lens or generic-optics with 'resourceSpec' instead." #-}

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApp (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateApp -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateApp)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDomainId :: Lens.Lens' CreateApp Lude.Text
caDomainId = Lens.lens (domainId :: CreateApp -> Lude.Text) (\s a -> s {domainId = a} :: CreateApp)
{-# DEPRECATED caDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caUserProfileName :: Lens.Lens' CreateApp Lude.Text
caUserProfileName = Lens.lens (userProfileName :: CreateApp -> Lude.Text) (\s a -> s {userProfileName = a} :: CreateApp)
{-# DEPRECATED caUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppType :: Lens.Lens' CreateApp AppType
caAppType = Lens.lens (appType :: CreateApp -> AppType) (\s a -> s {appType = a} :: CreateApp)
{-# DEPRECATED caAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppName :: Lens.Lens' CreateApp Lude.Text
caAppName = Lens.lens (appName :: CreateApp -> Lude.Text) (\s a -> s {appName = a} :: CreateApp)
{-# DEPRECATED caAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

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
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("DomainId" Lude..= domainId),
            Lude.Just ("UserProfileName" Lude..= userProfileName),
            Lude.Just ("AppType" Lude..= appType),
            Lude.Just ("AppName" Lude..= appName)
          ]
      )

instance Lude.ToPath CreateApp where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { appARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
crersAppARN :: Lens.Lens' CreateAppResponse (Lude.Maybe Lude.Text)
crersAppARN = Lens.lens (appARN :: CreateAppResponse -> Lude.Maybe Lude.Text) (\s a -> s {appARN = a} :: CreateAppResponse)
{-# DEPRECATED crersAppARN "Use generic-lens or generic-optics with 'appARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersResponseStatus :: Lens.Lens' CreateAppResponse Lude.Int
crersResponseStatus = Lens.lens (responseStatus :: CreateAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAppResponse)
{-# DEPRECATED crersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
