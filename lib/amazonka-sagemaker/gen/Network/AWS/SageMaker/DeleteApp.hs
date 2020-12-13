{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to stop and delete an app.
module Network.AWS.SageMaker.DeleteApp
  ( -- * Creating a request
    DeleteApp (..),
    mkDeleteApp,

    -- ** Request lenses
    dafUserProfileName,
    dafAppName,
    dafDomainId,
    dafAppType,

    -- * Destructuring the response
    DeleteAppResponse (..),
    mkDeleteAppResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | The user profile name.
    userProfileName :: Lude.Text,
    -- | The name of the app.
    appName :: Lude.Text,
    -- | The domain ID.
    domainId :: Lude.Text,
    -- | The type of app.
    appType :: AppType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- * 'userProfileName' - The user profile name.
-- * 'appName' - The name of the app.
-- * 'domainId' - The domain ID.
-- * 'appType' - The type of app.
mkDeleteApp ::
  -- | 'userProfileName'
  Lude.Text ->
  -- | 'appName'
  Lude.Text ->
  -- | 'domainId'
  Lude.Text ->
  -- | 'appType'
  AppType ->
  DeleteApp
mkDeleteApp pUserProfileName_ pAppName_ pDomainId_ pAppType_ =
  DeleteApp'
    { userProfileName = pUserProfileName_,
      appName = pAppName_,
      domainId = pDomainId_,
      appType = pAppType_
    }

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafUserProfileName :: Lens.Lens' DeleteApp Lude.Text
dafUserProfileName = Lens.lens (userProfileName :: DeleteApp -> Lude.Text) (\s a -> s {userProfileName = a} :: DeleteApp)
{-# DEPRECATED dafUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafAppName :: Lens.Lens' DeleteApp Lude.Text
dafAppName = Lens.lens (appName :: DeleteApp -> Lude.Text) (\s a -> s {appName = a} :: DeleteApp)
{-# DEPRECATED dafAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafDomainId :: Lens.Lens' DeleteApp Lude.Text
dafDomainId = Lens.lens (domainId :: DeleteApp -> Lude.Text) (\s a -> s {domainId = a} :: DeleteApp)
{-# DEPRECATED dafDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafAppType :: Lens.Lens' DeleteApp AppType
dafAppType = Lens.lens (appType :: DeleteApp -> AppType) (\s a -> s {appType = a} :: DeleteApp)
{-# DEPRECATED dafAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

instance Lude.AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteAppResponse'

instance Lude.ToHeaders DeleteApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("SageMaker.DeleteApp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserProfileName" Lude..= userProfileName),
            Lude.Just ("AppName" Lude..= appName),
            Lude.Just ("DomainId" Lude..= domainId),
            Lude.Just ("AppType" Lude..= appType)
          ]
      )

instance Lude.ToPath DeleteApp where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
mkDeleteAppResponse ::
  DeleteAppResponse
mkDeleteAppResponse = DeleteAppResponse'
