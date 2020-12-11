{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dDomainId,
    dUserProfileName,
    dAppType,
    dAppName,

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
  { domainId :: Lude.Text,
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

-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- * 'appName' - The name of the app.
-- * 'appType' - The type of app.
-- * 'domainId' - The domain ID.
-- * 'userProfileName' - The user profile name.
mkDeleteApp ::
  -- | 'domainId'
  Lude.Text ->
  -- | 'userProfileName'
  Lude.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Lude.Text ->
  DeleteApp
mkDeleteApp pDomainId_ pUserProfileName_ pAppType_ pAppName_ =
  DeleteApp'
    { domainId = pDomainId_,
      userProfileName = pUserProfileName_,
      appType = pAppType_,
      appName = pAppName_
    }

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainId :: Lens.Lens' DeleteApp Lude.Text
dDomainId = Lens.lens (domainId :: DeleteApp -> Lude.Text) (\s a -> s {domainId = a} :: DeleteApp)
{-# DEPRECATED dDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserProfileName :: Lens.Lens' DeleteApp Lude.Text
dUserProfileName = Lens.lens (userProfileName :: DeleteApp -> Lude.Text) (\s a -> s {userProfileName = a} :: DeleteApp)
{-# DEPRECATED dUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppType :: Lens.Lens' DeleteApp AppType
dAppType = Lens.lens (appType :: DeleteApp -> AppType) (\s a -> s {appType = a} :: DeleteApp)
{-# DEPRECATED dAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppName :: Lens.Lens' DeleteApp Lude.Text
dAppName = Lens.lens (appName :: DeleteApp -> Lude.Text) (\s a -> s {appName = a} :: DeleteApp)
{-# DEPRECATED dAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

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
          [ Lude.Just ("DomainId" Lude..= domainId),
            Lude.Just ("UserProfileName" Lude..= userProfileName),
            Lude.Just ("AppType" Lude..= appType),
            Lude.Just ("AppName" Lude..= appName)
          ]
      )

instance Lude.ToPath DeleteApp where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
mkDeleteAppResponse ::
  DeleteAppResponse
mkDeleteAppResponse = DeleteAppResponse'
