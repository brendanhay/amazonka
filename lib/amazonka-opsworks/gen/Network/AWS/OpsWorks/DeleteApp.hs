{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified app.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeleteApp
  ( -- * Creating a request
    DeleteApp (..),
    mkDeleteApp,

    -- ** Request lenses
    daAppId,

    -- * Destructuring the response
    DeleteAppResponse (..),
    mkDeleteAppResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteApp' smart constructor.
newtype DeleteApp = DeleteApp'
  { -- | The app ID.
    appId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- * 'appId' - The app ID.
mkDeleteApp ::
  -- | 'appId'
  Lude.Text ->
  DeleteApp
mkDeleteApp pAppId_ = DeleteApp' {appId = pAppId_}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppId :: Lens.Lens' DeleteApp Lude.Text
daAppId = Lens.lens (appId :: DeleteApp -> Lude.Text) (\s a -> s {appId = a} :: DeleteApp)
{-# DEPRECATED daAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeleteAppResponse'

instance Lude.ToHeaders DeleteApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeleteApp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("AppId" Lude..= appId)])

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
