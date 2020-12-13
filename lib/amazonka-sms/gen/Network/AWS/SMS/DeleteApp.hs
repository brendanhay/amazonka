{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Optionally deletes the launched stack associated with the application and all AWS SMS replication jobs for servers in the application.
module Network.AWS.SMS.DeleteApp
  ( -- * Creating a request
    DeleteApp (..),
    mkDeleteApp,

    -- ** Request lenses
    daForceTerminateApp,
    daAppId,
    daForceStopAppReplication,

    -- * Destructuring the response
    DeleteAppResponse (..),
    mkDeleteAppResponse,

    -- ** Response lenses
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | Indicates whether to terminate the stack corresponding to the application while deleting the application.
    forceTerminateApp :: Lude.Maybe Lude.Bool,
    -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text,
    -- | Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
    forceStopAppReplication :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- * 'forceTerminateApp' - Indicates whether to terminate the stack corresponding to the application while deleting the application.
-- * 'appId' - The ID of the application.
-- * 'forceStopAppReplication' - Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
mkDeleteApp ::
  DeleteApp
mkDeleteApp =
  DeleteApp'
    { forceTerminateApp = Lude.Nothing,
      appId = Lude.Nothing,
      forceStopAppReplication = Lude.Nothing
    }

-- | Indicates whether to terminate the stack corresponding to the application while deleting the application.
--
-- /Note:/ Consider using 'forceTerminateApp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daForceTerminateApp :: Lens.Lens' DeleteApp (Lude.Maybe Lude.Bool)
daForceTerminateApp = Lens.lens (forceTerminateApp :: DeleteApp -> Lude.Maybe Lude.Bool) (\s a -> s {forceTerminateApp = a} :: DeleteApp)
{-# DEPRECATED daForceTerminateApp "Use generic-lens or generic-optics with 'forceTerminateApp' instead." #-}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppId :: Lens.Lens' DeleteApp (Lude.Maybe Lude.Text)
daAppId = Lens.lens (appId :: DeleteApp -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: DeleteApp)
{-# DEPRECATED daAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
--
-- /Note:/ Consider using 'forceStopAppReplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daForceStopAppReplication :: Lens.Lens' DeleteApp (Lude.Maybe Lude.Bool)
daForceStopAppReplication = Lens.lens (forceStopAppReplication :: DeleteApp -> Lude.Maybe Lude.Bool) (\s a -> s {forceStopAppReplication = a} :: DeleteApp)
{-# DEPRECATED daForceStopAppReplication "Use generic-lens or generic-optics with 'forceStopAppReplication' instead." #-}

instance Lude.AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAppResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteApp" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("forceTerminateApp" Lude..=) Lude.<$> forceTerminateApp,
            ("appId" Lude..=) Lude.<$> appId,
            ("forceStopAppReplication" Lude..=)
              Lude.<$> forceStopAppReplication
          ]
      )

instance Lude.ToPath DeleteApp where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppResponse' smart constructor.
newtype DeleteAppResponse = DeleteAppResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAppResponse
mkDeleteAppResponse pResponseStatus_ =
  DeleteAppResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DeleteAppResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DeleteAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAppResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
