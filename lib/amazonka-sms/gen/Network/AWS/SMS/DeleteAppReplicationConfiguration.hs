{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteAppReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the replication configuration for the specified application.
module Network.AWS.SMS.DeleteAppReplicationConfiguration
  ( -- * Creating a request
    DeleteAppReplicationConfiguration (..),
    mkDeleteAppReplicationConfiguration,

    -- ** Request lenses
    darcAppId,

    -- * Destructuring the response
    DeleteAppReplicationConfigurationResponse (..),
    mkDeleteAppReplicationConfigurationResponse,

    -- ** Response lenses
    darcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkDeleteAppReplicationConfiguration' smart constructor.
newtype DeleteAppReplicationConfiguration = DeleteAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppReplicationConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkDeleteAppReplicationConfiguration ::
  DeleteAppReplicationConfiguration
mkDeleteAppReplicationConfiguration =
  DeleteAppReplicationConfiguration' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darcAppId :: Lens.Lens' DeleteAppReplicationConfiguration (Lude.Maybe Lude.Text)
darcAppId = Lens.lens (appId :: DeleteAppReplicationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: DeleteAppReplicationConfiguration)
{-# DEPRECATED darcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest DeleteAppReplicationConfiguration where
  type
    Rs DeleteAppReplicationConfiguration =
      DeleteAppReplicationConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAppReplicationConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAppReplicationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppReplicationConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAppReplicationConfiguration where
  toJSON DeleteAppReplicationConfiguration' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath DeleteAppReplicationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAppReplicationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppReplicationConfigurationResponse' smart constructor.
newtype DeleteAppReplicationConfigurationResponse = DeleteAppReplicationConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppReplicationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAppReplicationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAppReplicationConfigurationResponse
mkDeleteAppReplicationConfigurationResponse pResponseStatus_ =
  DeleteAppReplicationConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darcrsResponseStatus :: Lens.Lens' DeleteAppReplicationConfigurationResponse Lude.Int
darcrsResponseStatus = Lens.lens (responseStatus :: DeleteAppReplicationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAppReplicationConfigurationResponse)
{-# DEPRECATED darcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
