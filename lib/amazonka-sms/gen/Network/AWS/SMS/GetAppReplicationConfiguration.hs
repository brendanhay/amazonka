{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application replication configuration associated with the specified application.
module Network.AWS.SMS.GetAppReplicationConfiguration
  ( -- * Creating a request
    GetAppReplicationConfiguration (..),
    mkGetAppReplicationConfiguration,

    -- ** Request lenses
    garcAppId,

    -- * Destructuring the response
    GetAppReplicationConfigurationResponse (..),
    mkGetAppReplicationConfigurationResponse,

    -- ** Response lenses
    garcrsServerGroupReplicationConfigurations,
    garcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetAppReplicationConfiguration' smart constructor.
newtype GetAppReplicationConfiguration = GetAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppReplicationConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkGetAppReplicationConfiguration ::
  GetAppReplicationConfiguration
mkGetAppReplicationConfiguration =
  GetAppReplicationConfiguration' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcAppId :: Lens.Lens' GetAppReplicationConfiguration (Lude.Maybe Lude.Text)
garcAppId = Lens.lens (appId :: GetAppReplicationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: GetAppReplicationConfiguration)
{-# DEPRECATED garcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest GetAppReplicationConfiguration where
  type
    Rs GetAppReplicationConfiguration =
      GetAppReplicationConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppReplicationConfigurationResponse'
            Lude.<$> ( x Lude..?> "serverGroupReplicationConfigurations"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAppReplicationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetAppReplicationConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAppReplicationConfiguration where
  toJSON GetAppReplicationConfiguration' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath GetAppReplicationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAppReplicationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAppReplicationConfigurationResponse' smart constructor.
data GetAppReplicationConfigurationResponse = GetAppReplicationConfigurationResponse'
  { -- | The replication configurations associated with server groups in this application.
    serverGroupReplicationConfigurations :: Lude.Maybe [ServerGroupReplicationConfiguration],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppReplicationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'serverGroupReplicationConfigurations' - The replication configurations associated with server groups in this application.
-- * 'responseStatus' - The response status code.
mkGetAppReplicationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAppReplicationConfigurationResponse
mkGetAppReplicationConfigurationResponse pResponseStatus_ =
  GetAppReplicationConfigurationResponse'
    { serverGroupReplicationConfigurations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication configurations associated with server groups in this application.
--
-- /Note:/ Consider using 'serverGroupReplicationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrsServerGroupReplicationConfigurations :: Lens.Lens' GetAppReplicationConfigurationResponse (Lude.Maybe [ServerGroupReplicationConfiguration])
garcrsServerGroupReplicationConfigurations = Lens.lens (serverGroupReplicationConfigurations :: GetAppReplicationConfigurationResponse -> Lude.Maybe [ServerGroupReplicationConfiguration]) (\s a -> s {serverGroupReplicationConfigurations = a} :: GetAppReplicationConfigurationResponse)
{-# DEPRECATED garcrsServerGroupReplicationConfigurations "Use generic-lens or generic-optics with 'serverGroupReplicationConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrsResponseStatus :: Lens.Lens' GetAppReplicationConfigurationResponse Lude.Int
garcrsResponseStatus = Lens.lens (responseStatus :: GetAppReplicationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppReplicationConfigurationResponse)
{-# DEPRECATED garcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
