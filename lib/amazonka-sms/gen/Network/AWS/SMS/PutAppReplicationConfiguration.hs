{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.PutAppReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the replication configuration for the specified application.
module Network.AWS.SMS.PutAppReplicationConfiguration
  ( -- * Creating a request
    PutAppReplicationConfiguration (..),
    mkPutAppReplicationConfiguration,

    -- ** Request lenses
    parcAppId,
    parcServerGroupReplicationConfigurations,

    -- * Destructuring the response
    PutAppReplicationConfigurationResponse (..),
    mkPutAppReplicationConfigurationResponse,

    -- ** Response lenses
    parcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkPutAppReplicationConfiguration' smart constructor.
data PutAppReplicationConfiguration = PutAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text,
    -- | Information about the replication configurations for server groups in the application.
    serverGroupReplicationConfigurations :: Lude.Maybe [ServerGroupReplicationConfiguration]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAppReplicationConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
-- * 'serverGroupReplicationConfigurations' - Information about the replication configurations for server groups in the application.
mkPutAppReplicationConfiguration ::
  PutAppReplicationConfiguration
mkPutAppReplicationConfiguration =
  PutAppReplicationConfiguration'
    { appId = Lude.Nothing,
      serverGroupReplicationConfigurations = Lude.Nothing
    }

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parcAppId :: Lens.Lens' PutAppReplicationConfiguration (Lude.Maybe Lude.Text)
parcAppId = Lens.lens (appId :: PutAppReplicationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: PutAppReplicationConfiguration)
{-# DEPRECATED parcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | Information about the replication configurations for server groups in the application.
--
-- /Note:/ Consider using 'serverGroupReplicationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parcServerGroupReplicationConfigurations :: Lens.Lens' PutAppReplicationConfiguration (Lude.Maybe [ServerGroupReplicationConfiguration])
parcServerGroupReplicationConfigurations = Lens.lens (serverGroupReplicationConfigurations :: PutAppReplicationConfiguration -> Lude.Maybe [ServerGroupReplicationConfiguration]) (\s a -> s {serverGroupReplicationConfigurations = a} :: PutAppReplicationConfiguration)
{-# DEPRECATED parcServerGroupReplicationConfigurations "Use generic-lens or generic-optics with 'serverGroupReplicationConfigurations' instead." #-}

instance Lude.AWSRequest PutAppReplicationConfiguration where
  type
    Rs PutAppReplicationConfiguration =
      PutAppReplicationConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutAppReplicationConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAppReplicationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.PutAppReplicationConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAppReplicationConfiguration where
  toJSON PutAppReplicationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("appId" Lude..=) Lude.<$> appId,
            ("serverGroupReplicationConfigurations" Lude..=)
              Lude.<$> serverGroupReplicationConfigurations
          ]
      )

instance Lude.ToPath PutAppReplicationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAppReplicationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAppReplicationConfigurationResponse' smart constructor.
newtype PutAppReplicationConfigurationResponse = PutAppReplicationConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAppReplicationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutAppReplicationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAppReplicationConfigurationResponse
mkPutAppReplicationConfigurationResponse pResponseStatus_ =
  PutAppReplicationConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parcrsResponseStatus :: Lens.Lens' PutAppReplicationConfigurationResponse Lude.Int
parcrsResponseStatus = Lens.lens (responseStatus :: PutAppReplicationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAppReplicationConfigurationResponse)
{-# DEPRECATED parcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
