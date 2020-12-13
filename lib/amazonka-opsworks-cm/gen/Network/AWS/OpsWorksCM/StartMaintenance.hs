{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.StartMaintenance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manually starts server maintenance. This command can be useful if an earlier maintenance attempt failed, and the underlying cause of maintenance failure has been resolved. The server is in an @UNDER_MAINTENANCE@ state while maintenance is in progress.
--
-- Maintenance can only be started on servers in @HEALTHY@ and @UNHEALTHY@ states. Otherwise, an @InvalidStateException@ is thrown. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.StartMaintenance
  ( -- * Creating a request
    StartMaintenance (..),
    mkStartMaintenance,

    -- ** Request lenses
    smServerName,
    smEngineAttributes,

    -- * Destructuring the response
    StartMaintenanceResponse (..),
    mkStartMaintenanceResponse,

    -- ** Response lenses
    smrsServer,
    smrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartMaintenance' smart constructor.
data StartMaintenance = StartMaintenance'
  { -- | The name of the server on which to run maintenance.
    serverName :: Lude.Text,
    -- | Engine attributes that are specific to the server on which you want to run maintenance.
    --
    -- __Attributes accepted in a StartMaintenance request for Chef__
    --
    --     * @CHEF_MAJOR_UPGRADE@ : If a Chef Automate server is eligible for upgrade to Chef Automate 2, add this engine attribute to a @StartMaintenance@ request and set the value to @true@ to upgrade the server to Chef Automate 2. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opscm-a2upgrade.html Upgrade an AWS OpsWorks for Chef Automate Server to Chef Automate 2> .
    engineAttributes :: Lude.Maybe [EngineAttribute]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMaintenance' with the minimum fields required to make a request.
--
-- * 'serverName' - The name of the server on which to run maintenance.
-- * 'engineAttributes' - Engine attributes that are specific to the server on which you want to run maintenance.
--
-- __Attributes accepted in a StartMaintenance request for Chef__
--
--     * @CHEF_MAJOR_UPGRADE@ : If a Chef Automate server is eligible for upgrade to Chef Automate 2, add this engine attribute to a @StartMaintenance@ request and set the value to @true@ to upgrade the server to Chef Automate 2. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opscm-a2upgrade.html Upgrade an AWS OpsWorks for Chef Automate Server to Chef Automate 2> .
mkStartMaintenance ::
  -- | 'serverName'
  Lude.Text ->
  StartMaintenance
mkStartMaintenance pServerName_ =
  StartMaintenance'
    { serverName = pServerName_,
      engineAttributes = Lude.Nothing
    }

-- | The name of the server on which to run maintenance.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smServerName :: Lens.Lens' StartMaintenance Lude.Text
smServerName = Lens.lens (serverName :: StartMaintenance -> Lude.Text) (\s a -> s {serverName = a} :: StartMaintenance)
{-# DEPRECATED smServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Engine attributes that are specific to the server on which you want to run maintenance.
--
-- __Attributes accepted in a StartMaintenance request for Chef__
--
--     * @CHEF_MAJOR_UPGRADE@ : If a Chef Automate server is eligible for upgrade to Chef Automate 2, add this engine attribute to a @StartMaintenance@ request and set the value to @true@ to upgrade the server to Chef Automate 2. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opscm-a2upgrade.html Upgrade an AWS OpsWorks for Chef Automate Server to Chef Automate 2> .
--
--
--
-- /Note:/ Consider using 'engineAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smEngineAttributes :: Lens.Lens' StartMaintenance (Lude.Maybe [EngineAttribute])
smEngineAttributes = Lens.lens (engineAttributes :: StartMaintenance -> Lude.Maybe [EngineAttribute]) (\s a -> s {engineAttributes = a} :: StartMaintenance)
{-# DEPRECATED smEngineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead." #-}

instance Lude.AWSRequest StartMaintenance where
  type Rs StartMaintenance = StartMaintenanceResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMaintenanceResponse'
            Lude.<$> (x Lude..?> "Server") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMaintenance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.StartMaintenance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMaintenance where
  toJSON StartMaintenance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServerName" Lude..= serverName),
            ("EngineAttributes" Lude..=) Lude.<$> engineAttributes
          ]
      )

instance Lude.ToPath StartMaintenance where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMaintenance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartMaintenanceResponse' smart constructor.
data StartMaintenanceResponse = StartMaintenanceResponse'
  { -- | Contains the response to a @StartMaintenance@ request.
    server :: Lude.Maybe Server,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMaintenanceResponse' with the minimum fields required to make a request.
--
-- * 'server' - Contains the response to a @StartMaintenance@ request.
-- * 'responseStatus' - The response status code.
mkStartMaintenanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMaintenanceResponse
mkStartMaintenanceResponse pResponseStatus_ =
  StartMaintenanceResponse'
    { server = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the response to a @StartMaintenance@ request.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsServer :: Lens.Lens' StartMaintenanceResponse (Lude.Maybe Server)
smrsServer = Lens.lens (server :: StartMaintenanceResponse -> Lude.Maybe Server) (\s a -> s {server = a} :: StartMaintenanceResponse)
{-# DEPRECATED smrsServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsResponseStatus :: Lens.Lens' StartMaintenanceResponse Lude.Int
smrsResponseStatus = Lens.lens (responseStatus :: StartMaintenanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMaintenanceResponse)
{-# DEPRECATED smrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
