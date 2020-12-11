{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fleet's runtime configuration settings. The runtime configuration tells Amazon GameLift which server processes to run (and how) on each instance in the fleet.
--
-- To get a runtime configuration, specify the fleet's unique identifier. If successful, a 'RuntimeConfiguration' object is returned for the requested fleet. If the requested fleet has been deleted, the result set is empty.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-multiprocess.html Running Multiple Processes on a Fleet>
-- __Related operations__
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'DescribeFleetPortSettings'
--
--
--     * 'DescribeFleetUtilization'
--
--
--     * 'DescribeRuntimeConfiguration'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * 'DescribeFleetEvents'
--
--
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
module Network.AWS.GameLift.DescribeRuntimeConfiguration
  ( -- * Creating a request
    DescribeRuntimeConfiguration (..),
    mkDescribeRuntimeConfiguration,

    -- ** Request lenses
    drcFleetId,

    -- * Destructuring the response
    DescribeRuntimeConfigurationResponse (..),
    mkDescribeRuntimeConfigurationResponse,

    -- ** Response lenses
    drcrsRuntimeConfiguration,
    drcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeRuntimeConfiguration' smart constructor.
newtype DescribeRuntimeConfiguration = DescribeRuntimeConfiguration'
  { fleetId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRuntimeConfiguration' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet to get the runtime configuration for. You can use either the fleet ID or ARN value.
mkDescribeRuntimeConfiguration ::
  -- | 'fleetId'
  Lude.Text ->
  DescribeRuntimeConfiguration
mkDescribeRuntimeConfiguration pFleetId_ =
  DescribeRuntimeConfiguration' {fleetId = pFleetId_}

-- | A unique identifier for a fleet to get the runtime configuration for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcFleetId :: Lens.Lens' DescribeRuntimeConfiguration Lude.Text
drcFleetId = Lens.lens (fleetId :: DescribeRuntimeConfiguration -> Lude.Text) (\s a -> s {fleetId = a} :: DescribeRuntimeConfiguration)
{-# DEPRECATED drcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest DescribeRuntimeConfiguration where
  type
    Rs DescribeRuntimeConfiguration =
      DescribeRuntimeConfigurationResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRuntimeConfigurationResponse'
            Lude.<$> (x Lude..?> "RuntimeConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRuntimeConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeRuntimeConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRuntimeConfiguration where
  toJSON DescribeRuntimeConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("FleetId" Lude..= fleetId)])

instance Lude.ToPath DescribeRuntimeConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRuntimeConfiguration where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeRuntimeConfigurationResponse' smart constructor.
data DescribeRuntimeConfigurationResponse = DescribeRuntimeConfigurationResponse'
  { runtimeConfiguration ::
      Lude.Maybe
        RuntimeConfiguration,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRuntimeConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'runtimeConfiguration' - Instructions describing how server processes should be launched and maintained on each instance in the fleet.
mkDescribeRuntimeConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRuntimeConfigurationResponse
mkDescribeRuntimeConfigurationResponse pResponseStatus_ =
  DescribeRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Instructions describing how server processes should be launched and maintained on each instance in the fleet.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsRuntimeConfiguration :: Lens.Lens' DescribeRuntimeConfigurationResponse (Lude.Maybe RuntimeConfiguration)
drcrsRuntimeConfiguration = Lens.lens (runtimeConfiguration :: DescribeRuntimeConfigurationResponse -> Lude.Maybe RuntimeConfiguration) (\s a -> s {runtimeConfiguration = a} :: DescribeRuntimeConfigurationResponse)
{-# DEPRECATED drcrsRuntimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsResponseStatus :: Lens.Lens' DescribeRuntimeConfigurationResponse Lude.Int
drcrsResponseStatus = Lens.lens (responseStatus :: DescribeRuntimeConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRuntimeConfigurationResponse)
{-# DEPRECATED drcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
