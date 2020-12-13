{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetPortSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fleet's inbound connection permissions. Connection permissions specify the range of IP addresses and port settings that incoming traffic can use to access server processes in the fleet. Game sessions that are running on instances in the fleet use connections that fall in this range.
--
-- To get a fleet's inbound connection permissions, specify the fleet's unique identifier. If successful, a collection of 'IpPermission' objects is returned for the requested fleet ID. If the requested fleet has been deleted, the result set is empty.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
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
module Network.AWS.GameLift.DescribeFleetPortSettings
  ( -- * Creating a request
    DescribeFleetPortSettings (..),
    mkDescribeFleetPortSettings,

    -- ** Request lenses
    dfpsFleetId,

    -- * Destructuring the response
    DescribeFleetPortSettingsResponse (..),
    mkDescribeFleetPortSettingsResponse,

    -- ** Response lenses
    dfpsrsInboundPermissions,
    dfpsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeFleetPortSettings' smart constructor.
newtype DescribeFleetPortSettings = DescribeFleetPortSettings'
  { -- | A unique identifier for a fleet to retrieve port settings for. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetPortSettings' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet to retrieve port settings for. You can use either the fleet ID or ARN value.
mkDescribeFleetPortSettings ::
  -- | 'fleetId'
  Lude.Text ->
  DescribeFleetPortSettings
mkDescribeFleetPortSettings pFleetId_ =
  DescribeFleetPortSettings' {fleetId = pFleetId_}

-- | A unique identifier for a fleet to retrieve port settings for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfpsFleetId :: Lens.Lens' DescribeFleetPortSettings Lude.Text
dfpsFleetId = Lens.lens (fleetId :: DescribeFleetPortSettings -> Lude.Text) (\s a -> s {fleetId = a} :: DescribeFleetPortSettings)
{-# DEPRECATED dfpsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest DescribeFleetPortSettings where
  type
    Rs DescribeFleetPortSettings =
      DescribeFleetPortSettingsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFleetPortSettingsResponse'
            Lude.<$> (x Lude..?> "InboundPermissions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleetPortSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeFleetPortSettings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeFleetPortSettings where
  toJSON DescribeFleetPortSettings' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("FleetId" Lude..= fleetId)])

instance Lude.ToPath DescribeFleetPortSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleetPortSettings where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetPortSettingsResponse' smart constructor.
data DescribeFleetPortSettingsResponse = DescribeFleetPortSettingsResponse'
  { -- | The port settings for the requested fleet ID.
    inboundPermissions :: Lude.Maybe [IPPermission],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetPortSettingsResponse' with the minimum fields required to make a request.
--
-- * 'inboundPermissions' - The port settings for the requested fleet ID.
-- * 'responseStatus' - The response status code.
mkDescribeFleetPortSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetPortSettingsResponse
mkDescribeFleetPortSettingsResponse pResponseStatus_ =
  DescribeFleetPortSettingsResponse'
    { inboundPermissions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The port settings for the requested fleet ID.
--
-- /Note:/ Consider using 'inboundPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfpsrsInboundPermissions :: Lens.Lens' DescribeFleetPortSettingsResponse (Lude.Maybe [IPPermission])
dfpsrsInboundPermissions = Lens.lens (inboundPermissions :: DescribeFleetPortSettingsResponse -> Lude.Maybe [IPPermission]) (\s a -> s {inboundPermissions = a} :: DescribeFleetPortSettingsResponse)
{-# DEPRECATED dfpsrsInboundPermissions "Use generic-lens or generic-optics with 'inboundPermissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfpsrsResponseStatus :: Lens.Lens' DescribeFleetPortSettingsResponse Lude.Int
dfpsrsResponseStatus = Lens.lens (responseStatus :: DescribeFleetPortSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetPortSettingsResponse)
{-# DEPRECATED dfpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
