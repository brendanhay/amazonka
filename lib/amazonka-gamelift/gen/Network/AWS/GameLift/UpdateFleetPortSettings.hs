{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateFleetPortSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates port settings for a fleet. To update settings, specify the fleet ID to be updated and list the permissions you want to update. List the permissions you want to add in @InboundPermissionAuthorizations@ , and permissions you want to remove in @InboundPermissionRevocations@ . Permissions to be removed must match existing fleet permissions. If successful, the fleet ID for the updated fleet is returned.
--
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
--     * 'DescribeFleetAttributes'
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'UpdateFleetCapacity'
--
--
--     * 'UpdateFleetPortSettings'
--
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
module Network.AWS.GameLift.UpdateFleetPortSettings
  ( -- * Creating a request
    UpdateFleetPortSettings (..),
    mkUpdateFleetPortSettings,

    -- ** Request lenses
    ufpsInboundPermissionRevocations,
    ufpsInboundPermissionAuthorizations,
    ufpsFleetId,

    -- * Destructuring the response
    UpdateFleetPortSettingsResponse (..),
    mkUpdateFleetPortSettingsResponse,

    -- ** Response lenses
    ufpsrsFleetId,
    ufpsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateFleetPortSettings' smart constructor.
data UpdateFleetPortSettings = UpdateFleetPortSettings'
  { -- | A collection of port settings to be removed from the fleet resource.
    inboundPermissionRevocations :: Lude.Maybe [IPPermission],
    -- | A collection of port settings to be added to the fleet resource.
    inboundPermissionAuthorizations :: Lude.Maybe [IPPermission],
    -- | A unique identifier for a fleet to update port settings for. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFleetPortSettings' with the minimum fields required to make a request.
--
-- * 'inboundPermissionRevocations' - A collection of port settings to be removed from the fleet resource.
-- * 'inboundPermissionAuthorizations' - A collection of port settings to be added to the fleet resource.
-- * 'fleetId' - A unique identifier for a fleet to update port settings for. You can use either the fleet ID or ARN value.
mkUpdateFleetPortSettings ::
  -- | 'fleetId'
  Lude.Text ->
  UpdateFleetPortSettings
mkUpdateFleetPortSettings pFleetId_ =
  UpdateFleetPortSettings'
    { inboundPermissionRevocations =
        Lude.Nothing,
      inboundPermissionAuthorizations = Lude.Nothing,
      fleetId = pFleetId_
    }

-- | A collection of port settings to be removed from the fleet resource.
--
-- /Note:/ Consider using 'inboundPermissionRevocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsInboundPermissionRevocations :: Lens.Lens' UpdateFleetPortSettings (Lude.Maybe [IPPermission])
ufpsInboundPermissionRevocations = Lens.lens (inboundPermissionRevocations :: UpdateFleetPortSettings -> Lude.Maybe [IPPermission]) (\s a -> s {inboundPermissionRevocations = a} :: UpdateFleetPortSettings)
{-# DEPRECATED ufpsInboundPermissionRevocations "Use generic-lens or generic-optics with 'inboundPermissionRevocations' instead." #-}

-- | A collection of port settings to be added to the fleet resource.
--
-- /Note:/ Consider using 'inboundPermissionAuthorizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsInboundPermissionAuthorizations :: Lens.Lens' UpdateFleetPortSettings (Lude.Maybe [IPPermission])
ufpsInboundPermissionAuthorizations = Lens.lens (inboundPermissionAuthorizations :: UpdateFleetPortSettings -> Lude.Maybe [IPPermission]) (\s a -> s {inboundPermissionAuthorizations = a} :: UpdateFleetPortSettings)
{-# DEPRECATED ufpsInboundPermissionAuthorizations "Use generic-lens or generic-optics with 'inboundPermissionAuthorizations' instead." #-}

-- | A unique identifier for a fleet to update port settings for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsFleetId :: Lens.Lens' UpdateFleetPortSettings Lude.Text
ufpsFleetId = Lens.lens (fleetId :: UpdateFleetPortSettings -> Lude.Text) (\s a -> s {fleetId = a} :: UpdateFleetPortSettings)
{-# DEPRECATED ufpsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest UpdateFleetPortSettings where
  type Rs UpdateFleetPortSettings = UpdateFleetPortSettingsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateFleetPortSettingsResponse'
            Lude.<$> (x Lude..?> "FleetId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateFleetPortSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateFleetPortSettings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateFleetPortSettings where
  toJSON UpdateFleetPortSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InboundPermissionRevocations" Lude..=)
              Lude.<$> inboundPermissionRevocations,
            ("InboundPermissionAuthorizations" Lude..=)
              Lude.<$> inboundPermissionAuthorizations,
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath UpdateFleetPortSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateFleetPortSettings where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateFleetPortSettingsResponse' smart constructor.
data UpdateFleetPortSettingsResponse = UpdateFleetPortSettingsResponse'
  { -- | A unique identifier for a fleet that was updated.
    fleetId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFleetPortSettingsResponse' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet that was updated.
-- * 'responseStatus' - The response status code.
mkUpdateFleetPortSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFleetPortSettingsResponse
mkUpdateFleetPortSettingsResponse pResponseStatus_ =
  UpdateFleetPortSettingsResponse'
    { fleetId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier for a fleet that was updated.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsrsFleetId :: Lens.Lens' UpdateFleetPortSettingsResponse (Lude.Maybe Lude.Text)
ufpsrsFleetId = Lens.lens (fleetId :: UpdateFleetPortSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: UpdateFleetPortSettingsResponse)
{-# DEPRECATED ufpsrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsrsResponseStatus :: Lens.Lens' UpdateFleetPortSettingsResponse Lude.Int
ufpsrsResponseStatus = Lens.lens (responseStatus :: UpdateFleetPortSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFleetPortSettingsResponse)
{-# DEPRECATED ufpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
