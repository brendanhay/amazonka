{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateFleetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates fleet properties, including name and description, for a fleet. To update metadata, specify the fleet ID and the property values that you want to change. If successful, the fleet ID for the updated fleet is returned.
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
module Network.AWS.GameLift.UpdateFleetAttributes
  ( -- * Creating a request
    UpdateFleetAttributes (..),
    mkUpdateFleetAttributes,

    -- ** Request lenses
    ufaNewGameSessionProtectionPolicy,
    ufaName,
    ufaMetricGroups,
    ufaFleetId,
    ufaDescription,
    ufaResourceCreationLimitPolicy,

    -- * Destructuring the response
    UpdateFleetAttributesResponse (..),
    mkUpdateFleetAttributesResponse,

    -- ** Response lenses
    ufarsFleetId,
    ufarsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateFleetAttributes' smart constructor.
data UpdateFleetAttributes = UpdateFleetAttributes'
  { -- | Game session protection policy to apply to all new instances created in this fleet. Instances that already exist are not affected. You can set protection for individual instances using 'UpdateGameSession' .
    --
    --
    --     * __NoProtection__ -- The game session can be terminated during a scale-down event.
    --
    --
    --     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy :: Lude.Maybe ProtectionPolicy,
    -- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | Names of metric groups to include this fleet in. Amazon CloudWatch uses a fleet metric group is to aggregate metrics from multiple fleets. Use an existing metric group name to add this fleet to the group. Or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
    metricGroups :: Lude.Maybe [Lude.Text],
    -- | A unique identifier for a fleet to update attribute metadata for. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Text,
    -- | Human-readable description of a fleet.
    description :: Lude.Maybe Lude.Text,
    -- | Policy that limits the number of game sessions an individual player can create over a span of time.
    resourceCreationLimitPolicy :: Lude.Maybe ResourceCreationLimitPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFleetAttributes' with the minimum fields required to make a request.
--
-- * 'newGameSessionProtectionPolicy' - Game session protection policy to apply to all new instances created in this fleet. Instances that already exist are not affected. You can set protection for individual instances using 'UpdateGameSession' .
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
-- * 'name' - A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
-- * 'metricGroups' - Names of metric groups to include this fleet in. Amazon CloudWatch uses a fleet metric group is to aggregate metrics from multiple fleets. Use an existing metric group name to add this fleet to the group. Or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
-- * 'fleetId' - A unique identifier for a fleet to update attribute metadata for. You can use either the fleet ID or ARN value.
-- * 'description' - Human-readable description of a fleet.
-- * 'resourceCreationLimitPolicy' - Policy that limits the number of game sessions an individual player can create over a span of time.
mkUpdateFleetAttributes ::
  -- | 'fleetId'
  Lude.Text ->
  UpdateFleetAttributes
mkUpdateFleetAttributes pFleetId_ =
  UpdateFleetAttributes'
    { newGameSessionProtectionPolicy =
        Lude.Nothing,
      name = Lude.Nothing,
      metricGroups = Lude.Nothing,
      fleetId = pFleetId_,
      description = Lude.Nothing,
      resourceCreationLimitPolicy = Lude.Nothing
    }

-- | Game session protection policy to apply to all new instances created in this fleet. Instances that already exist are not affected. You can set protection for individual instances using 'UpdateGameSession' .
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
--
-- /Note:/ Consider using 'newGameSessionProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaNewGameSessionProtectionPolicy :: Lens.Lens' UpdateFleetAttributes (Lude.Maybe ProtectionPolicy)
ufaNewGameSessionProtectionPolicy = Lens.lens (newGameSessionProtectionPolicy :: UpdateFleetAttributes -> Lude.Maybe ProtectionPolicy) (\s a -> s {newGameSessionProtectionPolicy = a} :: UpdateFleetAttributes)
{-# DEPRECATED ufaNewGameSessionProtectionPolicy "Use generic-lens or generic-optics with 'newGameSessionProtectionPolicy' instead." #-}

-- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaName :: Lens.Lens' UpdateFleetAttributes (Lude.Maybe Lude.Text)
ufaName = Lens.lens (name :: UpdateFleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateFleetAttributes)
{-# DEPRECATED ufaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Names of metric groups to include this fleet in. Amazon CloudWatch uses a fleet metric group is to aggregate metrics from multiple fleets. Use an existing metric group name to add this fleet to the group. Or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
--
-- /Note:/ Consider using 'metricGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaMetricGroups :: Lens.Lens' UpdateFleetAttributes (Lude.Maybe [Lude.Text])
ufaMetricGroups = Lens.lens (metricGroups :: UpdateFleetAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {metricGroups = a} :: UpdateFleetAttributes)
{-# DEPRECATED ufaMetricGroups "Use generic-lens or generic-optics with 'metricGroups' instead." #-}

-- | A unique identifier for a fleet to update attribute metadata for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaFleetId :: Lens.Lens' UpdateFleetAttributes Lude.Text
ufaFleetId = Lens.lens (fleetId :: UpdateFleetAttributes -> Lude.Text) (\s a -> s {fleetId = a} :: UpdateFleetAttributes)
{-# DEPRECATED ufaFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Human-readable description of a fleet.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaDescription :: Lens.Lens' UpdateFleetAttributes (Lude.Maybe Lude.Text)
ufaDescription = Lens.lens (description :: UpdateFleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateFleetAttributes)
{-# DEPRECATED ufaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Policy that limits the number of game sessions an individual player can create over a span of time.
--
-- /Note:/ Consider using 'resourceCreationLimitPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaResourceCreationLimitPolicy :: Lens.Lens' UpdateFleetAttributes (Lude.Maybe ResourceCreationLimitPolicy)
ufaResourceCreationLimitPolicy = Lens.lens (resourceCreationLimitPolicy :: UpdateFleetAttributes -> Lude.Maybe ResourceCreationLimitPolicy) (\s a -> s {resourceCreationLimitPolicy = a} :: UpdateFleetAttributes)
{-# DEPRECATED ufaResourceCreationLimitPolicy "Use generic-lens or generic-optics with 'resourceCreationLimitPolicy' instead." #-}

instance Lude.AWSRequest UpdateFleetAttributes where
  type Rs UpdateFleetAttributes = UpdateFleetAttributesResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateFleetAttributesResponse'
            Lude.<$> (x Lude..?> "FleetId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateFleetAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateFleetAttributes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateFleetAttributes where
  toJSON UpdateFleetAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NewGameSessionProtectionPolicy" Lude..=)
              Lude.<$> newGameSessionProtectionPolicy,
            ("Name" Lude..=) Lude.<$> name,
            ("MetricGroups" Lude..=) Lude.<$> metricGroups,
            Lude.Just ("FleetId" Lude..= fleetId),
            ("Description" Lude..=) Lude.<$> description,
            ("ResourceCreationLimitPolicy" Lude..=)
              Lude.<$> resourceCreationLimitPolicy
          ]
      )

instance Lude.ToPath UpdateFleetAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateFleetAttributes where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateFleetAttributesResponse' smart constructor.
data UpdateFleetAttributesResponse = UpdateFleetAttributesResponse'
  { -- | A unique identifier for a fleet that was updated. Use either the fleet ID or ARN value.
    fleetId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFleetAttributesResponse' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet that was updated. Use either the fleet ID or ARN value.
-- * 'responseStatus' - The response status code.
mkUpdateFleetAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFleetAttributesResponse
mkUpdateFleetAttributesResponse pResponseStatus_ =
  UpdateFleetAttributesResponse'
    { fleetId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier for a fleet that was updated. Use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufarsFleetId :: Lens.Lens' UpdateFleetAttributesResponse (Lude.Maybe Lude.Text)
ufarsFleetId = Lens.lens (fleetId :: UpdateFleetAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: UpdateFleetAttributesResponse)
{-# DEPRECATED ufarsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufarsResponseStatus :: Lens.Lens' UpdateFleetAttributesResponse Lude.Int
ufarsResponseStatus = Lens.lens (responseStatus :: UpdateFleetAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFleetAttributesResponse)
{-# DEPRECATED ufarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
