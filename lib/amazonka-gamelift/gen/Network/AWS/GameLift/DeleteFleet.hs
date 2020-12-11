{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes everything related to a fleet. Before deleting a fleet, you must set the fleet's desired capacity to zero. See 'UpdateFleetCapacity' .
--
-- If the fleet being deleted has a VPC peering connection, you first need to get a valid authorization (good for 24 hours) by calling 'CreateVpcPeeringAuthorization' . You do not need to explicitly delete the VPC peering connection--this is done as part of the delete fleet process.
-- This operation removes the fleet and its resources. Once a fleet is deleted, you can no longer use any of the resource in that fleet.
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
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
module Network.AWS.GameLift.DeleteFleet
  ( -- * Creating a request
    DeleteFleet (..),
    mkDeleteFleet,

    -- ** Request lenses
    dfFleetId,

    -- * Destructuring the response
    DeleteFleetResponse (..),
    mkDeleteFleetResponse,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteFleet' smart constructor.
newtype DeleteFleet = DeleteFleet' {fleetId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFleet' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
mkDeleteFleet ::
  -- | 'fleetId'
  Lude.Text ->
  DeleteFleet
mkDeleteFleet pFleetId_ = DeleteFleet' {fleetId = pFleetId_}

-- | A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFleetId :: Lens.Lens' DeleteFleet Lude.Text
dfFleetId = Lens.lens (fleetId :: DeleteFleet -> Lude.Text) (\s a -> s {fleetId = a} :: DeleteFleet)
{-# DEPRECATED dfFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest DeleteFleet where
  type Rs DeleteFleet = DeleteFleetResponse
  request = Req.postJSON gameLiftService
  response = Res.receiveNull DeleteFleetResponse'

instance Lude.ToHeaders DeleteFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteFleet where
  toJSON DeleteFleet' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("FleetId" Lude..= fleetId)])

instance Lude.ToPath DeleteFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFleetResponse' smart constructor.
data DeleteFleetResponse = DeleteFleetResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFleetResponse' with the minimum fields required to make a request.
mkDeleteFleetResponse ::
  DeleteFleetResponse
mkDeleteFleetResponse = DeleteFleetResponse'
