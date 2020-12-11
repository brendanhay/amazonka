{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.AssociateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified fleet with the specified stack.
module Network.AWS.AppStream.AssociateFleet
  ( -- * Creating a request
    AssociateFleet (..),
    mkAssociateFleet,

    -- ** Request lenses
    afFleetName,
    afStackName,

    -- * Destructuring the response
    AssociateFleetResponse (..),
    mkAssociateFleetResponse,

    -- ** Response lenses
    afrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateFleet' smart constructor.
data AssociateFleet = AssociateFleet'
  { fleetName :: Lude.Text,
    stackName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateFleet' with the minimum fields required to make a request.
--
-- * 'fleetName' - The name of the fleet.
-- * 'stackName' - The name of the stack.
mkAssociateFleet ::
  -- | 'fleetName'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  AssociateFleet
mkAssociateFleet pFleetName_ pStackName_ =
  AssociateFleet' {fleetName = pFleetName_, stackName = pStackName_}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afFleetName :: Lens.Lens' AssociateFleet Lude.Text
afFleetName = Lens.lens (fleetName :: AssociateFleet -> Lude.Text) (\s a -> s {fleetName = a} :: AssociateFleet)
{-# DEPRECATED afFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afStackName :: Lens.Lens' AssociateFleet Lude.Text
afStackName = Lens.lens (stackName :: AssociateFleet -> Lude.Text) (\s a -> s {stackName = a} :: AssociateFleet)
{-# DEPRECATED afStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest AssociateFleet where
  type Rs AssociateFleet = AssociateFleetResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateFleetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.AssociateFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateFleet where
  toJSON AssociateFleet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FleetName" Lude..= fleetName),
            Lude.Just ("StackName" Lude..= stackName)
          ]
      )

instance Lude.ToPath AssociateFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateFleetResponse' smart constructor.
newtype AssociateFleetResponse = AssociateFleetResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateFleetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateFleetResponse
mkAssociateFleetResponse pResponseStatus_ =
  AssociateFleetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afrsResponseStatus :: Lens.Lens' AssociateFleetResponse Lude.Int
afrsResponseStatus = Lens.lens (responseStatus :: AssociateFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateFleetResponse)
{-# DEPRECATED afrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
