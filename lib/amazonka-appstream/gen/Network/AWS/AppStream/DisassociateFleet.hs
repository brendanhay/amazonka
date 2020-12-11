{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DisassociateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified fleet from the specified stack.
module Network.AWS.AppStream.DisassociateFleet
  ( -- * Creating a request
    DisassociateFleet (..),
    mkDisassociateFleet,

    -- ** Request lenses
    dfFleetName,
    dfStackName,

    -- * Destructuring the response
    DisassociateFleetResponse (..),
    mkDisassociateFleetResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateFleet' smart constructor.
data DisassociateFleet = DisassociateFleet'
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

-- | Creates a value of 'DisassociateFleet' with the minimum fields required to make a request.
--
-- * 'fleetName' - The name of the fleet.
-- * 'stackName' - The name of the stack.
mkDisassociateFleet ::
  -- | 'fleetName'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  DisassociateFleet
mkDisassociateFleet pFleetName_ pStackName_ =
  DisassociateFleet'
    { fleetName = pFleetName_,
      stackName = pStackName_
    }

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFleetName :: Lens.Lens' DisassociateFleet Lude.Text
dfFleetName = Lens.lens (fleetName :: DisassociateFleet -> Lude.Text) (\s a -> s {fleetName = a} :: DisassociateFleet)
{-# DEPRECATED dfFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfStackName :: Lens.Lens' DisassociateFleet Lude.Text
dfStackName = Lens.lens (stackName :: DisassociateFleet -> Lude.Text) (\s a -> s {stackName = a} :: DisassociateFleet)
{-# DEPRECATED dfStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest DisassociateFleet where
  type Rs DisassociateFleet = DisassociateFleetResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateFleetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DisassociateFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateFleet where
  toJSON DisassociateFleet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FleetName" Lude..= fleetName),
            Lude.Just ("StackName" Lude..= stackName)
          ]
      )

instance Lude.ToPath DisassociateFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateFleetResponse' smart constructor.
newtype DisassociateFleetResponse = DisassociateFleetResponse'
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

-- | Creates a value of 'DisassociateFleetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateFleetResponse
mkDisassociateFleetResponse pResponseStatus_ =
  DisassociateFleetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DisassociateFleetResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DisassociateFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateFleetResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
