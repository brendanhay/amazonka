-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNRouteStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNRouteStatus
  ( ClientVPNRouteStatus (..),

    -- * Smart constructor
    mkClientVPNRouteStatus,

    -- * Lenses
    cvrsCode,
    cvrsMessage,
  )
where

import Network.AWS.EC2.Types.ClientVPNRouteStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a Client VPN endpoint route.
--
-- /See:/ 'mkClientVPNRouteStatus' smart constructor.
data ClientVPNRouteStatus = ClientVPNRouteStatus'
  { code ::
      Lude.Maybe ClientVPNRouteStatusCode,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientVPNRouteStatus' with the minimum fields required to make a request.
--
-- * 'code' - The state of the Client VPN endpoint route.
-- * 'message' - A message about the status of the Client VPN endpoint route, if applicable.
mkClientVPNRouteStatus ::
  ClientVPNRouteStatus
mkClientVPNRouteStatus =
  ClientVPNRouteStatus'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The state of the Client VPN endpoint route.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsCode :: Lens.Lens' ClientVPNRouteStatus (Lude.Maybe ClientVPNRouteStatusCode)
cvrsCode = Lens.lens (code :: ClientVPNRouteStatus -> Lude.Maybe ClientVPNRouteStatusCode) (\s a -> s {code = a} :: ClientVPNRouteStatus)
{-# DEPRECATED cvrsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A message about the status of the Client VPN endpoint route, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsMessage :: Lens.Lens' ClientVPNRouteStatus (Lude.Maybe Lude.Text)
cvrsMessage = Lens.lens (message :: ClientVPNRouteStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ClientVPNRouteStatus)
{-# DEPRECATED cvrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ClientVPNRouteStatus where
  parseXML x =
    ClientVPNRouteStatus'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
