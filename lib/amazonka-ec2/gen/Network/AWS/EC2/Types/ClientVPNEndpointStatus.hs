{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNEndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNEndpointStatus
  ( ClientVPNEndpointStatus (..),

    -- * Smart constructor
    mkClientVPNEndpointStatus,

    -- * Lenses
    cvesCode,
    cvesMessage,
  )
where

import Network.AWS.EC2.Types.ClientVPNEndpointStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a Client VPN endpoint.
--
-- /See:/ 'mkClientVPNEndpointStatus' smart constructor.
data ClientVPNEndpointStatus = ClientVPNEndpointStatus'
  { code ::
      Lude.Maybe ClientVPNEndpointStatusCode,
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

-- | Creates a value of 'ClientVPNEndpointStatus' with the minimum fields required to make a request.
--
-- * 'code' - The state of the Client VPN endpoint. Possible states include:
--
--
--     * @pending-associate@ - The Client VPN endpoint has been created but no target networks have been associated. The Client VPN endpoint cannot accept connections.
--
--
--     * @available@ - The Client VPN endpoint has been created and a target network has been associated. The Client VPN endpoint can accept connections.
--
--
--     * @deleting@ - The Client VPN endpoint is being deleted. The Client VPN endpoint cannot accept connections.
--
--
--     * @deleted@ - The Client VPN endpoint has been deleted. The Client VPN endpoint cannot accept connections.
--
--
-- * 'message' - A message about the status of the Client VPN endpoint.
mkClientVPNEndpointStatus ::
  ClientVPNEndpointStatus
mkClientVPNEndpointStatus =
  ClientVPNEndpointStatus'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The state of the Client VPN endpoint. Possible states include:
--
--
--     * @pending-associate@ - The Client VPN endpoint has been created but no target networks have been associated. The Client VPN endpoint cannot accept connections.
--
--
--     * @available@ - The Client VPN endpoint has been created and a target network has been associated. The Client VPN endpoint can accept connections.
--
--
--     * @deleting@ - The Client VPN endpoint is being deleted. The Client VPN endpoint cannot accept connections.
--
--
--     * @deleted@ - The Client VPN endpoint has been deleted. The Client VPN endpoint cannot accept connections.
--
--
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvesCode :: Lens.Lens' ClientVPNEndpointStatus (Lude.Maybe ClientVPNEndpointStatusCode)
cvesCode = Lens.lens (code :: ClientVPNEndpointStatus -> Lude.Maybe ClientVPNEndpointStatusCode) (\s a -> s {code = a} :: ClientVPNEndpointStatus)
{-# DEPRECATED cvesCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A message about the status of the Client VPN endpoint.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvesMessage :: Lens.Lens' ClientVPNEndpointStatus (Lude.Maybe Lude.Text)
cvesMessage = Lens.lens (message :: ClientVPNEndpointStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ClientVPNEndpointStatus)
{-# DEPRECATED cvesMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ClientVPNEndpointStatus where
  parseXML x =
    ClientVPNEndpointStatus'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
