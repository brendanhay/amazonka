-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatus
  ( ClientVPNEndpointAttributeStatus (..),

    -- * Smart constructor
    mkClientVPNEndpointAttributeStatus,

    -- * Lenses
    cveasCode,
    cveasMessage,
  )
where

import Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of the Client VPN endpoint attribute.
--
-- /See:/ 'mkClientVPNEndpointAttributeStatus' smart constructor.
data ClientVPNEndpointAttributeStatus = ClientVPNEndpointAttributeStatus'
  { code ::
      Lude.Maybe
        ClientVPNEndpointAttributeStatusCode,
    message ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientVPNEndpointAttributeStatus' with the minimum fields required to make a request.
--
-- * 'code' - The status code.
-- * 'message' - The status message.
mkClientVPNEndpointAttributeStatus ::
  ClientVPNEndpointAttributeStatus
mkClientVPNEndpointAttributeStatus =
  ClientVPNEndpointAttributeStatus'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The status code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveasCode :: Lens.Lens' ClientVPNEndpointAttributeStatus (Lude.Maybe ClientVPNEndpointAttributeStatusCode)
cveasCode = Lens.lens (code :: ClientVPNEndpointAttributeStatus -> Lude.Maybe ClientVPNEndpointAttributeStatusCode) (\s a -> s {code = a} :: ClientVPNEndpointAttributeStatus)
{-# DEPRECATED cveasCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveasMessage :: Lens.Lens' ClientVPNEndpointAttributeStatus (Lude.Maybe Lude.Text)
cveasMessage = Lens.lens (message :: ClientVPNEndpointAttributeStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ClientVPNEndpointAttributeStatus)
{-# DEPRECATED cveasMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ClientVPNEndpointAttributeStatus where
  parseXML x =
    ClientVPNEndpointAttributeStatus'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
