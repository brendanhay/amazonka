{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.IPDialIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.IPDialIn
  ( IPDialIn (..),

    -- * Smart constructor
    mkIPDialIn,

    -- * Lenses
    idiCommsProtocol,
    idiEndpoint,
  )
where

import Network.AWS.AlexaBusiness.Types.CommsProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The IP endpoint and protocol for calling.
--
-- /See:/ 'mkIPDialIn' smart constructor.
data IPDialIn = IPDialIn'
  { -- | The protocol, including SIP, SIPS, and H323.
    commsProtocol :: CommsProtocol,
    -- | The IP address.
    endpoint :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPDialIn' with the minimum fields required to make a request.
--
-- * 'commsProtocol' - The protocol, including SIP, SIPS, and H323.
-- * 'endpoint' - The IP address.
mkIPDialIn ::
  -- | 'commsProtocol'
  CommsProtocol ->
  -- | 'endpoint'
  Lude.Text ->
  IPDialIn
mkIPDialIn pCommsProtocol_ pEndpoint_ =
  IPDialIn' {commsProtocol = pCommsProtocol_, endpoint = pEndpoint_}

-- | The protocol, including SIP, SIPS, and H323.
--
-- /Note:/ Consider using 'commsProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idiCommsProtocol :: Lens.Lens' IPDialIn CommsProtocol
idiCommsProtocol = Lens.lens (commsProtocol :: IPDialIn -> CommsProtocol) (\s a -> s {commsProtocol = a} :: IPDialIn)
{-# DEPRECATED idiCommsProtocol "Use generic-lens or generic-optics with 'commsProtocol' instead." #-}

-- | The IP address.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idiEndpoint :: Lens.Lens' IPDialIn Lude.Text
idiEndpoint = Lens.lens (endpoint :: IPDialIn -> Lude.Text) (\s a -> s {endpoint = a} :: IPDialIn)
{-# DEPRECATED idiEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Lude.FromJSON IPDialIn where
  parseJSON =
    Lude.withObject
      "IPDialIn"
      ( \x ->
          IPDialIn'
            Lude.<$> (x Lude..: "CommsProtocol") Lude.<*> (x Lude..: "Endpoint")
      )

instance Lude.ToJSON IPDialIn where
  toJSON IPDialIn' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CommsProtocol" Lude..= commsProtocol),
            Lude.Just ("Endpoint" Lude..= endpoint)
          ]
      )
