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
    ipdiEndpoint,
    ipdiCommsProtocol,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.CommsProtocol as Types
import qualified Network.AWS.AlexaBusiness.Types.Endpoint as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The IP endpoint and protocol for calling.
--
-- /See:/ 'mkIPDialIn' smart constructor.
data IPDialIn = IPDialIn'
  { -- | The IP address.
    endpoint :: Types.Endpoint,
    -- | The protocol, including SIP, SIPS, and H323.
    commsProtocol :: Types.CommsProtocol
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPDialIn' value with any optional fields omitted.
mkIPDialIn ::
  -- | 'endpoint'
  Types.Endpoint ->
  -- | 'commsProtocol'
  Types.CommsProtocol ->
  IPDialIn
mkIPDialIn endpoint commsProtocol =
  IPDialIn' {endpoint, commsProtocol}

-- | The IP address.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipdiEndpoint :: Lens.Lens' IPDialIn Types.Endpoint
ipdiEndpoint = Lens.field @"endpoint"
{-# DEPRECATED ipdiEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The protocol, including SIP, SIPS, and H323.
--
-- /Note:/ Consider using 'commsProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipdiCommsProtocol :: Lens.Lens' IPDialIn Types.CommsProtocol
ipdiCommsProtocol = Lens.field @"commsProtocol"
{-# DEPRECATED ipdiCommsProtocol "Use generic-lens or generic-optics with 'commsProtocol' instead." #-}

instance Core.FromJSON IPDialIn where
  toJSON IPDialIn {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Endpoint" Core..= endpoint),
            Core.Just ("CommsProtocol" Core..= commsProtocol)
          ]
      )

instance Core.FromJSON IPDialIn where
  parseJSON =
    Core.withObject "IPDialIn" Core.$
      \x ->
        IPDialIn'
          Core.<$> (x Core..: "Endpoint") Core.<*> (x Core..: "CommsProtocol")
