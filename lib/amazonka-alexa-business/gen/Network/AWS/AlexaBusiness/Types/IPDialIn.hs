{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.IPDialIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.IPDialIn
  ( IPDialIn (..)
  -- * Smart constructor
  , mkIPDialIn
  -- * Lenses
  , ipdiEndpoint
  , ipdiCommsProtocol
  ) where

import qualified Network.AWS.AlexaBusiness.Types.CommsProtocol as Types
import qualified Network.AWS.AlexaBusiness.Types.Endpoint as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The IP endpoint and protocol for calling.
--
-- /See:/ 'mkIPDialIn' smart constructor.
data IPDialIn = IPDialIn'
  { endpoint :: Types.Endpoint
    -- ^ The IP address.
  , commsProtocol :: Types.CommsProtocol
    -- ^ The protocol, including SIP, SIPS, and H323.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPDialIn' value with any optional fields omitted.
mkIPDialIn
    :: Types.Endpoint -- ^ 'endpoint'
    -> Types.CommsProtocol -- ^ 'commsProtocol'
    -> IPDialIn
mkIPDialIn endpoint commsProtocol
  = IPDialIn'{endpoint, commsProtocol}

-- | The IP address.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipdiEndpoint :: Lens.Lens' IPDialIn Types.Endpoint
ipdiEndpoint = Lens.field @"endpoint"
{-# INLINEABLE ipdiEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | The protocol, including SIP, SIPS, and H323.
--
-- /Note:/ Consider using 'commsProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipdiCommsProtocol :: Lens.Lens' IPDialIn Types.CommsProtocol
ipdiCommsProtocol = Lens.field @"commsProtocol"
{-# INLINEABLE ipdiCommsProtocol #-}
{-# DEPRECATED commsProtocol "Use generic-lens or generic-optics with 'commsProtocol' instead"  #-}

instance Core.FromJSON IPDialIn where
        toJSON IPDialIn{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Endpoint" Core..= endpoint),
                  Core.Just ("CommsProtocol" Core..= commsProtocol)])

instance Core.FromJSON IPDialIn where
        parseJSON
          = Core.withObject "IPDialIn" Core.$
              \ x ->
                IPDialIn' Core.<$>
                  (x Core..: "Endpoint") Core.<*> x Core..: "CommsProtocol"
