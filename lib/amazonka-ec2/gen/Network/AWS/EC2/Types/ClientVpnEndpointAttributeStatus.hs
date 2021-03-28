{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus
  ( ClientVpnEndpointAttributeStatus (..)
  -- * Smart constructor
  , mkClientVpnEndpointAttributeStatus
  -- * Lenses
  , cveasCode
  , cveasMessage
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of the Client VPN endpoint attribute.
--
-- /See:/ 'mkClientVpnEndpointAttributeStatus' smart constructor.
data ClientVpnEndpointAttributeStatus = ClientVpnEndpointAttributeStatus'
  { code :: Core.Maybe Types.ClientVpnEndpointAttributeStatusCode
    -- ^ The status code.
  , message :: Core.Maybe Core.Text
    -- ^ The status message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnEndpointAttributeStatus' value with any optional fields omitted.
mkClientVpnEndpointAttributeStatus
    :: ClientVpnEndpointAttributeStatus
mkClientVpnEndpointAttributeStatus
  = ClientVpnEndpointAttributeStatus'{code = Core.Nothing,
                                      message = Core.Nothing}

-- | The status code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveasCode :: Lens.Lens' ClientVpnEndpointAttributeStatus (Core.Maybe Types.ClientVpnEndpointAttributeStatusCode)
cveasCode = Lens.field @"code"
{-# INLINEABLE cveasCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The status message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cveasMessage :: Lens.Lens' ClientVpnEndpointAttributeStatus (Core.Maybe Core.Text)
cveasMessage = Lens.field @"message"
{-# INLINEABLE cveasMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML ClientVpnEndpointAttributeStatus where
        parseXML x
          = ClientVpnEndpointAttributeStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
