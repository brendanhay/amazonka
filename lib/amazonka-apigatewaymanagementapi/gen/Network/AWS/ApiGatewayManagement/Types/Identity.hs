{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGatewayManagement.Types.Identity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGatewayManagement.Types.Identity
  ( Identity (..)
  -- * Smart constructor
  , mkIdentity
  -- * Lenses
  , iSourceIp
  , iUserAgent
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkIdentity' smart constructor.
data Identity = Identity'
  { sourceIp :: Core.Text
    -- ^ The source IP address of the TCP connection making the request to API Gateway.
  , userAgent :: Core.Text
    -- ^ The User Agent of the API caller.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Identity' value with any optional fields omitted.
mkIdentity
    :: Core.Text -- ^ 'sourceIp'
    -> Core.Text -- ^ 'userAgent'
    -> Identity
mkIdentity sourceIp userAgent = Identity'{sourceIp, userAgent}

-- | The source IP address of the TCP connection making the request to API Gateway.
--
-- /Note:/ Consider using 'sourceIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSourceIp :: Lens.Lens' Identity Core.Text
iSourceIp = Lens.field @"sourceIp"
{-# INLINEABLE iSourceIp #-}
{-# DEPRECATED sourceIp "Use generic-lens or generic-optics with 'sourceIp' instead"  #-}

-- | The User Agent of the API caller.
--
-- /Note:/ Consider using 'userAgent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iUserAgent :: Lens.Lens' Identity Core.Text
iUserAgent = Lens.field @"userAgent"
{-# INLINEABLE iUserAgent #-}
{-# DEPRECATED userAgent "Use generic-lens or generic-optics with 'userAgent' instead"  #-}

instance Core.FromJSON Identity where
        parseJSON
          = Core.withObject "Identity" Core.$
              \ x ->
                Identity' Core.<$>
                  (x Core..: "sourceIp") Core.<*> x Core..: "userAgent"
