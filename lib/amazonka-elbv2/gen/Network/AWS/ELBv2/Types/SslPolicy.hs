{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SslPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.SslPolicy
  ( SslPolicy (..)
  -- * Smart constructor
  , mkSslPolicy
  -- * Lenses
  , spCiphers
  , spName
  , spSslProtocols
  ) where

import qualified Network.AWS.ELBv2.Types.Cipher as Types
import qualified Network.AWS.ELBv2.Types.Name as Types
import qualified Network.AWS.ELBv2.Types.SslProtocol as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a policy used for SSL negotiation.
--
-- /See:/ 'mkSslPolicy' smart constructor.
data SslPolicy = SslPolicy'
  { ciphers :: Core.Maybe [Types.Cipher]
    -- ^ The ciphers.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the policy.
  , sslProtocols :: Core.Maybe [Types.SslProtocol]
    -- ^ The protocols.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SslPolicy' value with any optional fields omitted.
mkSslPolicy
    :: SslPolicy
mkSslPolicy
  = SslPolicy'{ciphers = Core.Nothing, name = Core.Nothing,
               sslProtocols = Core.Nothing}

-- | The ciphers.
--
-- /Note:/ Consider using 'ciphers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spCiphers :: Lens.Lens' SslPolicy (Core.Maybe [Types.Cipher])
spCiphers = Lens.field @"ciphers"
{-# INLINEABLE spCiphers #-}
{-# DEPRECATED ciphers "Use generic-lens or generic-optics with 'ciphers' instead"  #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spName :: Lens.Lens' SslPolicy (Core.Maybe Types.Name)
spName = Lens.field @"name"
{-# INLINEABLE spName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The protocols.
--
-- /Note:/ Consider using 'sslProtocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSslProtocols :: Lens.Lens' SslPolicy (Core.Maybe [Types.SslProtocol])
spSslProtocols = Lens.field @"sslProtocols"
{-# INLINEABLE spSslProtocols #-}
{-# DEPRECATED sslProtocols "Use generic-lens or generic-optics with 'sslProtocols' instead"  #-}

instance Core.FromXML SslPolicy where
        parseXML x
          = SslPolicy' Core.<$>
              (x Core..@? "Ciphers" Core..<@> Core.parseXMLList "member")
                Core.<*> x Core..@? "Name"
                Core.<*>
                x Core..@? "SslProtocols" Core..<@> Core.parseXMLList "member"
