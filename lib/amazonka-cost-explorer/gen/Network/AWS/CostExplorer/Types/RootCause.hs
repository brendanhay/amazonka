{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.RootCause
  ( RootCause (..)
  -- * Smart constructor
  , mkRootCause
  -- * Lenses
  , rcLinkedAccount
  , rcRegion
  , rcService
  , rcUsageType
  ) where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The combination of AWS service, linked account, Region, and usage type where a cost anomaly is observed. 
--
-- /See:/ 'mkRootCause' smart constructor.
data RootCause = RootCause'
  { linkedAccount :: Core.Maybe Types.GenericString
    -- ^ The linked account value associated with the cost anomaly. 
  , region :: Core.Maybe Types.GenericString
    -- ^ The AWS Region associated with the cost anomaly. 
  , service :: Core.Maybe Types.GenericString
    -- ^ The AWS service name associated with the cost anomaly. 
  , usageType :: Core.Maybe Types.GenericString
    -- ^ The @UsageType@ value associated with the cost anomaly. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RootCause' value with any optional fields omitted.
mkRootCause
    :: RootCause
mkRootCause
  = RootCause'{linkedAccount = Core.Nothing, region = Core.Nothing,
               service = Core.Nothing, usageType = Core.Nothing}

-- | The linked account value associated with the cost anomaly. 
--
-- /Note:/ Consider using 'linkedAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcLinkedAccount :: Lens.Lens' RootCause (Core.Maybe Types.GenericString)
rcLinkedAccount = Lens.field @"linkedAccount"
{-# INLINEABLE rcLinkedAccount #-}
{-# DEPRECATED linkedAccount "Use generic-lens or generic-optics with 'linkedAccount' instead"  #-}

-- | The AWS Region associated with the cost anomaly. 
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRegion :: Lens.Lens' RootCause (Core.Maybe Types.GenericString)
rcRegion = Lens.field @"region"
{-# INLINEABLE rcRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The AWS service name associated with the cost anomaly. 
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcService :: Lens.Lens' RootCause (Core.Maybe Types.GenericString)
rcService = Lens.field @"service"
{-# INLINEABLE rcService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The @UsageType@ value associated with the cost anomaly. 
--
-- /Note:/ Consider using 'usageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcUsageType :: Lens.Lens' RootCause (Core.Maybe Types.GenericString)
rcUsageType = Lens.field @"usageType"
{-# INLINEABLE rcUsageType #-}
{-# DEPRECATED usageType "Use generic-lens or generic-optics with 'usageType' instead"  #-}

instance Core.FromJSON RootCause where
        parseJSON
          = Core.withObject "RootCause" Core.$
              \ x ->
                RootCause' Core.<$>
                  (x Core..:? "LinkedAccount") Core.<*> x Core..:? "Region" Core.<*>
                    x Core..:? "Service"
                    Core.<*> x Core..:? "UsageType"
