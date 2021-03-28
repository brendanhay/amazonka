{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.FaultRootCause
  ( FaultRootCause (..)
  -- * Smart constructor
  , mkFaultRootCause
  -- * Lenses
  , frcClientImpacting
  , frcServices
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.FaultRootCauseService as Types

-- | The root cause information for a trace summary fault.
--
-- /See:/ 'mkFaultRootCause' smart constructor.
data FaultRootCause = FaultRootCause'
  { clientImpacting :: Core.Maybe Core.Bool
    -- ^ A flag that denotes that the root cause impacts the trace client.
  , services :: Core.Maybe [Types.FaultRootCauseService]
    -- ^ A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaultRootCause' value with any optional fields omitted.
mkFaultRootCause
    :: FaultRootCause
mkFaultRootCause
  = FaultRootCause'{clientImpacting = Core.Nothing,
                    services = Core.Nothing}

-- | A flag that denotes that the root cause impacts the trace client.
--
-- /Note:/ Consider using 'clientImpacting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcClientImpacting :: Lens.Lens' FaultRootCause (Core.Maybe Core.Bool)
frcClientImpacting = Lens.field @"clientImpacting"
{-# INLINEABLE frcClientImpacting #-}
{-# DEPRECATED clientImpacting "Use generic-lens or generic-optics with 'clientImpacting' instead"  #-}

-- | A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcServices :: Lens.Lens' FaultRootCause (Core.Maybe [Types.FaultRootCauseService])
frcServices = Lens.field @"services"
{-# INLINEABLE frcServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

instance Core.FromJSON FaultRootCause where
        parseJSON
          = Core.withObject "FaultRootCause" Core.$
              \ x ->
                FaultRootCause' Core.<$>
                  (x Core..:? "ClientImpacting") Core.<*> x Core..:? "Services"
