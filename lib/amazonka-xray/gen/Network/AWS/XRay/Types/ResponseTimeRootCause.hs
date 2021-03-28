{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.ResponseTimeRootCause
  ( ResponseTimeRootCause (..)
  -- * Smart constructor
  , mkResponseTimeRootCause
  -- * Lenses
  , rtrcClientImpacting
  , rtrcServices
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ResponseTimeRootCauseService as Types

-- | The root cause information for a response time warning.
--
-- /See:/ 'mkResponseTimeRootCause' smart constructor.
data ResponseTimeRootCause = ResponseTimeRootCause'
  { clientImpacting :: Core.Maybe Core.Bool
    -- ^ A flag that denotes that the root cause impacts the trace client.
  , services :: Core.Maybe [Types.ResponseTimeRootCauseService]
    -- ^ A list of corresponding services. A service identifies a segment and contains a name, account ID, type, and inferred flag.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResponseTimeRootCause' value with any optional fields omitted.
mkResponseTimeRootCause
    :: ResponseTimeRootCause
mkResponseTimeRootCause
  = ResponseTimeRootCause'{clientImpacting = Core.Nothing,
                           services = Core.Nothing}

-- | A flag that denotes that the root cause impacts the trace client.
--
-- /Note:/ Consider using 'clientImpacting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcClientImpacting :: Lens.Lens' ResponseTimeRootCause (Core.Maybe Core.Bool)
rtrcClientImpacting = Lens.field @"clientImpacting"
{-# INLINEABLE rtrcClientImpacting #-}
{-# DEPRECATED clientImpacting "Use generic-lens or generic-optics with 'clientImpacting' instead"  #-}

-- | A list of corresponding services. A service identifies a segment and contains a name, account ID, type, and inferred flag.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcServices :: Lens.Lens' ResponseTimeRootCause (Core.Maybe [Types.ResponseTimeRootCauseService])
rtrcServices = Lens.field @"services"
{-# INLINEABLE rtrcServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

instance Core.FromJSON ResponseTimeRootCause where
        parseJSON
          = Core.withObject "ResponseTimeRootCause" Core.$
              \ x ->
                ResponseTimeRootCause' Core.<$>
                  (x Core..:? "ClientImpacting") Core.<*> x Core..:? "Services"
