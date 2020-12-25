{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCause
  ( ErrorRootCause (..),

    -- * Smart constructor
    mkErrorRootCause,

    -- * Lenses
    ercClientImpacting,
    ercServices,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ErrorRootCauseService as Types

-- | The root cause of a trace summary error.
--
-- /See:/ 'mkErrorRootCause' smart constructor.
data ErrorRootCause = ErrorRootCause'
  { -- | A flag that denotes that the root cause impacts the trace client.
    clientImpacting :: Core.Maybe Core.Bool,
    -- | A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
    services :: Core.Maybe [Types.ErrorRootCauseService]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorRootCause' value with any optional fields omitted.
mkErrorRootCause ::
  ErrorRootCause
mkErrorRootCause =
  ErrorRootCause'
    { clientImpacting = Core.Nothing,
      services = Core.Nothing
    }

-- | A flag that denotes that the root cause impacts the trace client.
--
-- /Note:/ Consider using 'clientImpacting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercClientImpacting :: Lens.Lens' ErrorRootCause (Core.Maybe Core.Bool)
ercClientImpacting = Lens.field @"clientImpacting"
{-# DEPRECATED ercClientImpacting "Use generic-lens or generic-optics with 'clientImpacting' instead." #-}

-- | A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercServices :: Lens.Lens' ErrorRootCause (Core.Maybe [Types.ErrorRootCauseService])
ercServices = Lens.field @"services"
{-# DEPRECATED ercServices "Use generic-lens or generic-optics with 'services' instead." #-}

instance Core.FromJSON ErrorRootCause where
  parseJSON =
    Core.withObject "ErrorRootCause" Core.$
      \x ->
        ErrorRootCause'
          Core.<$> (x Core..:? "ClientImpacting") Core.<*> (x Core..:? "Services")
