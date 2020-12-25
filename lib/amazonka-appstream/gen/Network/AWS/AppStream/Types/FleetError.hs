{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetError
  ( FleetError (..),

    -- * Smart constructor
    mkFleetError,

    -- * Lenses
    feErrorCode,
    feErrorMessage,
  )
where

import qualified Network.AWS.AppStream.Types.FleetErrorCode as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a fleet error.
--
-- /See:/ 'mkFleetError' smart constructor.
data FleetError = FleetError'
  { -- | The error code.
    errorCode :: Core.Maybe Types.FleetErrorCode,
    -- | The error message.
    errorMessage :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetError' value with any optional fields omitted.
mkFleetError ::
  FleetError
mkFleetError =
  FleetError'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feErrorCode :: Lens.Lens' FleetError (Core.Maybe Types.FleetErrorCode)
feErrorCode = Lens.field @"errorCode"
{-# DEPRECATED feErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feErrorMessage :: Lens.Lens' FleetError (Core.Maybe Types.String)
feErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED feErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Core.FromJSON FleetError where
  parseJSON =
    Core.withObject "FleetError" Core.$
      \x ->
        FleetError'
          Core.<$> (x Core..:? "ErrorCode") Core.<*> (x Core..:? "ErrorMessage")
