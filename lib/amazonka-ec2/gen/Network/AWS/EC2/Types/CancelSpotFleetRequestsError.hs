{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsError
  ( CancelSpotFleetRequestsError (..),

    -- * Smart constructor
    mkCancelSpotFleetRequestsError,

    -- * Lenses
    csfreCode,
    csfreMessage,
  )
where

import qualified Network.AWS.EC2.Types.CancelBatchErrorCode as Types
import qualified Network.AWS.EC2.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Spot Fleet error.
--
-- /See:/ 'mkCancelSpotFleetRequestsError' smart constructor.
data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError'
  { -- | The error code.
    code :: Core.Maybe Types.CancelBatchErrorCode,
    -- | The description for the error code.
    message :: Core.Maybe Types.Message
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSpotFleetRequestsError' value with any optional fields omitted.
mkCancelSpotFleetRequestsError ::
  CancelSpotFleetRequestsError
mkCancelSpotFleetRequestsError =
  CancelSpotFleetRequestsError'
    { code = Core.Nothing,
      message = Core.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreCode :: Lens.Lens' CancelSpotFleetRequestsError (Core.Maybe Types.CancelBatchErrorCode)
csfreCode = Lens.field @"code"
{-# DEPRECATED csfreCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The description for the error code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreMessage :: Lens.Lens' CancelSpotFleetRequestsError (Core.Maybe Types.Message)
csfreMessage = Lens.field @"message"
{-# DEPRECATED csfreMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromXML CancelSpotFleetRequestsError where
  parseXML x =
    CancelSpotFleetRequestsError'
      Core.<$> (x Core..@? "code") Core.<*> (x Core..@? "message")
