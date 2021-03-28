{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CancelSpotFleetRequestsError
  ( CancelSpotFleetRequestsError (..)
  -- * Smart constructor
  , mkCancelSpotFleetRequestsError
  -- * Lenses
  , csfreCode
  , csfreMessage
  ) where

import qualified Network.AWS.EC2.Types.CancelBatchErrorCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Spot Fleet error.
--
-- /See:/ 'mkCancelSpotFleetRequestsError' smart constructor.
data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError'
  { code :: Core.Maybe Types.CancelBatchErrorCode
    -- ^ The error code.
  , message :: Core.Maybe Core.Text
    -- ^ The description for the error code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSpotFleetRequestsError' value with any optional fields omitted.
mkCancelSpotFleetRequestsError
    :: CancelSpotFleetRequestsError
mkCancelSpotFleetRequestsError
  = CancelSpotFleetRequestsError'{code = Core.Nothing,
                                  message = Core.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreCode :: Lens.Lens' CancelSpotFleetRequestsError (Core.Maybe Types.CancelBatchErrorCode)
csfreCode = Lens.field @"code"
{-# INLINEABLE csfreCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The description for the error code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreMessage :: Lens.Lens' CancelSpotFleetRequestsError (Core.Maybe Core.Text)
csfreMessage = Lens.field @"message"
{-# INLINEABLE csfreMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML CancelSpotFleetRequestsError where
        parseXML x
          = CancelSpotFleetRequestsError' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
