{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ClusterStateChangeReason
  ( ClusterStateChangeReason (..)
  -- * Smart constructor
  , mkClusterStateChangeReason
  -- * Lenses
  , cscrCode
  , cscrMessage
  ) where

import qualified Network.AWS.EMR.Types.ClusterStateChangeReasonCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The reason that the cluster changed to its current state.
--
-- /See:/ 'mkClusterStateChangeReason' smart constructor.
data ClusterStateChangeReason = ClusterStateChangeReason'
  { code :: Core.Maybe Types.ClusterStateChangeReasonCode
    -- ^ The programmatic code for the state change reason.
  , message :: Core.Maybe Core.Text
    -- ^ The descriptive message for the state change reason.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterStateChangeReason' value with any optional fields omitted.
mkClusterStateChangeReason
    :: ClusterStateChangeReason
mkClusterStateChangeReason
  = ClusterStateChangeReason'{code = Core.Nothing,
                              message = Core.Nothing}

-- | The programmatic code for the state change reason.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrCode :: Lens.Lens' ClusterStateChangeReason (Core.Maybe Types.ClusterStateChangeReasonCode)
cscrCode = Lens.field @"code"
{-# INLINEABLE cscrCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The descriptive message for the state change reason.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrMessage :: Lens.Lens' ClusterStateChangeReason (Core.Maybe Core.Text)
cscrMessage = Lens.field @"message"
{-# INLINEABLE cscrMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON ClusterStateChangeReason where
        parseJSON
          = Core.withObject "ClusterStateChangeReason" Core.$
              \ x ->
                ClusterStateChangeReason' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Message"
