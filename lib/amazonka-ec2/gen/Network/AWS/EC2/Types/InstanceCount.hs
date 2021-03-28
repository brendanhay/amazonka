{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceCount
  ( InstanceCount (..)
  -- * Smart constructor
  , mkInstanceCount
  -- * Lenses
  , icInstanceCount
  , icState
  ) where

import qualified Network.AWS.EC2.Types.ListingState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance listing state.
--
-- /See:/ 'mkInstanceCount' smart constructor.
data InstanceCount = InstanceCount'
  { instanceCount :: Core.Maybe Core.Int
    -- ^ The number of listed Reserved Instances in the state specified by the @state@ .
  , state :: Core.Maybe Types.ListingState
    -- ^ The states of the listed Reserved Instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceCount' value with any optional fields omitted.
mkInstanceCount
    :: InstanceCount
mkInstanceCount
  = InstanceCount'{instanceCount = Core.Nothing,
                   state = Core.Nothing}

-- | The number of listed Reserved Instances in the state specified by the @state@ .
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icInstanceCount :: Lens.Lens' InstanceCount (Core.Maybe Core.Int)
icInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE icInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The states of the listed Reserved Instances.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icState :: Lens.Lens' InstanceCount (Core.Maybe Types.ListingState)
icState = Lens.field @"state"
{-# INLINEABLE icState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromXML InstanceCount where
        parseXML x
          = InstanceCount' Core.<$>
              (x Core..@? "instanceCount") Core.<*> x Core..@? "state"
