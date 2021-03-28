{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TargetConfiguration
  ( TargetConfiguration (..)
  -- * Smart constructor
  , mkTargetConfiguration
  -- * Lenses
  , tcInstanceCount
  , tcOfferingId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the Convertible Reserved Instance offering.
--
-- /See:/ 'mkTargetConfiguration' smart constructor.
data TargetConfiguration = TargetConfiguration'
  { instanceCount :: Core.Maybe Core.Int
    -- ^ The number of instances the Convertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
  , offeringId :: Core.Maybe Core.Text
    -- ^ The ID of the Convertible Reserved Instance offering.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetConfiguration' value with any optional fields omitted.
mkTargetConfiguration
    :: TargetConfiguration
mkTargetConfiguration
  = TargetConfiguration'{instanceCount = Core.Nothing,
                         offeringId = Core.Nothing}

-- | The number of instances the Convertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInstanceCount :: Lens.Lens' TargetConfiguration (Core.Maybe Core.Int)
tcInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE tcInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The ID of the Convertible Reserved Instance offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcOfferingId :: Lens.Lens' TargetConfiguration (Core.Maybe Core.Text)
tcOfferingId = Lens.field @"offeringId"
{-# INLINEABLE tcOfferingId #-}
{-# DEPRECATED offeringId "Use generic-lens or generic-optics with 'offeringId' instead"  #-}

instance Core.FromXML TargetConfiguration where
        parseXML x
          = TargetConfiguration' Core.<$>
              (x Core..@? "instanceCount") Core.<*> x Core..@? "offeringId"
