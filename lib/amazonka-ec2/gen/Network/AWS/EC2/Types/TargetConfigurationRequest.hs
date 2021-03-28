{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TargetConfigurationRequest
  ( TargetConfigurationRequest (..)
  -- * Smart constructor
  , mkTargetConfigurationRequest
  -- * Lenses
  , tcrOfferingId
  , tcrInstanceCount
  ) where

import qualified Network.AWS.EC2.Types.OfferingId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the target configuration.
--
-- /See:/ 'mkTargetConfigurationRequest' smart constructor.
data TargetConfigurationRequest = TargetConfigurationRequest'
  { offeringId :: Types.OfferingId
    -- ^ The Convertible Reserved Instance offering ID.
  , instanceCount :: Core.Maybe Core.Int
    -- ^ The number of instances the Covertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetConfigurationRequest' value with any optional fields omitted.
mkTargetConfigurationRequest
    :: Types.OfferingId -- ^ 'offeringId'
    -> TargetConfigurationRequest
mkTargetConfigurationRequest offeringId
  = TargetConfigurationRequest'{offeringId,
                                instanceCount = Core.Nothing}

-- | The Convertible Reserved Instance offering ID.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrOfferingId :: Lens.Lens' TargetConfigurationRequest Types.OfferingId
tcrOfferingId = Lens.field @"offeringId"
{-# INLINEABLE tcrOfferingId #-}
{-# DEPRECATED offeringId "Use generic-lens or generic-optics with 'offeringId' instead"  #-}

-- | The number of instances the Covertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrInstanceCount :: Lens.Lens' TargetConfigurationRequest (Core.Maybe Core.Int)
tcrInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE tcrInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

instance Core.ToQuery TargetConfigurationRequest where
        toQuery TargetConfigurationRequest{..}
          = Core.toQueryPair "OfferingId" offeringId Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceCount")
                instanceCount
