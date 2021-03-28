{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetTagSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotFleetTagSpecification
  ( SpotFleetTagSpecification (..)
  -- * Smart constructor
  , mkSpotFleetTagSpecification
  -- * Lenses
  , sftsResourceType
  , sftsTags
  ) where

import qualified Network.AWS.EC2.Types.ResourceType as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The tags for a Spot Fleet resource.
--
-- /See:/ 'mkSpotFleetTagSpecification' smart constructor.
data SpotFleetTagSpecification = SpotFleetTagSpecification'
  { resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of resource. Currently, the only resource type that is supported is @instance@ . To tag the Spot Fleet request on creation, use the @TagSpecifications@ parameter in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html @SpotFleetRequestConfigData@ > .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpotFleetTagSpecification' value with any optional fields omitted.
mkSpotFleetTagSpecification
    :: SpotFleetTagSpecification
mkSpotFleetTagSpecification
  = SpotFleetTagSpecification'{resourceType = Core.Nothing,
                               tags = Core.Nothing}

-- | The type of resource. Currently, the only resource type that is supported is @instance@ . To tag the Spot Fleet request on creation, use the @TagSpecifications@ parameter in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetRequestConfigData.html @SpotFleetRequestConfigData@ > .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sftsResourceType :: Lens.Lens' SpotFleetTagSpecification (Core.Maybe Types.ResourceType)
sftsResourceType = Lens.field @"resourceType"
{-# INLINEABLE sftsResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sftsTags :: Lens.Lens' SpotFleetTagSpecification (Core.Maybe [Types.Tag])
sftsTags = Lens.field @"tags"
{-# INLINEABLE sftsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery SpotFleetTagSpecification where
        toQuery SpotFleetTagSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "ResourceType")
              resourceType
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Tag") tags

instance Core.FromXML SpotFleetTagSpecification where
        parseXML x
          = SpotFleetTagSpecification' Core.<$>
              (x Core..@? "resourceType") Core.<*>
                x Core..@? "tag" Core..<@> Core.parseXMLList "item"
