{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceTypeOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceTypeOffering
  ( InstanceTypeOffering (..)
  -- * Smart constructor
  , mkInstanceTypeOffering
  -- * Lenses
  , itoInstanceType
  , itoLocation
  , itoLocationType
  ) where

import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.Location as Types
import qualified Network.AWS.EC2.Types.LocationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The instance types offered.
--
-- /See:/ 'mkInstanceTypeOffering' smart constructor.
data InstanceTypeOffering = InstanceTypeOffering'
  { instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
  , location :: Core.Maybe Types.Location
    -- ^ The identifier for the location. This depends on the location type. For example, if the location type is @region@ , the location is the Region code (for example, @us-east-2@ .)
  , locationType :: Core.Maybe Types.LocationType
    -- ^ The location type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceTypeOffering' value with any optional fields omitted.
mkInstanceTypeOffering
    :: InstanceTypeOffering
mkInstanceTypeOffering
  = InstanceTypeOffering'{instanceType = Core.Nothing,
                          location = Core.Nothing, locationType = Core.Nothing}

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itoInstanceType :: Lens.Lens' InstanceTypeOffering (Core.Maybe Types.InstanceType)
itoInstanceType = Lens.field @"instanceType"
{-# INLINEABLE itoInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The identifier for the location. This depends on the location type. For example, if the location type is @region@ , the location is the Region code (for example, @us-east-2@ .)
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itoLocation :: Lens.Lens' InstanceTypeOffering (Core.Maybe Types.Location)
itoLocation = Lens.field @"location"
{-# INLINEABLE itoLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The location type.
--
-- /Note:/ Consider using 'locationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itoLocationType :: Lens.Lens' InstanceTypeOffering (Core.Maybe Types.LocationType)
itoLocationType = Lens.field @"locationType"
{-# INLINEABLE itoLocationType #-}
{-# DEPRECATED locationType "Use generic-lens or generic-optics with 'locationType' instead"  #-}

instance Core.FromXML InstanceTypeOffering where
        parseXML x
          = InstanceTypeOffering' Core.<$>
              (x Core..@? "instanceType") Core.<*> x Core..@? "location" Core.<*>
                x Core..@? "locationType"
