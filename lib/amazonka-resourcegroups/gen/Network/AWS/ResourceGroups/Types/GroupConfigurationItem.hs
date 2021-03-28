{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfigurationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.GroupConfigurationItem
  ( GroupConfigurationItem (..)
  -- * Smart constructor
  , mkGroupConfigurationItem
  -- * Lenses
  , gciType
  , gciParameters
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.GroupConfigurationParameter as Types
import qualified Network.AWS.ResourceGroups.Types.GroupConfigurationType as Types

-- | An item in a group configuration. A group configuration can have one or more items.
--
-- /See:/ 'mkGroupConfigurationItem' smart constructor.
data GroupConfigurationItem = GroupConfigurationItem'
  { type' :: Types.GroupConfigurationType
    -- ^ Specifies the type of group configuration item. Each item must have a unique value for @type@ .
--
-- You can specify the following string values:
--
--     * @AWS::EC2::CapacityReservationPool@ 
-- For more information about EC2 capacity reservation groups, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
--
--
--     * @AWS::ResourceGroups::Generic@ - Supports parameters that configure the behavior of resource groups of any type.
--
--
  , parameters :: Core.Maybe [Types.GroupConfigurationParameter]
    -- ^ A collection of parameters for this group configuration item.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupConfigurationItem' value with any optional fields omitted.
mkGroupConfigurationItem
    :: Types.GroupConfigurationType -- ^ 'type\''
    -> GroupConfigurationItem
mkGroupConfigurationItem type'
  = GroupConfigurationItem'{type', parameters = Core.Nothing}

-- | Specifies the type of group configuration item. Each item must have a unique value for @type@ .
--
-- You can specify the following string values:
--
--     * @AWS::EC2::CapacityReservationPool@ 
-- For more information about EC2 capacity reservation groups, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
--
--
--     * @AWS::ResourceGroups::Generic@ - Supports parameters that configure the behavior of resource groups of any type.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciType :: Lens.Lens' GroupConfigurationItem Types.GroupConfigurationType
gciType = Lens.field @"type'"
{-# INLINEABLE gciType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A collection of parameters for this group configuration item.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciParameters :: Lens.Lens' GroupConfigurationItem (Core.Maybe [Types.GroupConfigurationParameter])
gciParameters = Lens.field @"parameters"
{-# INLINEABLE gciParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.FromJSON GroupConfigurationItem where
        toJSON GroupConfigurationItem{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'),
                  ("Parameters" Core..=) Core.<$> parameters])

instance Core.FromJSON GroupConfigurationItem where
        parseJSON
          = Core.withObject "GroupConfigurationItem" Core.$
              \ x ->
                GroupConfigurationItem' Core.<$>
                  (x Core..: "Type") Core.<*> x Core..:? "Parameters"
