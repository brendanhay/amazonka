{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfigurationParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupConfigurationParameter
  ( GroupConfigurationParameter (..),

    -- * Smart constructor
    mkGroupConfigurationParameter,

    -- * Lenses
    gcpName,
    gcpValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.GroupConfigurationParameterValue as Types
import qualified Network.AWS.ResourceGroups.Types.Name as Types

-- | A parameter for a group configuration item.
--
-- /See:/ 'mkGroupConfigurationParameter' smart constructor.
data GroupConfigurationParameter = GroupConfigurationParameter'
  { -- | The name of the group configuration parameter.
    --
    -- You can specify the following string values:
    --
    --     * For configuration item type @AWS::ResourceGroups::Generic@ :
    --
    --     * @allowed-resource-types@
    -- Specifies the types of resources that you can add to this group by using the 'GroupResources' operation.
    --
    --
    --
    --
    --     * For configuration item type @AWS::EC2::CapacityReservationPool@ :
    --
    --     * None - This configuration item type doesn't support any parameters.
    --
    --
    -- For more information about EC2 capacity reservation groups, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
    name :: Types.Name,
    -- | The values of for this parameter.
    --
    -- You can specify the following string value:
    --
    --     * For item type @allowed-resource-types@ : the only supported parameter value is @AWS::EC2::CapacityReservation@ .
    values :: Core.Maybe [Types.GroupConfigurationParameterValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupConfigurationParameter' value with any optional fields omitted.
mkGroupConfigurationParameter ::
  -- | 'name'
  Types.Name ->
  GroupConfigurationParameter
mkGroupConfigurationParameter name =
  GroupConfigurationParameter' {name, values = Core.Nothing}

-- | The name of the group configuration parameter.
--
-- You can specify the following string values:
--
--     * For configuration item type @AWS::ResourceGroups::Generic@ :
--
--     * @allowed-resource-types@
-- Specifies the types of resources that you can add to this group by using the 'GroupResources' operation.
--
--
--
--
--     * For configuration item type @AWS::EC2::CapacityReservationPool@ :
--
--     * None - This configuration item type doesn't support any parameters.
--
--
-- For more information about EC2 capacity reservation groups, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpName :: Lens.Lens' GroupConfigurationParameter Types.Name
gcpName = Lens.field @"name"
{-# DEPRECATED gcpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The values of for this parameter.
--
-- You can specify the following string value:
--
--     * For item type @allowed-resource-types@ : the only supported parameter value is @AWS::EC2::CapacityReservation@ .
--
--
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpValues :: Lens.Lens' GroupConfigurationParameter (Core.Maybe [Types.GroupConfigurationParameterValue])
gcpValues = Lens.field @"values"
{-# DEPRECATED gcpValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON GroupConfigurationParameter where
  toJSON GroupConfigurationParameter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("Values" Core..=) Core.<$> values
          ]
      )

instance Core.FromJSON GroupConfigurationParameter where
  parseJSON =
    Core.withObject "GroupConfigurationParameter" Core.$
      \x ->
        GroupConfigurationParameter'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..:? "Values")
