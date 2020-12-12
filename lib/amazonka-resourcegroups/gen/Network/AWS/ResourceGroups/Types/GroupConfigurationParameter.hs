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
    gcpValues,
    gcpName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A parameter for a group configuration item.
--
-- /See:/ 'mkGroupConfigurationParameter' smart constructor.
data GroupConfigurationParameter = GroupConfigurationParameter'
  { values ::
      Lude.Maybe [Lude.Text],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupConfigurationParameter' with the minimum fields required to make a request.
--
-- * 'name' - The name of the group configuration parameter.
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
-- * 'values' - The values of for this parameter.
--
-- You can specify the following string value:
--
--     * For item type @allowed-resource-types@ : the only supported parameter value is @AWS::EC2::CapacityReservation@ .
mkGroupConfigurationParameter ::
  -- | 'name'
  Lude.Text ->
  GroupConfigurationParameter
mkGroupConfigurationParameter pName_ =
  GroupConfigurationParameter'
    { values = Lude.Nothing,
      name = pName_
    }

-- | The values of for this parameter.
--
-- You can specify the following string value:
--
--     * For item type @allowed-resource-types@ : the only supported parameter value is @AWS::EC2::CapacityReservation@ .
--
--
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpValues :: Lens.Lens' GroupConfigurationParameter (Lude.Maybe [Lude.Text])
gcpValues = Lens.lens (values :: GroupConfigurationParameter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: GroupConfigurationParameter)
{-# DEPRECATED gcpValues "Use generic-lens or generic-optics with 'values' instead." #-}

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
gcpName :: Lens.Lens' GroupConfigurationParameter Lude.Text
gcpName = Lens.lens (name :: GroupConfigurationParameter -> Lude.Text) (\s a -> s {name = a} :: GroupConfigurationParameter)
{-# DEPRECATED gcpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON GroupConfigurationParameter where
  parseJSON =
    Lude.withObject
      "GroupConfigurationParameter"
      ( \x ->
          GroupConfigurationParameter'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON GroupConfigurationParameter where
  toJSON GroupConfigurationParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            Lude.Just ("Name" Lude..= name)
          ]
      )
