{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfigurationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupConfigurationItem
  ( GroupConfigurationItem (..),

    -- * Smart constructor
    mkGroupConfigurationItem,

    -- * Lenses
    gciParameters,
    gciType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroups.Types.GroupConfigurationParameter

-- | An item in a group configuration. A group configuration can have one or more items.
--
-- /See:/ 'mkGroupConfigurationItem' smart constructor.
data GroupConfigurationItem = GroupConfigurationItem'
  { -- | A collection of parameters for this group configuration item.
    parameters :: Lude.Maybe [GroupConfigurationParameter],
    -- | Specifies the type of group configuration item. Each item must have a unique value for @type@ .
    --
    -- You can specify the following string values:
    --
    --     * @AWS::EC2::CapacityReservationPool@
    -- For more information about EC2 capacity reservation groups, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
    --
    --
    --     * @AWS::ResourceGroups::Generic@ - Supports parameters that configure the behavior of resource groups of any type.
    type' :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupConfigurationItem' with the minimum fields required to make a request.
--
-- * 'parameters' - A collection of parameters for this group configuration item.
-- * 'type'' - Specifies the type of group configuration item. Each item must have a unique value for @type@ .
--
-- You can specify the following string values:
--
--     * @AWS::EC2::CapacityReservationPool@
-- For more information about EC2 capacity reservation groups, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
--
--
--     * @AWS::ResourceGroups::Generic@ - Supports parameters that configure the behavior of resource groups of any type.
mkGroupConfigurationItem ::
  -- | 'type''
  Lude.Text ->
  GroupConfigurationItem
mkGroupConfigurationItem pType_ =
  GroupConfigurationItem'
    { parameters = Lude.Nothing,
      type' = pType_
    }

-- | A collection of parameters for this group configuration item.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciParameters :: Lens.Lens' GroupConfigurationItem (Lude.Maybe [GroupConfigurationParameter])
gciParameters = Lens.lens (parameters :: GroupConfigurationItem -> Lude.Maybe [GroupConfigurationParameter]) (\s a -> s {parameters = a} :: GroupConfigurationItem)
{-# DEPRECATED gciParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

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
gciType :: Lens.Lens' GroupConfigurationItem Lude.Text
gciType = Lens.lens (type' :: GroupConfigurationItem -> Lude.Text) (\s a -> s {type' = a} :: GroupConfigurationItem)
{-# DEPRECATED gciType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON GroupConfigurationItem where
  parseJSON =
    Lude.withObject
      "GroupConfigurationItem"
      ( \x ->
          GroupConfigurationItem'
            Lude.<$> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON GroupConfigurationItem where
  toJSON GroupConfigurationItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("Type" Lude..= type')
          ]
      )
