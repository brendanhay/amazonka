-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.Group
  ( Group (..),

    -- * Smart constructor
    mkGroup,

    -- * Lenses
    gDescription,
    gGroupARN,
    gName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A resource group that contains AWS resources. You can assign resources to the group by associating either of the following elements with the group:
--
--
--     * 'ResourceQuery' - Use a resource query to specify a set of tag keys and values. All resources in the same AWS Region and AWS account that have those keys with the same values are included in the group. You can add a resource query when you create the group.
--
--
--     * 'GroupConfiguration' - Use a service configuration to associate the group with an AWS service. The configuration specifies which resource types can be included in the group.
--
--
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { description :: Lude.Maybe Lude.Text,
    groupARN :: Lude.Text,
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

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- * 'description' - The description of the resource group.
-- * 'groupARN' - The ARN of the resource group.
-- * 'name' - The name of the resource group.
mkGroup ::
  -- | 'groupARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  Group
mkGroup pGroupARN_ pName_ =
  Group'
    { description = Lude.Nothing,
      groupARN = pGroupARN_,
      name = pName_
    }

-- | The description of the resource group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDescription :: Lens.Lens' Group (Lude.Maybe Lude.Text)
gDescription = Lens.lens (description :: Group -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Group)
{-# DEPRECATED gDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGroupARN :: Lens.Lens' Group Lude.Text
gGroupARN = Lens.lens (groupARN :: Group -> Lude.Text) (\s a -> s {groupARN = a} :: Group)
{-# DEPRECATED gGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The name of the resource group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' Group Lude.Text
gName = Lens.lens (name :: Group -> Lude.Text) (\s a -> s {name = a} :: Group)
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Group where
  parseJSON =
    Lude.withObject
      "Group"
      ( \x ->
          Group'
            Lude.<$> (x Lude..:? "Description")
            Lude.<*> (x Lude..: "GroupArn")
            Lude.<*> (x Lude..: "Name")
      )
