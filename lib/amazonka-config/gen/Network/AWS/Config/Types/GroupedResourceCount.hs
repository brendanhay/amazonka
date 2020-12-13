{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.GroupedResourceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.GroupedResourceCount
  ( GroupedResourceCount (..),

    -- * Smart constructor
    mkGroupedResourceCount,

    -- * Lenses
    grcGroupName,
    grcResourceCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The count of resources that are grouped by the group name.
--
-- /See:/ 'mkGroupedResourceCount' smart constructor.
data GroupedResourceCount = GroupedResourceCount'
  { -- | The name of the group that can be region, account ID, or resource type. For example, region1, region2 if the region was chosen as @GroupByKey@ .
    groupName :: Lude.Text,
    -- | The number of resources in the group.
    resourceCount :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupedResourceCount' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the group that can be region, account ID, or resource type. For example, region1, region2 if the region was chosen as @GroupByKey@ .
-- * 'resourceCount' - The number of resources in the group.
mkGroupedResourceCount ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'resourceCount'
  Lude.Integer ->
  GroupedResourceCount
mkGroupedResourceCount pGroupName_ pResourceCount_ =
  GroupedResourceCount'
    { groupName = pGroupName_,
      resourceCount = pResourceCount_
    }

-- | The name of the group that can be region, account ID, or resource type. For example, region1, region2 if the region was chosen as @GroupByKey@ .
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcGroupName :: Lens.Lens' GroupedResourceCount Lude.Text
grcGroupName = Lens.lens (groupName :: GroupedResourceCount -> Lude.Text) (\s a -> s {groupName = a} :: GroupedResourceCount)
{-# DEPRECATED grcGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The number of resources in the group.
--
-- /Note:/ Consider using 'resourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcResourceCount :: Lens.Lens' GroupedResourceCount Lude.Integer
grcResourceCount = Lens.lens (resourceCount :: GroupedResourceCount -> Lude.Integer) (\s a -> s {resourceCount = a} :: GroupedResourceCount)
{-# DEPRECATED grcResourceCount "Use generic-lens or generic-optics with 'resourceCount' instead." #-}

instance Lude.FromJSON GroupedResourceCount where
  parseJSON =
    Lude.withObject
      "GroupedResourceCount"
      ( \x ->
          GroupedResourceCount'
            Lude.<$> (x Lude..: "GroupName") Lude.<*> (x Lude..: "ResourceCount")
      )
