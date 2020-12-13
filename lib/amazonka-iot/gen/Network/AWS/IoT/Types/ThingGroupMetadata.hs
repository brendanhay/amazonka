{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupMetadata
  ( ThingGroupMetadata (..),

    -- * Smart constructor
    mkThingGroupMetadata,

    -- * Lenses
    tgmRootToParentThingGroups,
    tgmParentGroupName,
    tgmCreationDate,
  )
where

import Network.AWS.IoT.Types.GroupNameAndARN
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Thing group metadata.
--
-- /See:/ 'mkThingGroupMetadata' smart constructor.
data ThingGroupMetadata = ThingGroupMetadata'
  { -- | The root parent thing group.
    rootToParentThingGroups :: Lude.Maybe [GroupNameAndARN],
    -- | The parent thing group name.
    parentGroupName :: Lude.Maybe Lude.Text,
    -- | The UNIX timestamp of when the thing group was created.
    creationDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingGroupMetadata' with the minimum fields required to make a request.
--
-- * 'rootToParentThingGroups' - The root parent thing group.
-- * 'parentGroupName' - The parent thing group name.
-- * 'creationDate' - The UNIX timestamp of when the thing group was created.
mkThingGroupMetadata ::
  ThingGroupMetadata
mkThingGroupMetadata =
  ThingGroupMetadata'
    { rootToParentThingGroups = Lude.Nothing,
      parentGroupName = Lude.Nothing,
      creationDate = Lude.Nothing
    }

-- | The root parent thing group.
--
-- /Note:/ Consider using 'rootToParentThingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmRootToParentThingGroups :: Lens.Lens' ThingGroupMetadata (Lude.Maybe [GroupNameAndARN])
tgmRootToParentThingGroups = Lens.lens (rootToParentThingGroups :: ThingGroupMetadata -> Lude.Maybe [GroupNameAndARN]) (\s a -> s {rootToParentThingGroups = a} :: ThingGroupMetadata)
{-# DEPRECATED tgmRootToParentThingGroups "Use generic-lens or generic-optics with 'rootToParentThingGroups' instead." #-}

-- | The parent thing group name.
--
-- /Note:/ Consider using 'parentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmParentGroupName :: Lens.Lens' ThingGroupMetadata (Lude.Maybe Lude.Text)
tgmParentGroupName = Lens.lens (parentGroupName :: ThingGroupMetadata -> Lude.Maybe Lude.Text) (\s a -> s {parentGroupName = a} :: ThingGroupMetadata)
{-# DEPRECATED tgmParentGroupName "Use generic-lens or generic-optics with 'parentGroupName' instead." #-}

-- | The UNIX timestamp of when the thing group was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmCreationDate :: Lens.Lens' ThingGroupMetadata (Lude.Maybe Lude.Timestamp)
tgmCreationDate = Lens.lens (creationDate :: ThingGroupMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ThingGroupMetadata)
{-# DEPRECATED tgmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON ThingGroupMetadata where
  parseJSON =
    Lude.withObject
      "ThingGroupMetadata"
      ( \x ->
          ThingGroupMetadata'
            Lude.<$> (x Lude..:? "rootToParentThingGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "parentGroupName")
            Lude.<*> (x Lude..:? "creationDate")
      )
