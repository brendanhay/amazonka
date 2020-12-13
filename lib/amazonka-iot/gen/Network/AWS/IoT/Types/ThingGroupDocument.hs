{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupDocument
  ( ThingGroupDocument (..),

    -- * Smart constructor
    mkThingGroupDocument,

    -- * Lenses
    tgdParentGroupNames,
    tgdThingGroupId,
    tgdThingGroupName,
    tgdAttributes,
    tgdThingGroupDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The thing group search index document.
--
-- /See:/ 'mkThingGroupDocument' smart constructor.
data ThingGroupDocument = ThingGroupDocument'
  { -- | Parent group names.
    parentGroupNames :: Lude.Maybe [Lude.Text],
    -- | The thing group ID.
    thingGroupId :: Lude.Maybe Lude.Text,
    -- | The thing group name.
    thingGroupName :: Lude.Maybe Lude.Text,
    -- | The thing group attributes.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The thing group description.
    thingGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingGroupDocument' with the minimum fields required to make a request.
--
-- * 'parentGroupNames' - Parent group names.
-- * 'thingGroupId' - The thing group ID.
-- * 'thingGroupName' - The thing group name.
-- * 'attributes' - The thing group attributes.
-- * 'thingGroupDescription' - The thing group description.
mkThingGroupDocument ::
  ThingGroupDocument
mkThingGroupDocument =
  ThingGroupDocument'
    { parentGroupNames = Lude.Nothing,
      thingGroupId = Lude.Nothing,
      thingGroupName = Lude.Nothing,
      attributes = Lude.Nothing,
      thingGroupDescription = Lude.Nothing
    }

-- | Parent group names.
--
-- /Note:/ Consider using 'parentGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdParentGroupNames :: Lens.Lens' ThingGroupDocument (Lude.Maybe [Lude.Text])
tgdParentGroupNames = Lens.lens (parentGroupNames :: ThingGroupDocument -> Lude.Maybe [Lude.Text]) (\s a -> s {parentGroupNames = a} :: ThingGroupDocument)
{-# DEPRECATED tgdParentGroupNames "Use generic-lens or generic-optics with 'parentGroupNames' instead." #-}

-- | The thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdThingGroupId :: Lens.Lens' ThingGroupDocument (Lude.Maybe Lude.Text)
tgdThingGroupId = Lens.lens (thingGroupId :: ThingGroupDocument -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupId = a} :: ThingGroupDocument)
{-# DEPRECATED tgdThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | The thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdThingGroupName :: Lens.Lens' ThingGroupDocument (Lude.Maybe Lude.Text)
tgdThingGroupName = Lens.lens (thingGroupName :: ThingGroupDocument -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupName = a} :: ThingGroupDocument)
{-# DEPRECATED tgdThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The thing group attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdAttributes :: Lens.Lens' ThingGroupDocument (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tgdAttributes = Lens.lens (attributes :: ThingGroupDocument -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: ThingGroupDocument)
{-# DEPRECATED tgdAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The thing group description.
--
-- /Note:/ Consider using 'thingGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdThingGroupDescription :: Lens.Lens' ThingGroupDocument (Lude.Maybe Lude.Text)
tgdThingGroupDescription = Lens.lens (thingGroupDescription :: ThingGroupDocument -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupDescription = a} :: ThingGroupDocument)
{-# DEPRECATED tgdThingGroupDescription "Use generic-lens or generic-optics with 'thingGroupDescription' instead." #-}

instance Lude.FromJSON ThingGroupDocument where
  parseJSON =
    Lude.withObject
      "ThingGroupDocument"
      ( \x ->
          ThingGroupDocument'
            Lude.<$> (x Lude..:? "parentGroupNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "thingGroupId")
            Lude.<*> (x Lude..:? "thingGroupName")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "thingGroupDescription")
      )
