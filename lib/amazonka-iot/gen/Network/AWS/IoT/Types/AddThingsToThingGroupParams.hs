{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AddThingsToThingGroupParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AddThingsToThingGroupParams
  ( AddThingsToThingGroupParams (..),

    -- * Smart constructor
    mkAddThingsToThingGroupParams,

    -- * Lenses
    atttgpThingGroupNames,
    atttgpOverrideDynamicGroups,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Parameters used when defining a mitigation action that move a set of things to a thing group.
--
-- /See:/ 'mkAddThingsToThingGroupParams' smart constructor.
data AddThingsToThingGroupParams = AddThingsToThingGroupParams'
  { -- | The list of groups to which you want to add the things that triggered the mitigation action. You can add a thing to a maximum of 10 groups, but you cannot add a thing to more than one group in the same hierarchy.
    thingGroupNames :: Lude.NonEmpty Lude.Text,
    -- | Specifies if this mitigation action can move the things that triggered the mitigation action even if they are part of one or more dynamic things groups.
    overrideDynamicGroups :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddThingsToThingGroupParams' with the minimum fields required to make a request.
--
-- * 'thingGroupNames' - The list of groups to which you want to add the things that triggered the mitigation action. You can add a thing to a maximum of 10 groups, but you cannot add a thing to more than one group in the same hierarchy.
-- * 'overrideDynamicGroups' - Specifies if this mitigation action can move the things that triggered the mitigation action even if they are part of one or more dynamic things groups.
mkAddThingsToThingGroupParams ::
  -- | 'thingGroupNames'
  Lude.NonEmpty Lude.Text ->
  AddThingsToThingGroupParams
mkAddThingsToThingGroupParams pThingGroupNames_ =
  AddThingsToThingGroupParams'
    { thingGroupNames = pThingGroupNames_,
      overrideDynamicGroups = Lude.Nothing
    }

-- | The list of groups to which you want to add the things that triggered the mitigation action. You can add a thing to a maximum of 10 groups, but you cannot add a thing to more than one group in the same hierarchy.
--
-- /Note:/ Consider using 'thingGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgpThingGroupNames :: Lens.Lens' AddThingsToThingGroupParams (Lude.NonEmpty Lude.Text)
atttgpThingGroupNames = Lens.lens (thingGroupNames :: AddThingsToThingGroupParams -> Lude.NonEmpty Lude.Text) (\s a -> s {thingGroupNames = a} :: AddThingsToThingGroupParams)
{-# DEPRECATED atttgpThingGroupNames "Use generic-lens or generic-optics with 'thingGroupNames' instead." #-}

-- | Specifies if this mitigation action can move the things that triggered the mitigation action even if they are part of one or more dynamic things groups.
--
-- /Note:/ Consider using 'overrideDynamicGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgpOverrideDynamicGroups :: Lens.Lens' AddThingsToThingGroupParams (Lude.Maybe Lude.Bool)
atttgpOverrideDynamicGroups = Lens.lens (overrideDynamicGroups :: AddThingsToThingGroupParams -> Lude.Maybe Lude.Bool) (\s a -> s {overrideDynamicGroups = a} :: AddThingsToThingGroupParams)
{-# DEPRECATED atttgpOverrideDynamicGroups "Use generic-lens or generic-optics with 'overrideDynamicGroups' instead." #-}

instance Lude.FromJSON AddThingsToThingGroupParams where
  parseJSON =
    Lude.withObject
      "AddThingsToThingGroupParams"
      ( \x ->
          AddThingsToThingGroupParams'
            Lude.<$> (x Lude..: "thingGroupNames")
            Lude.<*> (x Lude..:? "overrideDynamicGroups")
      )

instance Lude.ToJSON AddThingsToThingGroupParams where
  toJSON AddThingsToThingGroupParams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("thingGroupNames" Lude..= thingGroupNames),
            ("overrideDynamicGroups" Lude..=) Lude.<$> overrideDynamicGroups
          ]
      )
