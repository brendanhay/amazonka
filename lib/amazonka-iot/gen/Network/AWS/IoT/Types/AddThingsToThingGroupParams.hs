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

import qualified Network.AWS.IoT.Types.ThingGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameters used when defining a mitigation action that move a set of things to a thing group.
--
-- /See:/ 'mkAddThingsToThingGroupParams' smart constructor.
data AddThingsToThingGroupParams = AddThingsToThingGroupParams'
  { -- | The list of groups to which you want to add the things that triggered the mitigation action. You can add a thing to a maximum of 10 groups, but you cannot add a thing to more than one group in the same hierarchy.
    thingGroupNames :: Core.NonEmpty Types.ThingGroupName,
    -- | Specifies if this mitigation action can move the things that triggered the mitigation action even if they are part of one or more dynamic things groups.
    overrideDynamicGroups :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddThingsToThingGroupParams' value with any optional fields omitted.
mkAddThingsToThingGroupParams ::
  -- | 'thingGroupNames'
  Core.NonEmpty Types.ThingGroupName ->
  AddThingsToThingGroupParams
mkAddThingsToThingGroupParams thingGroupNames =
  AddThingsToThingGroupParams'
    { thingGroupNames,
      overrideDynamicGroups = Core.Nothing
    }

-- | The list of groups to which you want to add the things that triggered the mitigation action. You can add a thing to a maximum of 10 groups, but you cannot add a thing to more than one group in the same hierarchy.
--
-- /Note:/ Consider using 'thingGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgpThingGroupNames :: Lens.Lens' AddThingsToThingGroupParams (Core.NonEmpty Types.ThingGroupName)
atttgpThingGroupNames = Lens.field @"thingGroupNames"
{-# DEPRECATED atttgpThingGroupNames "Use generic-lens or generic-optics with 'thingGroupNames' instead." #-}

-- | Specifies if this mitigation action can move the things that triggered the mitigation action even if they are part of one or more dynamic things groups.
--
-- /Note:/ Consider using 'overrideDynamicGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgpOverrideDynamicGroups :: Lens.Lens' AddThingsToThingGroupParams (Core.Maybe Core.Bool)
atttgpOverrideDynamicGroups = Lens.field @"overrideDynamicGroups"
{-# DEPRECATED atttgpOverrideDynamicGroups "Use generic-lens or generic-optics with 'overrideDynamicGroups' instead." #-}

instance Core.FromJSON AddThingsToThingGroupParams where
  toJSON AddThingsToThingGroupParams {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("thingGroupNames" Core..= thingGroupNames),
            ("overrideDynamicGroups" Core..=) Core.<$> overrideDynamicGroups
          ]
      )

instance Core.FromJSON AddThingsToThingGroupParams where
  parseJSON =
    Core.withObject "AddThingsToThingGroupParams" Core.$
      \x ->
        AddThingsToThingGroupParams'
          Core.<$> (x Core..: "thingGroupNames")
          Core.<*> (x Core..:? "overrideDynamicGroups")
