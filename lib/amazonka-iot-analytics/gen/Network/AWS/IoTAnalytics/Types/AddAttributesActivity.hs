{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.AddAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.AddAttributesActivity
  ( AddAttributesActivity (..),

    -- * Smart constructor
    mkAddAttributesActivity,

    -- * Lenses
    aaaName,
    aaaAttributes,
    aaaNext,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.ActivityName as Types
import qualified Network.AWS.IoTAnalytics.Types.AttributeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An activity that adds other attributes based on existing attributes in the message.
--
-- /See:/ 'mkAddAttributesActivity' smart constructor.
data AddAttributesActivity = AddAttributesActivity'
  { -- | The name of the addAttributes activity.
    name :: Types.ActivityName,
    -- | A list of 1-50 @AttributeNameMapping@ objects that map an existing attribute to a new attribute.
    attributes :: Core.HashMap Types.AttributeName Types.AttributeName,
    -- | The next activity in the pipeline.
    next :: Core.Maybe Types.ActivityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddAttributesActivity' value with any optional fields omitted.
mkAddAttributesActivity ::
  -- | 'name'
  Types.ActivityName ->
  AddAttributesActivity
mkAddAttributesActivity name =
  AddAttributesActivity'
    { name,
      attributes = Core.mempty,
      next = Core.Nothing
    }

-- | The name of the addAttributes activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaaName :: Lens.Lens' AddAttributesActivity Types.ActivityName
aaaName = Lens.field @"name"
{-# DEPRECATED aaaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of 1-50 @AttributeNameMapping@ objects that map an existing attribute to a new attribute.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaaAttributes :: Lens.Lens' AddAttributesActivity (Core.HashMap Types.AttributeName Types.AttributeName)
aaaAttributes = Lens.field @"attributes"
{-# DEPRECATED aaaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaaNext :: Lens.Lens' AddAttributesActivity (Core.Maybe Types.ActivityName)
aaaNext = Lens.field @"next"
{-# DEPRECATED aaaNext "Use generic-lens or generic-optics with 'next' instead." #-}

instance Core.FromJSON AddAttributesActivity where
  toJSON AddAttributesActivity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("attributes" Core..= attributes),
            ("next" Core..=) Core.<$> next
          ]
      )

instance Core.FromJSON AddAttributesActivity where
  parseJSON =
    Core.withObject "AddAttributesActivity" Core.$
      \x ->
        AddAttributesActivity'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..:? "attributes" Core..!= Core.mempty)
          Core.<*> (x Core..:? "next")
