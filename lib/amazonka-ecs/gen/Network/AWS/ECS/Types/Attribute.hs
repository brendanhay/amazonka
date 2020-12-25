{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aName,
    aTargetId,
    aTargetType,
    aValue,
  )
where

import qualified Network.AWS.ECS.Types.Name as Types
import qualified Network.AWS.ECS.Types.TargetId as Types
import qualified Network.AWS.ECS.Types.TargetType as Types
import qualified Network.AWS.ECS.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The name of the attribute. The @name@ must contain between 1 and 128 characters and name may contain letters (uppercase and lowercase), numbers, hyphens, underscores, forward slashes, back slashes, or periods.
    name :: Types.Name,
    -- | The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
    targetId :: Core.Maybe Types.TargetId,
    -- | The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full ARN.
    targetType :: Core.Maybe Types.TargetType,
    -- | The value of the attribute. The @value@ must contain between 1 and 128 characters and may contain letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, back slashes, colons, or spaces. The value cannot contain any leading or trailing whitespace.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Attribute' value with any optional fields omitted.
mkAttribute ::
  -- | 'name'
  Types.Name ->
  Attribute
mkAttribute name =
  Attribute'
    { name,
      targetId = Core.Nothing,
      targetType = Core.Nothing,
      value = Core.Nothing
    }

-- | The name of the attribute. The @name@ must contain between 1 and 128 characters and name may contain letters (uppercase and lowercase), numbers, hyphens, underscores, forward slashes, back slashes, or periods.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Attribute Types.Name
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTargetId :: Lens.Lens' Attribute (Core.Maybe Types.TargetId)
aTargetId = Lens.field @"targetId"
{-# DEPRECATED aTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full ARN.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTargetType :: Lens.Lens' Attribute (Core.Maybe Types.TargetType)
aTargetType = Lens.field @"targetType"
{-# DEPRECATED aTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The value of the attribute. The @value@ must contain between 1 and 128 characters and may contain letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, back slashes, colons, or spaces. The value cannot contain any leading or trailing whitespace.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Core.Maybe Types.Value)
aValue = Lens.field @"value"
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Attribute where
  toJSON Attribute {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("targetId" Core..=) Core.<$> targetId,
            ("targetType" Core..=) Core.<$> targetType,
            ("value" Core..=) Core.<$> value
          ]
      )

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject "Attribute" Core.$
      \x ->
        Attribute'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..:? "targetId")
          Core.<*> (x Core..:? "targetType")
          Core.<*> (x Core..:? "value")
