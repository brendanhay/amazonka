{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tKey,
    tValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.Key as Types
import qualified Network.AWS.Route53.Types.Value as Types

-- | A complex type that contains information about a tag that you want to add or edit for the specified health check or hosted zone.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | The value of @Key@ depends on the operation that you want to perform:
    --
    --
    --     * __Add a tag to a health check or hosted zone__ : @Key@ is the name that you want to give the new tag.
    --
    --
    --     * __Edit a tag__ : @Key@ is the name of the tag that you want to change the @Value@ for.
    --
    --
    --     * __Delete a key__ : @Key@ is the name of the tag you want to remove.
    --
    --
    --     * __Give a name to a health check__ : Edit the default @Name@ tag. In the Amazon Route 53 console, the list of your health checks includes a __Name__ column that lets you see the name that you've given to each health check.
    key :: Core.Maybe Types.Key,
    -- | The value of @Value@ depends on the operation that you want to perform:
    --
    --
    --     * __Add a tag to a health check or hosted zone__ : @Value@ is the value that you want to give the new tag.
    --
    --
    --     * __Edit a tag__ : @Value@ is the new value that you want to assign the tag.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag ::
  Tag
mkTag = Tag' {key = Core.Nothing, value = Core.Nothing}

-- | The value of @Key@ depends on the operation that you want to perform:
--
--
--     * __Add a tag to a health check or hosted zone__ : @Key@ is the name that you want to give the new tag.
--
--
--     * __Edit a tag__ : @Key@ is the name of the tag that you want to change the @Value@ for.
--
--
--     * __Delete a key__ : @Key@ is the name of the tag you want to remove.
--
--
--     * __Give a name to a health check__ : Edit the default @Name@ tag. In the Amazon Route 53 console, the list of your health checks includes a __Name__ column that lets you see the name that you've given to each health check.
--
--
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag (Core.Maybe Types.Key)
tKey = Lens.field @"key"
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value of @Value@ depends on the operation that you want to perform:
--
--
--     * __Add a tag to a health check or hosted zone__ : @Value@ is the value that you want to give the new tag.
--
--
--     * __Edit a tag__ : @Value@ is the new value that you want to assign the tag.
--
--
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag (Core.Maybe Types.Value)
tValue = Lens.field @"value"
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.ToXML Tag where
  toXML Tag {..} =
    Core.toXMLNode "Key" Core.<$> key
      Core.<> Core.toXMLNode "Value" Core.<$> value

instance Core.FromXML Tag where
  parseXML x =
    Tag' Core.<$> (x Core..@? "Key") Core.<*> (x Core..@? "Value")
