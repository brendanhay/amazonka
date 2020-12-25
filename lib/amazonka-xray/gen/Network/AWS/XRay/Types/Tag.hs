{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Tag
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
import qualified Network.AWS.XRay.Types.Key as Types
import qualified Network.AWS.XRay.Types.Value as Types

-- | A map that contains tag keys and tag values to attach to an AWS X-Ray group or sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use. You cannot edit or delete system tags.
--
--
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | A tag key, such as @Stage@ or @Name@ . A tag key cannot be empty. The key can be a maximum of 128 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
    key :: Types.Key,
    -- | An optional tag value, such as @Production@ or @test-only@ . The value can be a maximum of 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag ::
  -- | 'key'
  Types.Key ->
  -- | 'value'
  Types.Value ->
  Tag
mkTag key value = Tag' {key, value}

-- | A tag key, such as @Stage@ or @Name@ . A tag key cannot be empty. The key can be a maximum of 128 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | An optional tag value, such as @Production@ or @test-only@ . The value can be a maximum of 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Types.Value
tValue = Lens.field @"value"
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Tag where
  toJSON Tag {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value)]
      )

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject "Tag" Core.$
      \x -> Tag' Core.<$> (x Core..: "Key") Core.<*> (x Core..: "Value")
