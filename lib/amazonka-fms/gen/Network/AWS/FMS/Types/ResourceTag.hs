{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ResourceTag
  ( ResourceTag (..),

    -- * Smart constructor
    mkResourceTag,

    -- * Lenses
    rtKey,
    rtValue,
  )
where

import qualified Network.AWS.FMS.Types.ResourceTagKey as Types
import qualified Network.AWS.FMS.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The resource tags that AWS Firewall Manager uses to determine if a particular resource should be included or excluded from the AWS Firewall Manager policy. Tags enable you to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value. Firewall Manager combines the tags with "AND" so that, if you add more than one tag to a policy scope, a resource must have all the specified tags to be included or excluded. For more information, see <https://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html Working with Tag Editor> .
--
-- /See:/ 'mkResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | The resource tag key.
    key :: Types.ResourceTagKey,
    -- | The resource tag value.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTag' value with any optional fields omitted.
mkResourceTag ::
  -- | 'key'
  Types.ResourceTagKey ->
  ResourceTag
mkResourceTag key = ResourceTag' {key, value = Core.Nothing}

-- | The resource tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtKey :: Lens.Lens' ResourceTag Types.ResourceTagKey
rtKey = Lens.field @"key"
{-# DEPRECATED rtKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The resource tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtValue :: Lens.Lens' ResourceTag (Core.Maybe Types.Value)
rtValue = Lens.field @"value"
{-# DEPRECATED rtValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ResourceTag where
  toJSON ResourceTag {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Key" Core..= key), ("Value" Core..=) Core.<$> value]
      )

instance Core.FromJSON ResourceTag where
  parseJSON =
    Core.withObject "ResourceTag" Core.$
      \x ->
        ResourceTag'
          Core.<$> (x Core..: "Key") Core.<*> (x Core..:? "Value")
