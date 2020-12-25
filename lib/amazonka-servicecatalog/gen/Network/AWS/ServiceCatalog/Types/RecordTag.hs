{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordTag
  ( RecordTag (..),

    -- * Smart constructor
    mkRecordTag,

    -- * Lenses
    rtKey,
    rtValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Key as Types
import qualified Network.AWS.ServiceCatalog.Types.Value as Types

-- | Information about a tag, which is a key-value pair.
--
-- /See:/ 'mkRecordTag' smart constructor.
data RecordTag = RecordTag'
  { -- | The key for this tag.
    key :: Core.Maybe Types.Key,
    -- | The value for this tag.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecordTag' value with any optional fields omitted.
mkRecordTag ::
  RecordTag
mkRecordTag = RecordTag' {key = Core.Nothing, value = Core.Nothing}

-- | The key for this tag.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtKey :: Lens.Lens' RecordTag (Core.Maybe Types.Key)
rtKey = Lens.field @"key"
{-# DEPRECATED rtKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value for this tag.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtValue :: Lens.Lens' RecordTag (Core.Maybe Types.Value)
rtValue = Lens.field @"value"
{-# DEPRECATED rtValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON RecordTag where
  parseJSON =
    Core.withObject "RecordTag" Core.$
      \x ->
        RecordTag'
          Core.<$> (x Core..:? "Key") Core.<*> (x Core..:? "Value")
