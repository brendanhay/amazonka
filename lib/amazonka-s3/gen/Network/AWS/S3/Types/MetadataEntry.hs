{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetadataEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetadataEntry
  ( MetadataEntry (..),

    -- * Smart constructor
    mkMetadataEntry,

    -- * Lenses
    meName,
    meValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.MetadataKey as Types
import qualified Network.AWS.S3.Types.MetadataValue as Types

-- | A metadata key-value pair to store with an object.
--
-- /See:/ 'mkMetadataEntry' smart constructor.
data MetadataEntry = MetadataEntry'
  { -- | Name of the Object.
    name :: Core.Maybe Types.MetadataKey,
    -- | Value of the Object.
    value :: Core.Maybe Types.MetadataValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetadataEntry' value with any optional fields omitted.
mkMetadataEntry ::
  MetadataEntry
mkMetadataEntry =
  MetadataEntry' {name = Core.Nothing, value = Core.Nothing}

-- | Name of the Object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meName :: Lens.Lens' MetadataEntry (Core.Maybe Types.MetadataKey)
meName = Lens.field @"name"
{-# DEPRECATED meName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Value of the Object.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meValue :: Lens.Lens' MetadataEntry (Core.Maybe Types.MetadataValue)
meValue = Lens.field @"value"
{-# DEPRECATED meValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.ToXML MetadataEntry where
  toXML MetadataEntry {..} =
    Core.toXMLNode "Name" Core.<$> name
      Core.<> Core.toXMLNode "Value" Core.<$> value
