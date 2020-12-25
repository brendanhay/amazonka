{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.FileSystemSize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.FileSystemSize
  ( FileSystemSize (..),

    -- * Smart constructor
    mkFileSystemSize,

    -- * Lenses
    fssValue,
    fssTimestamp,
    fssValueInIA,
    fssValueInStandard,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value represents the actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not necessarily the exact size the file system was at any instant in time.
--
-- /See:/ 'mkFileSystemSize' smart constructor.
data FileSystemSize = FileSystemSize'
  { -- | The latest known metered size (in bytes) of data stored in the file system.
    value :: Core.Natural,
    -- | The time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
    timestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The latest known metered size (in bytes) of data stored in the Infrequent Access storage class.
    valueInIA :: Core.Maybe Core.Natural,
    -- | The latest known metered size (in bytes) of data stored in the Standard storage class.
    valueInStandard :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FileSystemSize' value with any optional fields omitted.
mkFileSystemSize ::
  -- | 'value'
  Core.Natural ->
  FileSystemSize
mkFileSystemSize value =
  FileSystemSize'
    { value,
      timestamp = Core.Nothing,
      valueInIA = Core.Nothing,
      valueInStandard = Core.Nothing
    }

-- | The latest known metered size (in bytes) of data stored in the file system.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssValue :: Lens.Lens' FileSystemSize Core.Natural
fssValue = Lens.field @"value"
{-# DEPRECATED fssValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssTimestamp :: Lens.Lens' FileSystemSize (Core.Maybe Core.NominalDiffTime)
fssTimestamp = Lens.field @"timestamp"
{-# DEPRECATED fssTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The latest known metered size (in bytes) of data stored in the Infrequent Access storage class.
--
-- /Note:/ Consider using 'valueInIA' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssValueInIA :: Lens.Lens' FileSystemSize (Core.Maybe Core.Natural)
fssValueInIA = Lens.field @"valueInIA"
{-# DEPRECATED fssValueInIA "Use generic-lens or generic-optics with 'valueInIA' instead." #-}

-- | The latest known metered size (in bytes) of data stored in the Standard storage class.
--
-- /Note:/ Consider using 'valueInStandard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssValueInStandard :: Lens.Lens' FileSystemSize (Core.Maybe Core.Natural)
fssValueInStandard = Lens.field @"valueInStandard"
{-# DEPRECATED fssValueInStandard "Use generic-lens or generic-optics with 'valueInStandard' instead." #-}

instance Core.FromJSON FileSystemSize where
  parseJSON =
    Core.withObject "FileSystemSize" Core.$
      \x ->
        FileSystemSize'
          Core.<$> (x Core..: "Value")
          Core.<*> (x Core..:? "Timestamp")
          Core.<*> (x Core..:? "ValueInIA")
          Core.<*> (x Core..:? "ValueInStandard")
