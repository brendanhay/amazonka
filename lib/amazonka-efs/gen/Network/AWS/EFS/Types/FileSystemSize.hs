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
    fssValueInIA,
    fssValueInStandard,
    fssTimestamp,
    fssValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value represents the actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not necessarily the exact size the file system was at any instant in time.
--
-- /See:/ 'mkFileSystemSize' smart constructor.
data FileSystemSize = FileSystemSize'
  { valueInIA ::
      Lude.Maybe Lude.Natural,
    valueInStandard :: Lude.Maybe Lude.Natural,
    timestamp :: Lude.Maybe Lude.Timestamp,
    value :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileSystemSize' with the minimum fields required to make a request.
--
-- * 'timestamp' - The time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
-- * 'value' - The latest known metered size (in bytes) of data stored in the file system.
-- * 'valueInIA' - The latest known metered size (in bytes) of data stored in the Infrequent Access storage class.
-- * 'valueInStandard' - The latest known metered size (in bytes) of data stored in the Standard storage class.
mkFileSystemSize ::
  -- | 'value'
  Lude.Natural ->
  FileSystemSize
mkFileSystemSize pValue_ =
  FileSystemSize'
    { valueInIA = Lude.Nothing,
      valueInStandard = Lude.Nothing,
      timestamp = Lude.Nothing,
      value = pValue_
    }

-- | The latest known metered size (in bytes) of data stored in the Infrequent Access storage class.
--
-- /Note:/ Consider using 'valueInIA' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssValueInIA :: Lens.Lens' FileSystemSize (Lude.Maybe Lude.Natural)
fssValueInIA = Lens.lens (valueInIA :: FileSystemSize -> Lude.Maybe Lude.Natural) (\s a -> s {valueInIA = a} :: FileSystemSize)
{-# DEPRECATED fssValueInIA "Use generic-lens or generic-optics with 'valueInIA' instead." #-}

-- | The latest known metered size (in bytes) of data stored in the Standard storage class.
--
-- /Note:/ Consider using 'valueInStandard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssValueInStandard :: Lens.Lens' FileSystemSize (Lude.Maybe Lude.Natural)
fssValueInStandard = Lens.lens (valueInStandard :: FileSystemSize -> Lude.Maybe Lude.Natural) (\s a -> s {valueInStandard = a} :: FileSystemSize)
{-# DEPRECATED fssValueInStandard "Use generic-lens or generic-optics with 'valueInStandard' instead." #-}

-- | The time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssTimestamp :: Lens.Lens' FileSystemSize (Lude.Maybe Lude.Timestamp)
fssTimestamp = Lens.lens (timestamp :: FileSystemSize -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: FileSystemSize)
{-# DEPRECATED fssTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The latest known metered size (in bytes) of data stored in the file system.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssValue :: Lens.Lens' FileSystemSize Lude.Natural
fssValue = Lens.lens (value :: FileSystemSize -> Lude.Natural) (\s a -> s {value = a} :: FileSystemSize)
{-# DEPRECATED fssValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON FileSystemSize where
  parseJSON =
    Lude.withObject
      "FileSystemSize"
      ( \x ->
          FileSystemSize'
            Lude.<$> (x Lude..:? "ValueInIA")
            Lude.<*> (x Lude..:? "ValueInStandard")
            Lude.<*> (x Lude..:? "Timestamp")
            Lude.<*> (x Lude..: "Value")
      )
