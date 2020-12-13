{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionInput
  ( PartitionInput (..),

    -- * Smart constructor
    mkPartitionInput,

    -- * Lenses
    piValues,
    piLastAnalyzedTime,
    piStorageDescriptor,
    piParameters,
    piLastAccessTime,
  )
where

import Network.AWS.Glue.Types.StorageDescriptor
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The structure used to create and update a partition.
--
-- /See:/ 'mkPartitionInput' smart constructor.
data PartitionInput = PartitionInput'
  { -- | The values of the partition. Although this parameter is not required by the SDK, you must specify this parameter for a valid input.
    --
    -- The values for the keys for the new partition must be passed as an array of String objects that must be ordered in the same order as the partition keys appearing in the Amazon S3 prefix. Otherwise AWS Glue will add the values to the wrong keys.
    values :: Lude.Maybe [Lude.Text],
    -- | The last time at which column statistics were computed for this partition.
    lastAnalyzedTime :: Lude.Maybe Lude.Timestamp,
    -- | Provides information about the physical location where the partition is stored.
    storageDescriptor :: Lude.Maybe StorageDescriptor,
    -- | These key-value pairs define partition parameters.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The last time at which the partition was accessed.
    lastAccessTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartitionInput' with the minimum fields required to make a request.
--
-- * 'values' - The values of the partition. Although this parameter is not required by the SDK, you must specify this parameter for a valid input.
--
-- The values for the keys for the new partition must be passed as an array of String objects that must be ordered in the same order as the partition keys appearing in the Amazon S3 prefix. Otherwise AWS Glue will add the values to the wrong keys.
-- * 'lastAnalyzedTime' - The last time at which column statistics were computed for this partition.
-- * 'storageDescriptor' - Provides information about the physical location where the partition is stored.
-- * 'parameters' - These key-value pairs define partition parameters.
-- * 'lastAccessTime' - The last time at which the partition was accessed.
mkPartitionInput ::
  PartitionInput
mkPartitionInput =
  PartitionInput'
    { values = Lude.Nothing,
      lastAnalyzedTime = Lude.Nothing,
      storageDescriptor = Lude.Nothing,
      parameters = Lude.Nothing,
      lastAccessTime = Lude.Nothing
    }

-- | The values of the partition. Although this parameter is not required by the SDK, you must specify this parameter for a valid input.
--
-- The values for the keys for the new partition must be passed as an array of String objects that must be ordered in the same order as the partition keys appearing in the Amazon S3 prefix. Otherwise AWS Glue will add the values to the wrong keys.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piValues :: Lens.Lens' PartitionInput (Lude.Maybe [Lude.Text])
piValues = Lens.lens (values :: PartitionInput -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: PartitionInput)
{-# DEPRECATED piValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The last time at which column statistics were computed for this partition.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piLastAnalyzedTime :: Lens.Lens' PartitionInput (Lude.Maybe Lude.Timestamp)
piLastAnalyzedTime = Lens.lens (lastAnalyzedTime :: PartitionInput -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAnalyzedTime = a} :: PartitionInput)
{-# DEPRECATED piLastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead." #-}

-- | Provides information about the physical location where the partition is stored.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piStorageDescriptor :: Lens.Lens' PartitionInput (Lude.Maybe StorageDescriptor)
piStorageDescriptor = Lens.lens (storageDescriptor :: PartitionInput -> Lude.Maybe StorageDescriptor) (\s a -> s {storageDescriptor = a} :: PartitionInput)
{-# DEPRECATED piStorageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead." #-}

-- | These key-value pairs define partition parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piParameters :: Lens.Lens' PartitionInput (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
piParameters = Lens.lens (parameters :: PartitionInput -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: PartitionInput)
{-# DEPRECATED piParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The last time at which the partition was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piLastAccessTime :: Lens.Lens' PartitionInput (Lude.Maybe Lude.Timestamp)
piLastAccessTime = Lens.lens (lastAccessTime :: PartitionInput -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessTime = a} :: PartitionInput)
{-# DEPRECATED piLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

instance Lude.ToJSON PartitionInput where
  toJSON PartitionInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            ("LastAnalyzedTime" Lude..=) Lude.<$> lastAnalyzedTime,
            ("StorageDescriptor" Lude..=) Lude.<$> storageDescriptor,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("LastAccessTime" Lude..=) Lude.<$> lastAccessTime
          ]
      )
