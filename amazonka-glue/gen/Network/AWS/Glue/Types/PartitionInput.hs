{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionInput where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.StorageDescriptor
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The structure used to create and update a partition.
--
-- /See:/ 'newPartitionInput' smart constructor.
data PartitionInput = PartitionInput'
  { -- | The values of the partition. Although this parameter is not required by
    -- the SDK, you must specify this parameter for a valid input.
    --
    -- The values for the keys for the new partition must be passed as an array
    -- of String objects that must be ordered in the same order as the
    -- partition keys appearing in the Amazon S3 prefix. Otherwise AWS Glue
    -- will add the values to the wrong keys.
    values :: Prelude.Maybe [Prelude.Text],
    -- | Provides information about the physical location where the partition is
    -- stored.
    storageDescriptor :: Prelude.Maybe StorageDescriptor,
    -- | The last time at which column statistics were computed for this
    -- partition.
    lastAnalyzedTime :: Prelude.Maybe Core.POSIX,
    -- | The last time at which the partition was accessed.
    lastAccessTime :: Prelude.Maybe Core.POSIX,
    -- | These key-value pairs define partition parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartitionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'partitionInput_values' - The values of the partition. Although this parameter is not required by
-- the SDK, you must specify this parameter for a valid input.
--
-- The values for the keys for the new partition must be passed as an array
-- of String objects that must be ordered in the same order as the
-- partition keys appearing in the Amazon S3 prefix. Otherwise AWS Glue
-- will add the values to the wrong keys.
--
-- 'storageDescriptor', 'partitionInput_storageDescriptor' - Provides information about the physical location where the partition is
-- stored.
--
-- 'lastAnalyzedTime', 'partitionInput_lastAnalyzedTime' - The last time at which column statistics were computed for this
-- partition.
--
-- 'lastAccessTime', 'partitionInput_lastAccessTime' - The last time at which the partition was accessed.
--
-- 'parameters', 'partitionInput_parameters' - These key-value pairs define partition parameters.
newPartitionInput ::
  PartitionInput
newPartitionInput =
  PartitionInput'
    { values = Prelude.Nothing,
      storageDescriptor = Prelude.Nothing,
      lastAnalyzedTime = Prelude.Nothing,
      lastAccessTime = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The values of the partition. Although this parameter is not required by
-- the SDK, you must specify this parameter for a valid input.
--
-- The values for the keys for the new partition must be passed as an array
-- of String objects that must be ordered in the same order as the
-- partition keys appearing in the Amazon S3 prefix. Otherwise AWS Glue
-- will add the values to the wrong keys.
partitionInput_values :: Lens.Lens' PartitionInput (Prelude.Maybe [Prelude.Text])
partitionInput_values = Lens.lens (\PartitionInput' {values} -> values) (\s@PartitionInput' {} a -> s {values = a} :: PartitionInput) Prelude.. Lens.mapping Lens._Coerce

-- | Provides information about the physical location where the partition is
-- stored.
partitionInput_storageDescriptor :: Lens.Lens' PartitionInput (Prelude.Maybe StorageDescriptor)
partitionInput_storageDescriptor = Lens.lens (\PartitionInput' {storageDescriptor} -> storageDescriptor) (\s@PartitionInput' {} a -> s {storageDescriptor = a} :: PartitionInput)

-- | The last time at which column statistics were computed for this
-- partition.
partitionInput_lastAnalyzedTime :: Lens.Lens' PartitionInput (Prelude.Maybe Prelude.UTCTime)
partitionInput_lastAnalyzedTime = Lens.lens (\PartitionInput' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@PartitionInput' {} a -> s {lastAnalyzedTime = a} :: PartitionInput) Prelude.. Lens.mapping Core._Time

-- | The last time at which the partition was accessed.
partitionInput_lastAccessTime :: Lens.Lens' PartitionInput (Prelude.Maybe Prelude.UTCTime)
partitionInput_lastAccessTime = Lens.lens (\PartitionInput' {lastAccessTime} -> lastAccessTime) (\s@PartitionInput' {} a -> s {lastAccessTime = a} :: PartitionInput) Prelude.. Lens.mapping Core._Time

-- | These key-value pairs define partition parameters.
partitionInput_parameters :: Lens.Lens' PartitionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
partitionInput_parameters = Lens.lens (\PartitionInput' {parameters} -> parameters) (\s@PartitionInput' {} a -> s {parameters = a} :: PartitionInput) Prelude.. Lens.mapping Lens._Coerce

instance Prelude.Hashable PartitionInput

instance Prelude.NFData PartitionInput

instance Core.ToJSON PartitionInput where
  toJSON PartitionInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("StorageDescriptor" Core..=)
              Prelude.<$> storageDescriptor,
            ("LastAnalyzedTime" Core..=)
              Prelude.<$> lastAnalyzedTime,
            ("LastAccessTime" Core..=)
              Prelude.<$> lastAccessTime,
            ("Parameters" Core..=) Prelude.<$> parameters
          ]
      )
