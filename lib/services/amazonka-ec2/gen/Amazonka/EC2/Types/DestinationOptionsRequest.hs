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
-- Module      : Amazonka.EC2.Types.DestinationOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DestinationOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DestinationFileFormat
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination options for a flow log.
--
-- /See:/ 'newDestinationOptionsRequest' smart constructor.
data DestinationOptionsRequest = DestinationOptionsRequest'
  { -- | The format for the flow log. The default is @plain-text@.
    fileFormat :: Prelude.Maybe DestinationFileFormat,
    -- | Indicates whether to use Hive-compatible prefixes for flow logs stored
    -- in Amazon S3. The default is @false@.
    hiveCompatiblePartitions :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to partition the flow log per hour. This reduces the
    -- cost and response time for queries. The default is @false@.
    perHourPartition :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileFormat', 'destinationOptionsRequest_fileFormat' - The format for the flow log. The default is @plain-text@.
--
-- 'hiveCompatiblePartitions', 'destinationOptionsRequest_hiveCompatiblePartitions' - Indicates whether to use Hive-compatible prefixes for flow logs stored
-- in Amazon S3. The default is @false@.
--
-- 'perHourPartition', 'destinationOptionsRequest_perHourPartition' - Indicates whether to partition the flow log per hour. This reduces the
-- cost and response time for queries. The default is @false@.
newDestinationOptionsRequest ::
  DestinationOptionsRequest
newDestinationOptionsRequest =
  DestinationOptionsRequest'
    { fileFormat =
        Prelude.Nothing,
      hiveCompatiblePartitions = Prelude.Nothing,
      perHourPartition = Prelude.Nothing
    }

-- | The format for the flow log. The default is @plain-text@.
destinationOptionsRequest_fileFormat :: Lens.Lens' DestinationOptionsRequest (Prelude.Maybe DestinationFileFormat)
destinationOptionsRequest_fileFormat = Lens.lens (\DestinationOptionsRequest' {fileFormat} -> fileFormat) (\s@DestinationOptionsRequest' {} a -> s {fileFormat = a} :: DestinationOptionsRequest)

-- | Indicates whether to use Hive-compatible prefixes for flow logs stored
-- in Amazon S3. The default is @false@.
destinationOptionsRequest_hiveCompatiblePartitions :: Lens.Lens' DestinationOptionsRequest (Prelude.Maybe Prelude.Bool)
destinationOptionsRequest_hiveCompatiblePartitions = Lens.lens (\DestinationOptionsRequest' {hiveCompatiblePartitions} -> hiveCompatiblePartitions) (\s@DestinationOptionsRequest' {} a -> s {hiveCompatiblePartitions = a} :: DestinationOptionsRequest)

-- | Indicates whether to partition the flow log per hour. This reduces the
-- cost and response time for queries. The default is @false@.
destinationOptionsRequest_perHourPartition :: Lens.Lens' DestinationOptionsRequest (Prelude.Maybe Prelude.Bool)
destinationOptionsRequest_perHourPartition = Lens.lens (\DestinationOptionsRequest' {perHourPartition} -> perHourPartition) (\s@DestinationOptionsRequest' {} a -> s {perHourPartition = a} :: DestinationOptionsRequest)

instance Prelude.Hashable DestinationOptionsRequest where
  hashWithSalt _salt DestinationOptionsRequest' {..} =
    _salt
      `Prelude.hashWithSalt` fileFormat
      `Prelude.hashWithSalt` hiveCompatiblePartitions
      `Prelude.hashWithSalt` perHourPartition

instance Prelude.NFData DestinationOptionsRequest where
  rnf DestinationOptionsRequest' {..} =
    Prelude.rnf fileFormat `Prelude.seq`
      Prelude.rnf hiveCompatiblePartitions `Prelude.seq`
        Prelude.rnf perHourPartition

instance Data.ToQuery DestinationOptionsRequest where
  toQuery DestinationOptionsRequest' {..} =
    Prelude.mconcat
      [ "FileFormat" Data.=: fileFormat,
        "HiveCompatiblePartitions"
          Data.=: hiveCompatiblePartitions,
        "PerHourPartition" Data.=: perHourPartition
      ]
