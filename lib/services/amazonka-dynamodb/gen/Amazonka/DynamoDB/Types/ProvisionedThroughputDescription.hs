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
-- Module      : Amazonka.DynamoDB.Types.ProvisionedThroughputDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ProvisionedThroughputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the provisioned throughput settings for the table, consisting
-- of read and write capacity units, along with data about increases and
-- decreases.
--
-- /See:/ 'newProvisionedThroughputDescription' smart constructor.
data ProvisionedThroughputDescription = ProvisionedThroughputDescription'
  { -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@. Eventually consistent
    -- reads require less effort than strongly consistent reads, so a setting
    -- of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent
    -- @ReadCapacityUnits@ per second.
    readCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The number of provisioned throughput decreases for this table during
    -- this UTC calendar day. For current maximums on provisioned throughput
    -- decreases, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
    -- in the /Amazon DynamoDB Developer Guide/.
    numberOfDecreasesToday :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException@.
    writeCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The date and time of the last provisioned throughput increase for this
    -- table.
    lastIncreaseDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time of the last provisioned throughput decrease for this
    -- table.
    lastDecreaseDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedThroughputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readCapacityUnits', 'provisionedThroughputDescription_readCapacityUnits' - The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. Eventually consistent
-- reads require less effort than strongly consistent reads, so a setting
-- of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent
-- @ReadCapacityUnits@ per second.
--
-- 'numberOfDecreasesToday', 'provisionedThroughputDescription_numberOfDecreasesToday' - The number of provisioned throughput decreases for this table during
-- this UTC calendar day. For current maximums on provisioned throughput
-- decreases, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'writeCapacityUnits', 'provisionedThroughputDescription_writeCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@.
--
-- 'lastIncreaseDateTime', 'provisionedThroughputDescription_lastIncreaseDateTime' - The date and time of the last provisioned throughput increase for this
-- table.
--
-- 'lastDecreaseDateTime', 'provisionedThroughputDescription_lastDecreaseDateTime' - The date and time of the last provisioned throughput decrease for this
-- table.
newProvisionedThroughputDescription ::
  ProvisionedThroughputDescription
newProvisionedThroughputDescription =
  ProvisionedThroughputDescription'
    { readCapacityUnits =
        Prelude.Nothing,
      numberOfDecreasesToday = Prelude.Nothing,
      writeCapacityUnits = Prelude.Nothing,
      lastIncreaseDateTime = Prelude.Nothing,
      lastDecreaseDateTime = Prelude.Nothing
    }

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. Eventually consistent
-- reads require less effort than strongly consistent reads, so a setting
-- of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent
-- @ReadCapacityUnits@ per second.
provisionedThroughputDescription_readCapacityUnits :: Lens.Lens' ProvisionedThroughputDescription (Prelude.Maybe Prelude.Natural)
provisionedThroughputDescription_readCapacityUnits = Lens.lens (\ProvisionedThroughputDescription' {readCapacityUnits} -> readCapacityUnits) (\s@ProvisionedThroughputDescription' {} a -> s {readCapacityUnits = a} :: ProvisionedThroughputDescription)

-- | The number of provisioned throughput decreases for this table during
-- this UTC calendar day. For current maximums on provisioned throughput
-- decreases, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
provisionedThroughputDescription_numberOfDecreasesToday :: Lens.Lens' ProvisionedThroughputDescription (Prelude.Maybe Prelude.Natural)
provisionedThroughputDescription_numberOfDecreasesToday = Lens.lens (\ProvisionedThroughputDescription' {numberOfDecreasesToday} -> numberOfDecreasesToday) (\s@ProvisionedThroughputDescription' {} a -> s {numberOfDecreasesToday = a} :: ProvisionedThroughputDescription)

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@.
provisionedThroughputDescription_writeCapacityUnits :: Lens.Lens' ProvisionedThroughputDescription (Prelude.Maybe Prelude.Natural)
provisionedThroughputDescription_writeCapacityUnits = Lens.lens (\ProvisionedThroughputDescription' {writeCapacityUnits} -> writeCapacityUnits) (\s@ProvisionedThroughputDescription' {} a -> s {writeCapacityUnits = a} :: ProvisionedThroughputDescription)

-- | The date and time of the last provisioned throughput increase for this
-- table.
provisionedThroughputDescription_lastIncreaseDateTime :: Lens.Lens' ProvisionedThroughputDescription (Prelude.Maybe Prelude.UTCTime)
provisionedThroughputDescription_lastIncreaseDateTime = Lens.lens (\ProvisionedThroughputDescription' {lastIncreaseDateTime} -> lastIncreaseDateTime) (\s@ProvisionedThroughputDescription' {} a -> s {lastIncreaseDateTime = a} :: ProvisionedThroughputDescription) Prelude.. Lens.mapping Data._Time

-- | The date and time of the last provisioned throughput decrease for this
-- table.
provisionedThroughputDescription_lastDecreaseDateTime :: Lens.Lens' ProvisionedThroughputDescription (Prelude.Maybe Prelude.UTCTime)
provisionedThroughputDescription_lastDecreaseDateTime = Lens.lens (\ProvisionedThroughputDescription' {lastDecreaseDateTime} -> lastDecreaseDateTime) (\s@ProvisionedThroughputDescription' {} a -> s {lastDecreaseDateTime = a} :: ProvisionedThroughputDescription) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    ProvisionedThroughputDescription
  where
  parseJSON =
    Data.withObject
      "ProvisionedThroughputDescription"
      ( \x ->
          ProvisionedThroughputDescription'
            Prelude.<$> (x Data..:? "ReadCapacityUnits")
            Prelude.<*> (x Data..:? "NumberOfDecreasesToday")
            Prelude.<*> (x Data..:? "WriteCapacityUnits")
            Prelude.<*> (x Data..:? "LastIncreaseDateTime")
            Prelude.<*> (x Data..:? "LastDecreaseDateTime")
      )

instance
  Prelude.Hashable
    ProvisionedThroughputDescription
  where
  hashWithSalt
    _salt
    ProvisionedThroughputDescription' {..} =
      _salt `Prelude.hashWithSalt` readCapacityUnits
        `Prelude.hashWithSalt` numberOfDecreasesToday
        `Prelude.hashWithSalt` writeCapacityUnits
        `Prelude.hashWithSalt` lastIncreaseDateTime
        `Prelude.hashWithSalt` lastDecreaseDateTime

instance
  Prelude.NFData
    ProvisionedThroughputDescription
  where
  rnf ProvisionedThroughputDescription' {..} =
    Prelude.rnf readCapacityUnits
      `Prelude.seq` Prelude.rnf numberOfDecreasesToday
      `Prelude.seq` Prelude.rnf writeCapacityUnits
      `Prelude.seq` Prelude.rnf lastIncreaseDateTime
      `Prelude.seq` Prelude.rnf lastDecreaseDateTime
