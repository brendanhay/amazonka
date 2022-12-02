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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the provisioned throughput for the table or for a
-- global secondary index.
--
-- /See:/ 'newAwsDynamoDbTableProvisionedThroughput' smart constructor.
data AwsDynamoDbTableProvisionedThroughput = AwsDynamoDbTableProvisionedThroughput'
  { -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@.
    readCapacityUnits :: Prelude.Maybe Prelude.Int,
    -- | The number of times during the current UTC calendar day that the
    -- provisioned throughput was decreased.
    numberOfDecreasesToday :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException@.
    writeCapacityUnits :: Prelude.Maybe Prelude.Int,
    -- | Indicates when the provisioned throughput was last increased.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastIncreaseDateTime :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the provisioned throughput was last decreased.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastDecreaseDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableProvisionedThroughput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readCapacityUnits', 'awsDynamoDbTableProvisionedThroughput_readCapacityUnits' - The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@.
--
-- 'numberOfDecreasesToday', 'awsDynamoDbTableProvisionedThroughput_numberOfDecreasesToday' - The number of times during the current UTC calendar day that the
-- provisioned throughput was decreased.
--
-- 'writeCapacityUnits', 'awsDynamoDbTableProvisionedThroughput_writeCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@.
--
-- 'lastIncreaseDateTime', 'awsDynamoDbTableProvisionedThroughput_lastIncreaseDateTime' - Indicates when the provisioned throughput was last increased.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'lastDecreaseDateTime', 'awsDynamoDbTableProvisionedThroughput_lastDecreaseDateTime' - Indicates when the provisioned throughput was last decreased.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newAwsDynamoDbTableProvisionedThroughput ::
  AwsDynamoDbTableProvisionedThroughput
newAwsDynamoDbTableProvisionedThroughput =
  AwsDynamoDbTableProvisionedThroughput'
    { readCapacityUnits =
        Prelude.Nothing,
      numberOfDecreasesToday =
        Prelude.Nothing,
      writeCapacityUnits = Prelude.Nothing,
      lastIncreaseDateTime =
        Prelude.Nothing,
      lastDecreaseDateTime =
        Prelude.Nothing
    }

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@.
awsDynamoDbTableProvisionedThroughput_readCapacityUnits :: Lens.Lens' AwsDynamoDbTableProvisionedThroughput (Prelude.Maybe Prelude.Int)
awsDynamoDbTableProvisionedThroughput_readCapacityUnits = Lens.lens (\AwsDynamoDbTableProvisionedThroughput' {readCapacityUnits} -> readCapacityUnits) (\s@AwsDynamoDbTableProvisionedThroughput' {} a -> s {readCapacityUnits = a} :: AwsDynamoDbTableProvisionedThroughput)

-- | The number of times during the current UTC calendar day that the
-- provisioned throughput was decreased.
awsDynamoDbTableProvisionedThroughput_numberOfDecreasesToday :: Lens.Lens' AwsDynamoDbTableProvisionedThroughput (Prelude.Maybe Prelude.Int)
awsDynamoDbTableProvisionedThroughput_numberOfDecreasesToday = Lens.lens (\AwsDynamoDbTableProvisionedThroughput' {numberOfDecreasesToday} -> numberOfDecreasesToday) (\s@AwsDynamoDbTableProvisionedThroughput' {} a -> s {numberOfDecreasesToday = a} :: AwsDynamoDbTableProvisionedThroughput)

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@.
awsDynamoDbTableProvisionedThroughput_writeCapacityUnits :: Lens.Lens' AwsDynamoDbTableProvisionedThroughput (Prelude.Maybe Prelude.Int)
awsDynamoDbTableProvisionedThroughput_writeCapacityUnits = Lens.lens (\AwsDynamoDbTableProvisionedThroughput' {writeCapacityUnits} -> writeCapacityUnits) (\s@AwsDynamoDbTableProvisionedThroughput' {} a -> s {writeCapacityUnits = a} :: AwsDynamoDbTableProvisionedThroughput)

-- | Indicates when the provisioned throughput was last increased.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsDynamoDbTableProvisionedThroughput_lastIncreaseDateTime :: Lens.Lens' AwsDynamoDbTableProvisionedThroughput (Prelude.Maybe Prelude.Text)
awsDynamoDbTableProvisionedThroughput_lastIncreaseDateTime = Lens.lens (\AwsDynamoDbTableProvisionedThroughput' {lastIncreaseDateTime} -> lastIncreaseDateTime) (\s@AwsDynamoDbTableProvisionedThroughput' {} a -> s {lastIncreaseDateTime = a} :: AwsDynamoDbTableProvisionedThroughput)

-- | Indicates when the provisioned throughput was last decreased.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsDynamoDbTableProvisionedThroughput_lastDecreaseDateTime :: Lens.Lens' AwsDynamoDbTableProvisionedThroughput (Prelude.Maybe Prelude.Text)
awsDynamoDbTableProvisionedThroughput_lastDecreaseDateTime = Lens.lens (\AwsDynamoDbTableProvisionedThroughput' {lastDecreaseDateTime} -> lastDecreaseDateTime) (\s@AwsDynamoDbTableProvisionedThroughput' {} a -> s {lastDecreaseDateTime = a} :: AwsDynamoDbTableProvisionedThroughput)

instance
  Data.FromJSON
    AwsDynamoDbTableProvisionedThroughput
  where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableProvisionedThroughput"
      ( \x ->
          AwsDynamoDbTableProvisionedThroughput'
            Prelude.<$> (x Data..:? "ReadCapacityUnits")
            Prelude.<*> (x Data..:? "NumberOfDecreasesToday")
            Prelude.<*> (x Data..:? "WriteCapacityUnits")
            Prelude.<*> (x Data..:? "LastIncreaseDateTime")
            Prelude.<*> (x Data..:? "LastDecreaseDateTime")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableProvisionedThroughput
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableProvisionedThroughput' {..} =
      _salt `Prelude.hashWithSalt` readCapacityUnits
        `Prelude.hashWithSalt` numberOfDecreasesToday
        `Prelude.hashWithSalt` writeCapacityUnits
        `Prelude.hashWithSalt` lastIncreaseDateTime
        `Prelude.hashWithSalt` lastDecreaseDateTime

instance
  Prelude.NFData
    AwsDynamoDbTableProvisionedThroughput
  where
  rnf AwsDynamoDbTableProvisionedThroughput' {..} =
    Prelude.rnf readCapacityUnits
      `Prelude.seq` Prelude.rnf numberOfDecreasesToday
      `Prelude.seq` Prelude.rnf writeCapacityUnits
      `Prelude.seq` Prelude.rnf lastIncreaseDateTime
      `Prelude.seq` Prelude.rnf lastDecreaseDateTime

instance
  Data.ToJSON
    AwsDynamoDbTableProvisionedThroughput
  where
  toJSON AwsDynamoDbTableProvisionedThroughput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReadCapacityUnits" Data..=)
              Prelude.<$> readCapacityUnits,
            ("NumberOfDecreasesToday" Data..=)
              Prelude.<$> numberOfDecreasesToday,
            ("WriteCapacityUnits" Data..=)
              Prelude.<$> writeCapacityUnits,
            ("LastIncreaseDateTime" Data..=)
              Prelude.<$> lastIncreaseDateTime,
            ("LastDecreaseDateTime" Data..=)
              Prelude.<$> lastDecreaseDateTime
          ]
      )
