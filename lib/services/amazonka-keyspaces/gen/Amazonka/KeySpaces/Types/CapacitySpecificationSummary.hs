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
-- Module      : Amazonka.KeySpaces.Types.CapacitySpecificationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.CapacitySpecificationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.ThroughputMode
import qualified Amazonka.Prelude as Prelude

-- | The read\/write throughput capacity mode for a table. The options are:
--
-- • @throughputMode:PAY_PER_REQUEST@ and
--
-- • @throughputMode:PROVISIONED@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- /See:/ 'newCapacitySpecificationSummary' smart constructor.
data CapacitySpecificationSummary = CapacitySpecificationSummary'
  { -- | The timestamp of the last operation that changed the provisioned
    -- throughput capacity of a table.
    lastUpdateToPayPerRequestTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The throughput capacity specified for @read@ operations defined in
    -- @read capacity units@ @(RCUs)@.
    readCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The throughput capacity specified for @write@ operations defined in
    -- @write capacity units@ @(WCUs)@.
    writeCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The read\/write throughput capacity mode for a table. The options are:
    --
    -- • @throughputMode:PAY_PER_REQUEST@ and
    --
    -- • @throughputMode:PROVISIONED@ - Provisioned capacity mode requires
    -- @readCapacityUnits@ and @writeCapacityUnits@ as input.
    --
    -- The default is @throughput_mode:PAY_PER_REQUEST@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
    -- in the /Amazon Keyspaces Developer Guide/.
    throughputMode :: ThroughputMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacitySpecificationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateToPayPerRequestTimestamp', 'capacitySpecificationSummary_lastUpdateToPayPerRequestTimestamp' - The timestamp of the last operation that changed the provisioned
-- throughput capacity of a table.
--
-- 'readCapacityUnits', 'capacitySpecificationSummary_readCapacityUnits' - The throughput capacity specified for @read@ operations defined in
-- @read capacity units@ @(RCUs)@.
--
-- 'writeCapacityUnits', 'capacitySpecificationSummary_writeCapacityUnits' - The throughput capacity specified for @write@ operations defined in
-- @write capacity units@ @(WCUs)@.
--
-- 'throughputMode', 'capacitySpecificationSummary_throughputMode' - The read\/write throughput capacity mode for a table. The options are:
--
-- • @throughputMode:PAY_PER_REQUEST@ and
--
-- • @throughputMode:PROVISIONED@ - Provisioned capacity mode requires
-- @readCapacityUnits@ and @writeCapacityUnits@ as input.
--
-- The default is @throughput_mode:PAY_PER_REQUEST@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
-- in the /Amazon Keyspaces Developer Guide/.
newCapacitySpecificationSummary ::
  -- | 'throughputMode'
  ThroughputMode ->
  CapacitySpecificationSummary
newCapacitySpecificationSummary pThroughputMode_ =
  CapacitySpecificationSummary'
    { lastUpdateToPayPerRequestTimestamp =
        Prelude.Nothing,
      readCapacityUnits = Prelude.Nothing,
      writeCapacityUnits = Prelude.Nothing,
      throughputMode = pThroughputMode_
    }

-- | The timestamp of the last operation that changed the provisioned
-- throughput capacity of a table.
capacitySpecificationSummary_lastUpdateToPayPerRequestTimestamp :: Lens.Lens' CapacitySpecificationSummary (Prelude.Maybe Prelude.UTCTime)
capacitySpecificationSummary_lastUpdateToPayPerRequestTimestamp = Lens.lens (\CapacitySpecificationSummary' {lastUpdateToPayPerRequestTimestamp} -> lastUpdateToPayPerRequestTimestamp) (\s@CapacitySpecificationSummary' {} a -> s {lastUpdateToPayPerRequestTimestamp = a} :: CapacitySpecificationSummary) Prelude.. Lens.mapping Data._Time

-- | The throughput capacity specified for @read@ operations defined in
-- @read capacity units@ @(RCUs)@.
capacitySpecificationSummary_readCapacityUnits :: Lens.Lens' CapacitySpecificationSummary (Prelude.Maybe Prelude.Natural)
capacitySpecificationSummary_readCapacityUnits = Lens.lens (\CapacitySpecificationSummary' {readCapacityUnits} -> readCapacityUnits) (\s@CapacitySpecificationSummary' {} a -> s {readCapacityUnits = a} :: CapacitySpecificationSummary)

-- | The throughput capacity specified for @write@ operations defined in
-- @write capacity units@ @(WCUs)@.
capacitySpecificationSummary_writeCapacityUnits :: Lens.Lens' CapacitySpecificationSummary (Prelude.Maybe Prelude.Natural)
capacitySpecificationSummary_writeCapacityUnits = Lens.lens (\CapacitySpecificationSummary' {writeCapacityUnits} -> writeCapacityUnits) (\s@CapacitySpecificationSummary' {} a -> s {writeCapacityUnits = a} :: CapacitySpecificationSummary)

-- | The read\/write throughput capacity mode for a table. The options are:
--
-- • @throughputMode:PAY_PER_REQUEST@ and
--
-- • @throughputMode:PROVISIONED@ - Provisioned capacity mode requires
-- @readCapacityUnits@ and @writeCapacityUnits@ as input.
--
-- The default is @throughput_mode:PAY_PER_REQUEST@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
-- in the /Amazon Keyspaces Developer Guide/.
capacitySpecificationSummary_throughputMode :: Lens.Lens' CapacitySpecificationSummary ThroughputMode
capacitySpecificationSummary_throughputMode = Lens.lens (\CapacitySpecificationSummary' {throughputMode} -> throughputMode) (\s@CapacitySpecificationSummary' {} a -> s {throughputMode = a} :: CapacitySpecificationSummary)

instance Data.FromJSON CapacitySpecificationSummary where
  parseJSON =
    Data.withObject
      "CapacitySpecificationSummary"
      ( \x ->
          CapacitySpecificationSummary'
            Prelude.<$> (x Data..:? "lastUpdateToPayPerRequestTimestamp")
            Prelude.<*> (x Data..:? "readCapacityUnits")
            Prelude.<*> (x Data..:? "writeCapacityUnits")
            Prelude.<*> (x Data..: "throughputMode")
      )

instance
  Prelude.Hashable
    CapacitySpecificationSummary
  where
  hashWithSalt _salt CapacitySpecificationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` lastUpdateToPayPerRequestTimestamp
      `Prelude.hashWithSalt` readCapacityUnits
      `Prelude.hashWithSalt` writeCapacityUnits
      `Prelude.hashWithSalt` throughputMode

instance Prelude.NFData CapacitySpecificationSummary where
  rnf CapacitySpecificationSummary' {..} =
    Prelude.rnf lastUpdateToPayPerRequestTimestamp
      `Prelude.seq` Prelude.rnf readCapacityUnits
      `Prelude.seq` Prelude.rnf writeCapacityUnits
      `Prelude.seq` Prelude.rnf throughputMode
