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
-- Module      : Amazonka.KeySpaces.Types.CapacitySpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.CapacitySpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.ThroughputMode
import qualified Amazonka.Prelude as Prelude

-- | Amazon Keyspaces has two read\/write capacity modes for processing reads
-- and writes on your tables:
--
-- • On-demand (default)
--
-- • Provisioned
--
-- The read\/write capacity mode that you choose controls how you are
-- charged for read and write throughput and how table throughput capacity
-- is managed.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- /See:/ 'newCapacitySpecification' smart constructor.
data CapacitySpecification = CapacitySpecification'
  { -- | The throughput capacity specified for @read@ operations defined in
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
-- Create a value of 'CapacitySpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readCapacityUnits', 'capacitySpecification_readCapacityUnits' - The throughput capacity specified for @read@ operations defined in
-- @read capacity units@ @(RCUs)@.
--
-- 'writeCapacityUnits', 'capacitySpecification_writeCapacityUnits' - The throughput capacity specified for @write@ operations defined in
-- @write capacity units@ @(WCUs)@.
--
-- 'throughputMode', 'capacitySpecification_throughputMode' - The read\/write throughput capacity mode for a table. The options are:
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
newCapacitySpecification ::
  -- | 'throughputMode'
  ThroughputMode ->
  CapacitySpecification
newCapacitySpecification pThroughputMode_ =
  CapacitySpecification'
    { readCapacityUnits =
        Prelude.Nothing,
      writeCapacityUnits = Prelude.Nothing,
      throughputMode = pThroughputMode_
    }

-- | The throughput capacity specified for @read@ operations defined in
-- @read capacity units@ @(RCUs)@.
capacitySpecification_readCapacityUnits :: Lens.Lens' CapacitySpecification (Prelude.Maybe Prelude.Natural)
capacitySpecification_readCapacityUnits = Lens.lens (\CapacitySpecification' {readCapacityUnits} -> readCapacityUnits) (\s@CapacitySpecification' {} a -> s {readCapacityUnits = a} :: CapacitySpecification)

-- | The throughput capacity specified for @write@ operations defined in
-- @write capacity units@ @(WCUs)@.
capacitySpecification_writeCapacityUnits :: Lens.Lens' CapacitySpecification (Prelude.Maybe Prelude.Natural)
capacitySpecification_writeCapacityUnits = Lens.lens (\CapacitySpecification' {writeCapacityUnits} -> writeCapacityUnits) (\s@CapacitySpecification' {} a -> s {writeCapacityUnits = a} :: CapacitySpecification)

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
capacitySpecification_throughputMode :: Lens.Lens' CapacitySpecification ThroughputMode
capacitySpecification_throughputMode = Lens.lens (\CapacitySpecification' {throughputMode} -> throughputMode) (\s@CapacitySpecification' {} a -> s {throughputMode = a} :: CapacitySpecification)

instance Prelude.Hashable CapacitySpecification where
  hashWithSalt _salt CapacitySpecification' {..} =
    _salt `Prelude.hashWithSalt` readCapacityUnits
      `Prelude.hashWithSalt` writeCapacityUnits
      `Prelude.hashWithSalt` throughputMode

instance Prelude.NFData CapacitySpecification where
  rnf CapacitySpecification' {..} =
    Prelude.rnf readCapacityUnits
      `Prelude.seq` Prelude.rnf writeCapacityUnits
      `Prelude.seq` Prelude.rnf throughputMode

instance Data.ToJSON CapacitySpecification where
  toJSON CapacitySpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("readCapacityUnits" Data..=)
              Prelude.<$> readCapacityUnits,
            ("writeCapacityUnits" Data..=)
              Prelude.<$> writeCapacityUnits,
            Prelude.Just
              ("throughputMode" Data..= throughputMode)
          ]
      )
