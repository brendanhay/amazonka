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
-- Module      : Amazonka.Firehose.Types.BufferingHints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.BufferingHints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes hints for the buffering to perform before delivering data to
-- the destination. These options are treated as hints, and therefore
-- Kinesis Data Firehose might choose to use different values when it is
-- optimal. The @SizeInMBs@ and @IntervalInSeconds@ parameters are
-- optional. However, if specify a value for one of them, you must also
-- provide a value for the other.
--
-- /See:/ 'newBufferingHints' smart constructor.
data BufferingHints = BufferingHints'
  { -- | Buffer incoming data to the specified size, in MiBs, before delivering
    -- it to the destination. The default value is 5. This parameter is
    -- optional but if you specify a value for it, you must also specify a
    -- value for @IntervalInSeconds@, and vice versa.
    --
    -- We recommend setting this parameter to a value greater than the amount
    -- of data you typically ingest into the delivery stream in 10 seconds. For
    -- example, if you typically ingest data at 1 MiB\/sec, the value should be
    -- 10 MiB or higher.
    sizeInMBs :: Prelude.Maybe Prelude.Natural,
    -- | Buffer incoming data for the specified period of time, in seconds,
    -- before delivering it to the destination. The default value is 300. This
    -- parameter is optional but if you specify a value for it, you must also
    -- specify a value for @SizeInMBs@, and vice versa.
    intervalInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BufferingHints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInMBs', 'bufferingHints_sizeInMBs' - Buffer incoming data to the specified size, in MiBs, before delivering
-- it to the destination. The default value is 5. This parameter is
-- optional but if you specify a value for it, you must also specify a
-- value for @IntervalInSeconds@, and vice versa.
--
-- We recommend setting this parameter to a value greater than the amount
-- of data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MiB\/sec, the value should be
-- 10 MiB or higher.
--
-- 'intervalInSeconds', 'bufferingHints_intervalInSeconds' - Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300. This
-- parameter is optional but if you specify a value for it, you must also
-- specify a value for @SizeInMBs@, and vice versa.
newBufferingHints ::
  BufferingHints
newBufferingHints =
  BufferingHints'
    { sizeInMBs = Prelude.Nothing,
      intervalInSeconds = Prelude.Nothing
    }

-- | Buffer incoming data to the specified size, in MiBs, before delivering
-- it to the destination. The default value is 5. This parameter is
-- optional but if you specify a value for it, you must also specify a
-- value for @IntervalInSeconds@, and vice versa.
--
-- We recommend setting this parameter to a value greater than the amount
-- of data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MiB\/sec, the value should be
-- 10 MiB or higher.
bufferingHints_sizeInMBs :: Lens.Lens' BufferingHints (Prelude.Maybe Prelude.Natural)
bufferingHints_sizeInMBs = Lens.lens (\BufferingHints' {sizeInMBs} -> sizeInMBs) (\s@BufferingHints' {} a -> s {sizeInMBs = a} :: BufferingHints)

-- | Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300. This
-- parameter is optional but if you specify a value for it, you must also
-- specify a value for @SizeInMBs@, and vice versa.
bufferingHints_intervalInSeconds :: Lens.Lens' BufferingHints (Prelude.Maybe Prelude.Natural)
bufferingHints_intervalInSeconds = Lens.lens (\BufferingHints' {intervalInSeconds} -> intervalInSeconds) (\s@BufferingHints' {} a -> s {intervalInSeconds = a} :: BufferingHints)

instance Core.FromJSON BufferingHints where
  parseJSON =
    Core.withObject
      "BufferingHints"
      ( \x ->
          BufferingHints'
            Prelude.<$> (x Core..:? "SizeInMBs")
            Prelude.<*> (x Core..:? "IntervalInSeconds")
      )

instance Prelude.Hashable BufferingHints where
  hashWithSalt _salt BufferingHints' {..} =
    _salt `Prelude.hashWithSalt` sizeInMBs
      `Prelude.hashWithSalt` intervalInSeconds

instance Prelude.NFData BufferingHints where
  rnf BufferingHints' {..} =
    Prelude.rnf sizeInMBs
      `Prelude.seq` Prelude.rnf intervalInSeconds

instance Core.ToJSON BufferingHints where
  toJSON BufferingHints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SizeInMBs" Core..=) Prelude.<$> sizeInMBs,
            ("IntervalInSeconds" Core..=)
              Prelude.<$> intervalInSeconds
          ]
      )
