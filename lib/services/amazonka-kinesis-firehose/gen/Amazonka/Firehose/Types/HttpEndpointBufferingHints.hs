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
-- Module      : Amazonka.Firehose.Types.HttpEndpointBufferingHints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.HttpEndpointBufferingHints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the buffering options that can be applied before data is
-- delivered to the HTTP endpoint destination. Kinesis Data Firehose treats
-- these options as hints, and it might choose to use more optimal values.
-- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
-- However, if specify a value for one of them, you must also provide a
-- value for the other.
--
-- /See:/ 'newHttpEndpointBufferingHints' smart constructor.
data HttpEndpointBufferingHints = HttpEndpointBufferingHints'
  { -- | Buffer incoming data to the specified size, in MBs, before delivering it
    -- to the destination. The default value is 5.
    --
    -- We recommend setting this parameter to a value greater than the amount
    -- of data you typically ingest into the delivery stream in 10 seconds. For
    -- example, if you typically ingest data at 1 MB\/sec, the value should be
    -- 10 MB or higher.
    sizeInMBs :: Prelude.Maybe Prelude.Natural,
    -- | Buffer incoming data for the specified period of time, in seconds,
    -- before delivering it to the destination. The default value is 300 (5
    -- minutes).
    intervalInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointBufferingHints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInMBs', 'httpEndpointBufferingHints_sizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it
-- to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount
-- of data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MB\/sec, the value should be
-- 10 MB or higher.
--
-- 'intervalInSeconds', 'httpEndpointBufferingHints_intervalInSeconds' - Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300 (5
-- minutes).
newHttpEndpointBufferingHints ::
  HttpEndpointBufferingHints
newHttpEndpointBufferingHints =
  HttpEndpointBufferingHints'
    { sizeInMBs =
        Prelude.Nothing,
      intervalInSeconds = Prelude.Nothing
    }

-- | Buffer incoming data to the specified size, in MBs, before delivering it
-- to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount
-- of data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MB\/sec, the value should be
-- 10 MB or higher.
httpEndpointBufferingHints_sizeInMBs :: Lens.Lens' HttpEndpointBufferingHints (Prelude.Maybe Prelude.Natural)
httpEndpointBufferingHints_sizeInMBs = Lens.lens (\HttpEndpointBufferingHints' {sizeInMBs} -> sizeInMBs) (\s@HttpEndpointBufferingHints' {} a -> s {sizeInMBs = a} :: HttpEndpointBufferingHints)

-- | Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300 (5
-- minutes).
httpEndpointBufferingHints_intervalInSeconds :: Lens.Lens' HttpEndpointBufferingHints (Prelude.Maybe Prelude.Natural)
httpEndpointBufferingHints_intervalInSeconds = Lens.lens (\HttpEndpointBufferingHints' {intervalInSeconds} -> intervalInSeconds) (\s@HttpEndpointBufferingHints' {} a -> s {intervalInSeconds = a} :: HttpEndpointBufferingHints)

instance Data.FromJSON HttpEndpointBufferingHints where
  parseJSON =
    Data.withObject
      "HttpEndpointBufferingHints"
      ( \x ->
          HttpEndpointBufferingHints'
            Prelude.<$> (x Data..:? "SizeInMBs")
            Prelude.<*> (x Data..:? "IntervalInSeconds")
      )

instance Prelude.Hashable HttpEndpointBufferingHints where
  hashWithSalt _salt HttpEndpointBufferingHints' {..} =
    _salt `Prelude.hashWithSalt` sizeInMBs
      `Prelude.hashWithSalt` intervalInSeconds

instance Prelude.NFData HttpEndpointBufferingHints where
  rnf HttpEndpointBufferingHints' {..} =
    Prelude.rnf sizeInMBs
      `Prelude.seq` Prelude.rnf intervalInSeconds

instance Data.ToJSON HttpEndpointBufferingHints where
  toJSON HttpEndpointBufferingHints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SizeInMBs" Data..=) Prelude.<$> sizeInMBs,
            ("IntervalInSeconds" Data..=)
              Prelude.<$> intervalInSeconds
          ]
      )
