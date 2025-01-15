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
-- Module      : Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the buffering to perform before delivering data to the Amazon
-- OpenSearch Service destination.
--
-- /See:/ 'newAmazonopensearchserviceBufferingHints' smart constructor.
data AmazonopensearchserviceBufferingHints = AmazonopensearchserviceBufferingHints'
  { -- | Buffer incoming data for the specified period of time, in seconds,
    -- before delivering it to the destination. The default value is 300 (5
    -- minutes).
    intervalInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Buffer incoming data to the specified size, in MBs, before delivering it
    -- to the destination. The default value is 5.
    --
    -- We recommend setting this parameter to a value greater than the amount
    -- of data you typically ingest into the delivery stream in 10 seconds. For
    -- example, if you typically ingest data at 1 MB\/sec, the value should be
    -- 10 MB or higher.
    sizeInMBs :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonopensearchserviceBufferingHints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intervalInSeconds', 'amazonopensearchserviceBufferingHints_intervalInSeconds' - Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300 (5
-- minutes).
--
-- 'sizeInMBs', 'amazonopensearchserviceBufferingHints_sizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it
-- to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount
-- of data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MB\/sec, the value should be
-- 10 MB or higher.
newAmazonopensearchserviceBufferingHints ::
  AmazonopensearchserviceBufferingHints
newAmazonopensearchserviceBufferingHints =
  AmazonopensearchserviceBufferingHints'
    { intervalInSeconds =
        Prelude.Nothing,
      sizeInMBs = Prelude.Nothing
    }

-- | Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300 (5
-- minutes).
amazonopensearchserviceBufferingHints_intervalInSeconds :: Lens.Lens' AmazonopensearchserviceBufferingHints (Prelude.Maybe Prelude.Natural)
amazonopensearchserviceBufferingHints_intervalInSeconds = Lens.lens (\AmazonopensearchserviceBufferingHints' {intervalInSeconds} -> intervalInSeconds) (\s@AmazonopensearchserviceBufferingHints' {} a -> s {intervalInSeconds = a} :: AmazonopensearchserviceBufferingHints)

-- | Buffer incoming data to the specified size, in MBs, before delivering it
-- to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount
-- of data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MB\/sec, the value should be
-- 10 MB or higher.
amazonopensearchserviceBufferingHints_sizeInMBs :: Lens.Lens' AmazonopensearchserviceBufferingHints (Prelude.Maybe Prelude.Natural)
amazonopensearchserviceBufferingHints_sizeInMBs = Lens.lens (\AmazonopensearchserviceBufferingHints' {sizeInMBs} -> sizeInMBs) (\s@AmazonopensearchserviceBufferingHints' {} a -> s {sizeInMBs = a} :: AmazonopensearchserviceBufferingHints)

instance
  Data.FromJSON
    AmazonopensearchserviceBufferingHints
  where
  parseJSON =
    Data.withObject
      "AmazonopensearchserviceBufferingHints"
      ( \x ->
          AmazonopensearchserviceBufferingHints'
            Prelude.<$> (x Data..:? "IntervalInSeconds")
            Prelude.<*> (x Data..:? "SizeInMBs")
      )

instance
  Prelude.Hashable
    AmazonopensearchserviceBufferingHints
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceBufferingHints' {..} =
      _salt
        `Prelude.hashWithSalt` intervalInSeconds
        `Prelude.hashWithSalt` sizeInMBs

instance
  Prelude.NFData
    AmazonopensearchserviceBufferingHints
  where
  rnf AmazonopensearchserviceBufferingHints' {..} =
    Prelude.rnf intervalInSeconds `Prelude.seq`
      Prelude.rnf sizeInMBs

instance
  Data.ToJSON
    AmazonopensearchserviceBufferingHints
  where
  toJSON AmazonopensearchserviceBufferingHints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IntervalInSeconds" Data..=)
              Prelude.<$> intervalInSeconds,
            ("SizeInMBs" Data..=) Prelude.<$> sizeInMBs
          ]
      )
