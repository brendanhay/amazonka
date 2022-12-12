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
-- Module      : Amazonka.Firehose.Types.ElasticsearchBufferingHints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ElasticsearchBufferingHints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the buffering to perform before delivering data to the Amazon
-- ES destination.
--
-- /See:/ 'newElasticsearchBufferingHints' smart constructor.
data ElasticsearchBufferingHints = ElasticsearchBufferingHints'
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
-- Create a value of 'ElasticsearchBufferingHints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intervalInSeconds', 'elasticsearchBufferingHints_intervalInSeconds' - Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300 (5
-- minutes).
--
-- 'sizeInMBs', 'elasticsearchBufferingHints_sizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it
-- to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount
-- of data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MB\/sec, the value should be
-- 10 MB or higher.
newElasticsearchBufferingHints ::
  ElasticsearchBufferingHints
newElasticsearchBufferingHints =
  ElasticsearchBufferingHints'
    { intervalInSeconds =
        Prelude.Nothing,
      sizeInMBs = Prelude.Nothing
    }

-- | Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300 (5
-- minutes).
elasticsearchBufferingHints_intervalInSeconds :: Lens.Lens' ElasticsearchBufferingHints (Prelude.Maybe Prelude.Natural)
elasticsearchBufferingHints_intervalInSeconds = Lens.lens (\ElasticsearchBufferingHints' {intervalInSeconds} -> intervalInSeconds) (\s@ElasticsearchBufferingHints' {} a -> s {intervalInSeconds = a} :: ElasticsearchBufferingHints)

-- | Buffer incoming data to the specified size, in MBs, before delivering it
-- to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount
-- of data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MB\/sec, the value should be
-- 10 MB or higher.
elasticsearchBufferingHints_sizeInMBs :: Lens.Lens' ElasticsearchBufferingHints (Prelude.Maybe Prelude.Natural)
elasticsearchBufferingHints_sizeInMBs = Lens.lens (\ElasticsearchBufferingHints' {sizeInMBs} -> sizeInMBs) (\s@ElasticsearchBufferingHints' {} a -> s {sizeInMBs = a} :: ElasticsearchBufferingHints)

instance Data.FromJSON ElasticsearchBufferingHints where
  parseJSON =
    Data.withObject
      "ElasticsearchBufferingHints"
      ( \x ->
          ElasticsearchBufferingHints'
            Prelude.<$> (x Data..:? "IntervalInSeconds")
            Prelude.<*> (x Data..:? "SizeInMBs")
      )

instance Prelude.Hashable ElasticsearchBufferingHints where
  hashWithSalt _salt ElasticsearchBufferingHints' {..} =
    _salt `Prelude.hashWithSalt` intervalInSeconds
      `Prelude.hashWithSalt` sizeInMBs

instance Prelude.NFData ElasticsearchBufferingHints where
  rnf ElasticsearchBufferingHints' {..} =
    Prelude.rnf intervalInSeconds
      `Prelude.seq` Prelude.rnf sizeInMBs

instance Data.ToJSON ElasticsearchBufferingHints where
  toJSON ElasticsearchBufferingHints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IntervalInSeconds" Data..=)
              Prelude.<$> intervalInSeconds,
            ("SizeInMBs" Data..=) Prelude.<$> sizeInMBs
          ]
      )
