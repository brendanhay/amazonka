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
-- Module      : Amazonka.Kafka.Types.BrokerLogs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.BrokerLogs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.CloudWatchLogs
import Amazonka.Kafka.Types.Firehose
import Amazonka.Kafka.Types.S3
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newBrokerLogs' smart constructor.
data BrokerLogs = BrokerLogs'
  { cloudWatchLogs :: Prelude.Maybe CloudWatchLogs,
    firehose :: Prelude.Maybe Firehose,
    s3 :: Prelude.Maybe S3
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'brokerLogs_cloudWatchLogs' - Undocumented member.
--
-- 'firehose', 'brokerLogs_firehose' - Undocumented member.
--
-- 's3', 'brokerLogs_s3' - Undocumented member.
newBrokerLogs ::
  BrokerLogs
newBrokerLogs =
  BrokerLogs'
    { cloudWatchLogs = Prelude.Nothing,
      firehose = Prelude.Nothing,
      s3 = Prelude.Nothing
    }

-- | Undocumented member.
brokerLogs_cloudWatchLogs :: Lens.Lens' BrokerLogs (Prelude.Maybe CloudWatchLogs)
brokerLogs_cloudWatchLogs = Lens.lens (\BrokerLogs' {cloudWatchLogs} -> cloudWatchLogs) (\s@BrokerLogs' {} a -> s {cloudWatchLogs = a} :: BrokerLogs)

-- | Undocumented member.
brokerLogs_firehose :: Lens.Lens' BrokerLogs (Prelude.Maybe Firehose)
brokerLogs_firehose = Lens.lens (\BrokerLogs' {firehose} -> firehose) (\s@BrokerLogs' {} a -> s {firehose = a} :: BrokerLogs)

-- | Undocumented member.
brokerLogs_s3 :: Lens.Lens' BrokerLogs (Prelude.Maybe S3)
brokerLogs_s3 = Lens.lens (\BrokerLogs' {s3} -> s3) (\s@BrokerLogs' {} a -> s {s3 = a} :: BrokerLogs)

instance Data.FromJSON BrokerLogs where
  parseJSON =
    Data.withObject
      "BrokerLogs"
      ( \x ->
          BrokerLogs'
            Prelude.<$> (x Data..:? "cloudWatchLogs")
            Prelude.<*> (x Data..:? "firehose")
            Prelude.<*> (x Data..:? "s3")
      )

instance Prelude.Hashable BrokerLogs where
  hashWithSalt _salt BrokerLogs' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` firehose
      `Prelude.hashWithSalt` s3

instance Prelude.NFData BrokerLogs where
  rnf BrokerLogs' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf firehose
      `Prelude.seq` Prelude.rnf s3

instance Data.ToJSON BrokerLogs where
  toJSON BrokerLogs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogs" Data..=)
              Prelude.<$> cloudWatchLogs,
            ("firehose" Data..=) Prelude.<$> firehose,
            ("s3" Data..=) Prelude.<$> s3
          ]
      )
