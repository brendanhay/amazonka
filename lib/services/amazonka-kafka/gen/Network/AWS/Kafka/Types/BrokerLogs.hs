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
-- Module      : Network.AWS.Kafka.Types.BrokerLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kafka.Types.BrokerLogs where

import qualified Network.AWS.Core as Core
import Network.AWS.Kafka.Types.CloudWatchLogs
import Network.AWS.Kafka.Types.Firehose
import Network.AWS.Kafka.Types.S3
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON BrokerLogs where
  parseJSON =
    Core.withObject
      "BrokerLogs"
      ( \x ->
          BrokerLogs'
            Prelude.<$> (x Core..:? "cloudWatchLogs")
            Prelude.<*> (x Core..:? "firehose")
            Prelude.<*> (x Core..:? "s3")
      )

instance Prelude.Hashable BrokerLogs

instance Prelude.NFData BrokerLogs

instance Core.ToJSON BrokerLogs where
  toJSON BrokerLogs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogs" Core..=)
              Prelude.<$> cloudWatchLogs,
            ("firehose" Core..=) Prelude.<$> firehose,
            ("s3" Core..=) Prelude.<$> s3
          ]
      )
