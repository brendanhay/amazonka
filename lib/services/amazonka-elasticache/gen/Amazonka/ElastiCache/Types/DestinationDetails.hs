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
-- Module      : Amazonka.ElastiCache.Types.DestinationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.DestinationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.CloudWatchLogsDestinationDetails
import Amazonka.ElastiCache.Types.KinesisFirehoseDestinationDetails
import qualified Amazonka.Prelude as Prelude

-- | Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
--
-- /See:/ 'newDestinationDetails' smart constructor.
data DestinationDetails = DestinationDetails'
  { -- | The configuration details of the Kinesis Data Firehose destination.
    kinesisFirehoseDetails :: Prelude.Maybe KinesisFirehoseDestinationDetails,
    -- | The configuration details of the CloudWatch Logs destination.
    cloudWatchLogsDetails :: Prelude.Maybe CloudWatchLogsDestinationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisFirehoseDetails', 'destinationDetails_kinesisFirehoseDetails' - The configuration details of the Kinesis Data Firehose destination.
--
-- 'cloudWatchLogsDetails', 'destinationDetails_cloudWatchLogsDetails' - The configuration details of the CloudWatch Logs destination.
newDestinationDetails ::
  DestinationDetails
newDestinationDetails =
  DestinationDetails'
    { kinesisFirehoseDetails =
        Prelude.Nothing,
      cloudWatchLogsDetails = Prelude.Nothing
    }

-- | The configuration details of the Kinesis Data Firehose destination.
destinationDetails_kinesisFirehoseDetails :: Lens.Lens' DestinationDetails (Prelude.Maybe KinesisFirehoseDestinationDetails)
destinationDetails_kinesisFirehoseDetails = Lens.lens (\DestinationDetails' {kinesisFirehoseDetails} -> kinesisFirehoseDetails) (\s@DestinationDetails' {} a -> s {kinesisFirehoseDetails = a} :: DestinationDetails)

-- | The configuration details of the CloudWatch Logs destination.
destinationDetails_cloudWatchLogsDetails :: Lens.Lens' DestinationDetails (Prelude.Maybe CloudWatchLogsDestinationDetails)
destinationDetails_cloudWatchLogsDetails = Lens.lens (\DestinationDetails' {cloudWatchLogsDetails} -> cloudWatchLogsDetails) (\s@DestinationDetails' {} a -> s {cloudWatchLogsDetails = a} :: DestinationDetails)

instance Data.FromXML DestinationDetails where
  parseXML x =
    DestinationDetails'
      Prelude.<$> (x Data..@? "KinesisFirehoseDetails")
      Prelude.<*> (x Data..@? "CloudWatchLogsDetails")

instance Prelude.Hashable DestinationDetails where
  hashWithSalt _salt DestinationDetails' {..} =
    _salt `Prelude.hashWithSalt` kinesisFirehoseDetails
      `Prelude.hashWithSalt` cloudWatchLogsDetails

instance Prelude.NFData DestinationDetails where
  rnf DestinationDetails' {..} =
    Prelude.rnf kinesisFirehoseDetails
      `Prelude.seq` Prelude.rnf cloudWatchLogsDetails

instance Data.ToQuery DestinationDetails where
  toQuery DestinationDetails' {..} =
    Prelude.mconcat
      [ "KinesisFirehoseDetails"
          Data.=: kinesisFirehoseDetails,
        "CloudWatchLogsDetails"
          Data.=: cloudWatchLogsDetails
      ]
