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
-- Module      : Amazonka.KafkaConnect.Types.CloudWatchLogsLogDeliveryDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CloudWatchLogsLogDeliveryDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A description of the log delivery settings.
--
-- /See:/ 'newCloudWatchLogsLogDeliveryDescription' smart constructor.
data CloudWatchLogsLogDeliveryDescription = CloudWatchLogsLogDeliveryDescription'
  { -- | Whether log delivery to Amazon CloudWatch Logs is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the CloudWatch log group that is the destination for log
    -- delivery.
    logGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsLogDeliveryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'cloudWatchLogsLogDeliveryDescription_enabled' - Whether log delivery to Amazon CloudWatch Logs is enabled.
--
-- 'logGroup', 'cloudWatchLogsLogDeliveryDescription_logGroup' - The name of the CloudWatch log group that is the destination for log
-- delivery.
newCloudWatchLogsLogDeliveryDescription ::
  CloudWatchLogsLogDeliveryDescription
newCloudWatchLogsLogDeliveryDescription =
  CloudWatchLogsLogDeliveryDescription'
    { enabled =
        Prelude.Nothing,
      logGroup = Prelude.Nothing
    }

-- | Whether log delivery to Amazon CloudWatch Logs is enabled.
cloudWatchLogsLogDeliveryDescription_enabled :: Lens.Lens' CloudWatchLogsLogDeliveryDescription (Prelude.Maybe Prelude.Bool)
cloudWatchLogsLogDeliveryDescription_enabled = Lens.lens (\CloudWatchLogsLogDeliveryDescription' {enabled} -> enabled) (\s@CloudWatchLogsLogDeliveryDescription' {} a -> s {enabled = a} :: CloudWatchLogsLogDeliveryDescription)

-- | The name of the CloudWatch log group that is the destination for log
-- delivery.
cloudWatchLogsLogDeliveryDescription_logGroup :: Lens.Lens' CloudWatchLogsLogDeliveryDescription (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogDeliveryDescription_logGroup = Lens.lens (\CloudWatchLogsLogDeliveryDescription' {logGroup} -> logGroup) (\s@CloudWatchLogsLogDeliveryDescription' {} a -> s {logGroup = a} :: CloudWatchLogsLogDeliveryDescription)

instance
  Data.FromJSON
    CloudWatchLogsLogDeliveryDescription
  where
  parseJSON =
    Data.withObject
      "CloudWatchLogsLogDeliveryDescription"
      ( \x ->
          CloudWatchLogsLogDeliveryDescription'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "logGroup")
      )

instance
  Prelude.Hashable
    CloudWatchLogsLogDeliveryDescription
  where
  hashWithSalt
    _salt
    CloudWatchLogsLogDeliveryDescription' {..} =
      _salt `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` logGroup

instance
  Prelude.NFData
    CloudWatchLogsLogDeliveryDescription
  where
  rnf CloudWatchLogsLogDeliveryDescription' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf logGroup
