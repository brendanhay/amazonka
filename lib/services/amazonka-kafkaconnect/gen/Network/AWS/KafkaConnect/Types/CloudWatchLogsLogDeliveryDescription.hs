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
-- Module      : Network.AWS.KafkaConnect.Types.CloudWatchLogsLogDeliveryDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KafkaConnect.Types.CloudWatchLogsLogDeliveryDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  Core.FromJSON
    CloudWatchLogsLogDeliveryDescription
  where
  parseJSON =
    Core.withObject
      "CloudWatchLogsLogDeliveryDescription"
      ( \x ->
          CloudWatchLogsLogDeliveryDescription'
            Prelude.<$> (x Core..:? "enabled")
            Prelude.<*> (x Core..:? "logGroup")
      )

instance
  Prelude.Hashable
    CloudWatchLogsLogDeliveryDescription

instance
  Prelude.NFData
    CloudWatchLogsLogDeliveryDescription
