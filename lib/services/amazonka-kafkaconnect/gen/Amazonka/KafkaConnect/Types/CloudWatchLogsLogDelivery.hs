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
-- Module      : Amazonka.KafkaConnect.Types.CloudWatchLogsLogDelivery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CloudWatchLogsLogDelivery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for delivering connector logs to Amazon CloudWatch Logs.
--
-- /See:/ 'newCloudWatchLogsLogDelivery' smart constructor.
data CloudWatchLogsLogDelivery = CloudWatchLogsLogDelivery'
  { -- | The name of the CloudWatch log group that is the destination for log
    -- delivery.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | Whether log delivery to Amazon CloudWatch Logs is enabled.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsLogDelivery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroup', 'cloudWatchLogsLogDelivery_logGroup' - The name of the CloudWatch log group that is the destination for log
-- delivery.
--
-- 'enabled', 'cloudWatchLogsLogDelivery_enabled' - Whether log delivery to Amazon CloudWatch Logs is enabled.
newCloudWatchLogsLogDelivery ::
  -- | 'enabled'
  Prelude.Bool ->
  CloudWatchLogsLogDelivery
newCloudWatchLogsLogDelivery pEnabled_ =
  CloudWatchLogsLogDelivery'
    { logGroup =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The name of the CloudWatch log group that is the destination for log
-- delivery.
cloudWatchLogsLogDelivery_logGroup :: Lens.Lens' CloudWatchLogsLogDelivery (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogDelivery_logGroup = Lens.lens (\CloudWatchLogsLogDelivery' {logGroup} -> logGroup) (\s@CloudWatchLogsLogDelivery' {} a -> s {logGroup = a} :: CloudWatchLogsLogDelivery)

-- | Whether log delivery to Amazon CloudWatch Logs is enabled.
cloudWatchLogsLogDelivery_enabled :: Lens.Lens' CloudWatchLogsLogDelivery Prelude.Bool
cloudWatchLogsLogDelivery_enabled = Lens.lens (\CloudWatchLogsLogDelivery' {enabled} -> enabled) (\s@CloudWatchLogsLogDelivery' {} a -> s {enabled = a} :: CloudWatchLogsLogDelivery)

instance Prelude.Hashable CloudWatchLogsLogDelivery where
  hashWithSalt _salt CloudWatchLogsLogDelivery' {..} =
    _salt
      `Prelude.hashWithSalt` logGroup
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData CloudWatchLogsLogDelivery where
  rnf CloudWatchLogsLogDelivery' {..} =
    Prelude.rnf logGroup
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON CloudWatchLogsLogDelivery where
  toJSON CloudWatchLogsLogDelivery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logGroup" Data..=) Prelude.<$> logGroup,
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
