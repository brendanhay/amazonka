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
-- Module      : Amazonka.IoTFleetWise.Types.CloudWatchLogDeliveryOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CloudWatchLogDeliveryOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types.LogType
import qualified Amazonka.Prelude as Prelude

-- | The log delivery option to send data to Amazon CloudWatch Logs.
--
-- /See:/ 'newCloudWatchLogDeliveryOptions' smart constructor.
data CloudWatchLogDeliveryOptions = CloudWatchLogDeliveryOptions'
  { -- | The Amazon CloudWatch Logs group the operation sends data to.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The type of log to send data to Amazon CloudWatch Logs.
    logType :: LogType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogDeliveryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'cloudWatchLogDeliveryOptions_logGroupName' - The Amazon CloudWatch Logs group the operation sends data to.
--
-- 'logType', 'cloudWatchLogDeliveryOptions_logType' - The type of log to send data to Amazon CloudWatch Logs.
newCloudWatchLogDeliveryOptions ::
  -- | 'logType'
  LogType ->
  CloudWatchLogDeliveryOptions
newCloudWatchLogDeliveryOptions pLogType_ =
  CloudWatchLogDeliveryOptions'
    { logGroupName =
        Prelude.Nothing,
      logType = pLogType_
    }

-- | The Amazon CloudWatch Logs group the operation sends data to.
cloudWatchLogDeliveryOptions_logGroupName :: Lens.Lens' CloudWatchLogDeliveryOptions (Prelude.Maybe Prelude.Text)
cloudWatchLogDeliveryOptions_logGroupName = Lens.lens (\CloudWatchLogDeliveryOptions' {logGroupName} -> logGroupName) (\s@CloudWatchLogDeliveryOptions' {} a -> s {logGroupName = a} :: CloudWatchLogDeliveryOptions)

-- | The type of log to send data to Amazon CloudWatch Logs.
cloudWatchLogDeliveryOptions_logType :: Lens.Lens' CloudWatchLogDeliveryOptions LogType
cloudWatchLogDeliveryOptions_logType = Lens.lens (\CloudWatchLogDeliveryOptions' {logType} -> logType) (\s@CloudWatchLogDeliveryOptions' {} a -> s {logType = a} :: CloudWatchLogDeliveryOptions)

instance Core.FromJSON CloudWatchLogDeliveryOptions where
  parseJSON =
    Core.withObject
      "CloudWatchLogDeliveryOptions"
      ( \x ->
          CloudWatchLogDeliveryOptions'
            Prelude.<$> (x Core..:? "logGroupName")
            Prelude.<*> (x Core..: "logType")
      )

instance
  Prelude.Hashable
    CloudWatchLogDeliveryOptions
  where
  hashWithSalt _salt CloudWatchLogDeliveryOptions' {..} =
    _salt `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` logType

instance Prelude.NFData CloudWatchLogDeliveryOptions where
  rnf CloudWatchLogDeliveryOptions' {..} =
    Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf logType

instance Core.ToJSON CloudWatchLogDeliveryOptions where
  toJSON CloudWatchLogDeliveryOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("logGroupName" Core..=) Prelude.<$> logGroupName,
            Prelude.Just ("logType" Core..= logType)
          ]
      )
