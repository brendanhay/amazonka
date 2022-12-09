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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatus
import qualified Amazonka.Prelude as Prelude

-- | Options for CloudWatch Logs as a logging destination.
--
-- /See:/ 'newVerifiedAccessLogCloudWatchLogsDestination' smart constructor.
data VerifiedAccessLogCloudWatchLogsDestination = VerifiedAccessLogCloudWatchLogsDestination'
  { -- | The delivery status for access logs.
    deliveryStatus :: Prelude.Maybe VerifiedAccessLogDeliveryStatus,
    -- | Indicates whether logging is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the CloudWatch Logs log group.
    logGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogCloudWatchLogsDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStatus', 'verifiedAccessLogCloudWatchLogsDestination_deliveryStatus' - The delivery status for access logs.
--
-- 'enabled', 'verifiedAccessLogCloudWatchLogsDestination_enabled' - Indicates whether logging is enabled.
--
-- 'logGroup', 'verifiedAccessLogCloudWatchLogsDestination_logGroup' - The ID of the CloudWatch Logs log group.
newVerifiedAccessLogCloudWatchLogsDestination ::
  VerifiedAccessLogCloudWatchLogsDestination
newVerifiedAccessLogCloudWatchLogsDestination =
  VerifiedAccessLogCloudWatchLogsDestination'
    { deliveryStatus =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      logGroup = Prelude.Nothing
    }

-- | The delivery status for access logs.
verifiedAccessLogCloudWatchLogsDestination_deliveryStatus :: Lens.Lens' VerifiedAccessLogCloudWatchLogsDestination (Prelude.Maybe VerifiedAccessLogDeliveryStatus)
verifiedAccessLogCloudWatchLogsDestination_deliveryStatus = Lens.lens (\VerifiedAccessLogCloudWatchLogsDestination' {deliveryStatus} -> deliveryStatus) (\s@VerifiedAccessLogCloudWatchLogsDestination' {} a -> s {deliveryStatus = a} :: VerifiedAccessLogCloudWatchLogsDestination)

-- | Indicates whether logging is enabled.
verifiedAccessLogCloudWatchLogsDestination_enabled :: Lens.Lens' VerifiedAccessLogCloudWatchLogsDestination (Prelude.Maybe Prelude.Bool)
verifiedAccessLogCloudWatchLogsDestination_enabled = Lens.lens (\VerifiedAccessLogCloudWatchLogsDestination' {enabled} -> enabled) (\s@VerifiedAccessLogCloudWatchLogsDestination' {} a -> s {enabled = a} :: VerifiedAccessLogCloudWatchLogsDestination)

-- | The ID of the CloudWatch Logs log group.
verifiedAccessLogCloudWatchLogsDestination_logGroup :: Lens.Lens' VerifiedAccessLogCloudWatchLogsDestination (Prelude.Maybe Prelude.Text)
verifiedAccessLogCloudWatchLogsDestination_logGroup = Lens.lens (\VerifiedAccessLogCloudWatchLogsDestination' {logGroup} -> logGroup) (\s@VerifiedAccessLogCloudWatchLogsDestination' {} a -> s {logGroup = a} :: VerifiedAccessLogCloudWatchLogsDestination)

instance
  Data.FromXML
    VerifiedAccessLogCloudWatchLogsDestination
  where
  parseXML x =
    VerifiedAccessLogCloudWatchLogsDestination'
      Prelude.<$> (x Data..@? "deliveryStatus")
        Prelude.<*> (x Data..@? "enabled")
        Prelude.<*> (x Data..@? "logGroup")

instance
  Prelude.Hashable
    VerifiedAccessLogCloudWatchLogsDestination
  where
  hashWithSalt
    _salt
    VerifiedAccessLogCloudWatchLogsDestination' {..} =
      _salt `Prelude.hashWithSalt` deliveryStatus
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` logGroup

instance
  Prelude.NFData
    VerifiedAccessLogCloudWatchLogsDestination
  where
  rnf VerifiedAccessLogCloudWatchLogsDestination' {..} =
    Prelude.rnf deliveryStatus
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf logGroup
