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
-- Module      : Amazonka.Route53.Types.QueryLoggingConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.QueryLoggingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that contains information about a configuration for DNS
-- query logging.
--
-- /See:/ 'newQueryLoggingConfig' smart constructor.
data QueryLoggingConfig = QueryLoggingConfig'
  { -- | The ID for a configuration for DNS query logging.
    id :: Prelude.Text,
    -- | The ID of the hosted zone that CloudWatch Logs is logging queries for.
    hostedZoneId :: ResourceId,
    -- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that
    -- Amazon Route 53 is publishing logs to.
    cloudWatchLogsLogGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryLoggingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'queryLoggingConfig_id' - The ID for a configuration for DNS query logging.
--
-- 'hostedZoneId', 'queryLoggingConfig_hostedZoneId' - The ID of the hosted zone that CloudWatch Logs is logging queries for.
--
-- 'cloudWatchLogsLogGroupArn', 'queryLoggingConfig_cloudWatchLogsLogGroupArn' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group that
-- Amazon Route 53 is publishing logs to.
newQueryLoggingConfig ::
  -- | 'id'
  Prelude.Text ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'cloudWatchLogsLogGroupArn'
  Prelude.Text ->
  QueryLoggingConfig
newQueryLoggingConfig
  pId_
  pHostedZoneId_
  pCloudWatchLogsLogGroupArn_ =
    QueryLoggingConfig'
      { id = pId_,
        hostedZoneId = pHostedZoneId_,
        cloudWatchLogsLogGroupArn =
          pCloudWatchLogsLogGroupArn_
      }

-- | The ID for a configuration for DNS query logging.
queryLoggingConfig_id :: Lens.Lens' QueryLoggingConfig Prelude.Text
queryLoggingConfig_id = Lens.lens (\QueryLoggingConfig' {id} -> id) (\s@QueryLoggingConfig' {} a -> s {id = a} :: QueryLoggingConfig)

-- | The ID of the hosted zone that CloudWatch Logs is logging queries for.
queryLoggingConfig_hostedZoneId :: Lens.Lens' QueryLoggingConfig ResourceId
queryLoggingConfig_hostedZoneId = Lens.lens (\QueryLoggingConfig' {hostedZoneId} -> hostedZoneId) (\s@QueryLoggingConfig' {} a -> s {hostedZoneId = a} :: QueryLoggingConfig)

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that
-- Amazon Route 53 is publishing logs to.
queryLoggingConfig_cloudWatchLogsLogGroupArn :: Lens.Lens' QueryLoggingConfig Prelude.Text
queryLoggingConfig_cloudWatchLogsLogGroupArn = Lens.lens (\QueryLoggingConfig' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@QueryLoggingConfig' {} a -> s {cloudWatchLogsLogGroupArn = a} :: QueryLoggingConfig)

instance Data.FromXML QueryLoggingConfig where
  parseXML x =
    QueryLoggingConfig'
      Prelude.<$> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "HostedZoneId")
      Prelude.<*> (x Data..@ "CloudWatchLogsLogGroupArn")

instance Prelude.Hashable QueryLoggingConfig where
  hashWithSalt _salt QueryLoggingConfig' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn

instance Prelude.NFData QueryLoggingConfig where
  rnf QueryLoggingConfig' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf cloudWatchLogsLogGroupArn
