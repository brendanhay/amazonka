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
-- Module      : Network.AWS.Route53.Types.QueryLoggingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.QueryLoggingConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal

-- | A complex type that contains information about a configuration for DNS
-- query logging.
--
-- /See:/ 'newQueryLoggingConfig' smart constructor.
data QueryLoggingConfig = QueryLoggingConfig'
  { -- | The ID for a configuration for DNS query logging.
    id :: Core.Text,
    -- | The ID of the hosted zone that CloudWatch Logs is logging queries for.
    hostedZoneId :: ResourceId,
    -- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that
    -- Amazon Route 53 is publishing logs to.
    cloudWatchLogsLogGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'cloudWatchLogsLogGroupArn'
  Core.Text ->
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
queryLoggingConfig_id :: Lens.Lens' QueryLoggingConfig Core.Text
queryLoggingConfig_id = Lens.lens (\QueryLoggingConfig' {id} -> id) (\s@QueryLoggingConfig' {} a -> s {id = a} :: QueryLoggingConfig)

-- | The ID of the hosted zone that CloudWatch Logs is logging queries for.
queryLoggingConfig_hostedZoneId :: Lens.Lens' QueryLoggingConfig ResourceId
queryLoggingConfig_hostedZoneId = Lens.lens (\QueryLoggingConfig' {hostedZoneId} -> hostedZoneId) (\s@QueryLoggingConfig' {} a -> s {hostedZoneId = a} :: QueryLoggingConfig)

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that
-- Amazon Route 53 is publishing logs to.
queryLoggingConfig_cloudWatchLogsLogGroupArn :: Lens.Lens' QueryLoggingConfig Core.Text
queryLoggingConfig_cloudWatchLogsLogGroupArn = Lens.lens (\QueryLoggingConfig' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@QueryLoggingConfig' {} a -> s {cloudWatchLogsLogGroupArn = a} :: QueryLoggingConfig)

instance Core.FromXML QueryLoggingConfig where
  parseXML x =
    QueryLoggingConfig'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "HostedZoneId")
      Core.<*> (x Core..@ "CloudWatchLogsLogGroupArn")

instance Core.Hashable QueryLoggingConfig

instance Core.NFData QueryLoggingConfig
