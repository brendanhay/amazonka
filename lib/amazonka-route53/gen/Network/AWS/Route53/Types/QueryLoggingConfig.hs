{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.QueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.QueryLoggingConfig
  ( QueryLoggingConfig (..),

    -- * Smart constructor
    mkQueryLoggingConfig,

    -- * Lenses
    qlcId,
    qlcHostedZoneId,
    qlcCloudWatchLogsLogGroupArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.CloudWatchLogsLogGroupArn as Types
import qualified Network.AWS.Route53.Types.Id as Types

-- | A complex type that contains information about a configuration for DNS query logging.
--
-- /See:/ 'mkQueryLoggingConfig' smart constructor.
data QueryLoggingConfig = QueryLoggingConfig'
  { -- | The ID for a configuration for DNS query logging.
    id :: Types.Id,
    -- | The ID of the hosted zone that CloudWatch Logs is logging queries for.
    hostedZoneId :: Types.ResourceId,
    -- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that Amazon Route 53 is publishing logs to.
    cloudWatchLogsLogGroupArn :: Types.CloudWatchLogsLogGroupArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryLoggingConfig' value with any optional fields omitted.
mkQueryLoggingConfig ::
  -- | 'id'
  Types.Id ->
  -- | 'hostedZoneId'
  Types.ResourceId ->
  -- | 'cloudWatchLogsLogGroupArn'
  Types.CloudWatchLogsLogGroupArn ->
  QueryLoggingConfig
mkQueryLoggingConfig id hostedZoneId cloudWatchLogsLogGroupArn =
  QueryLoggingConfig' {id, hostedZoneId, cloudWatchLogsLogGroupArn}

-- | The ID for a configuration for DNS query logging.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qlcId :: Lens.Lens' QueryLoggingConfig Types.Id
qlcId = Lens.field @"id"
{-# DEPRECATED qlcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the hosted zone that CloudWatch Logs is logging queries for.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qlcHostedZoneId :: Lens.Lens' QueryLoggingConfig Types.ResourceId
qlcHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED qlcHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that Amazon Route 53 is publishing logs to.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qlcCloudWatchLogsLogGroupArn :: Lens.Lens' QueryLoggingConfig Types.CloudWatchLogsLogGroupArn
qlcCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# DEPRECATED qlcCloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead." #-}

instance Core.FromXML QueryLoggingConfig where
  parseXML x =
    QueryLoggingConfig'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "HostedZoneId")
      Core.<*> (x Core..@ "CloudWatchLogsLogGroupArn")
