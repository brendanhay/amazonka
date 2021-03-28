{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.QueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.QueryLoggingConfig
  ( QueryLoggingConfig (..)
  -- * Smart constructor
  , mkQueryLoggingConfig
  -- * Lenses
  , qlcId
  , qlcHostedZoneId
  , qlcCloudWatchLogsLogGroupArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.CloudWatchLogsLogGroupArn as Types
import qualified Network.AWS.Route53.Types.Id as Types

-- | A complex type that contains information about a configuration for DNS query logging.
--
-- /See:/ 'mkQueryLoggingConfig' smart constructor.
data QueryLoggingConfig = QueryLoggingConfig'
  { id :: Types.Id
    -- ^ The ID for a configuration for DNS query logging.
  , hostedZoneId :: Types.ResourceId
    -- ^ The ID of the hosted zone that CloudWatch Logs is logging queries for. 
  , cloudWatchLogsLogGroupArn :: Types.CloudWatchLogsLogGroupArn
    -- ^ The Amazon Resource Name (ARN) of the CloudWatch Logs log group that Amazon Route 53 is publishing logs to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryLoggingConfig' value with any optional fields omitted.
mkQueryLoggingConfig
    :: Types.Id -- ^ 'id'
    -> Types.ResourceId -- ^ 'hostedZoneId'
    -> Types.CloudWatchLogsLogGroupArn -- ^ 'cloudWatchLogsLogGroupArn'
    -> QueryLoggingConfig
mkQueryLoggingConfig id hostedZoneId cloudWatchLogsLogGroupArn
  = QueryLoggingConfig'{id, hostedZoneId, cloudWatchLogsLogGroupArn}

-- | The ID for a configuration for DNS query logging.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qlcId :: Lens.Lens' QueryLoggingConfig Types.Id
qlcId = Lens.field @"id"
{-# INLINEABLE qlcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the hosted zone that CloudWatch Logs is logging queries for. 
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qlcHostedZoneId :: Lens.Lens' QueryLoggingConfig Types.ResourceId
qlcHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE qlcHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that Amazon Route 53 is publishing logs to.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qlcCloudWatchLogsLogGroupArn :: Lens.Lens' QueryLoggingConfig Types.CloudWatchLogsLogGroupArn
qlcCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# INLINEABLE qlcCloudWatchLogsLogGroupArn #-}
{-# DEPRECATED cloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead"  #-}

instance Core.FromXML QueryLoggingConfig where
        parseXML x
          = QueryLoggingConfig' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "HostedZoneId" Core.<*>
                x Core..@ "CloudWatchLogsLogGroupArn"
