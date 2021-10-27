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
-- Module      : Network.AWS.SecurityHub.Types.AwsElbLoadBalancerAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsElbLoadBalancerAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsElbLoadBalancerAccessLog
import Network.AWS.SecurityHub.Types.AwsElbLoadBalancerConnectionDraining
import Network.AWS.SecurityHub.Types.AwsElbLoadBalancerConnectionSettings
import Network.AWS.SecurityHub.Types.AwsElbLoadBalancerCrossZoneLoadBalancing

-- | Contains attributes for the load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerAttributes' smart constructor.
data AwsElbLoadBalancerAttributes = AwsElbLoadBalancerAttributes'
  { -- | Cross-zone load balancing settings for the load balancer.
    --
    -- If cross-zone load balancing is enabled, the load balancer routes the
    -- request traffic evenly across all instances regardless of the
    -- Availability Zones.
    crossZoneLoadBalancing :: Prelude.Maybe AwsElbLoadBalancerCrossZoneLoadBalancing,
    -- | Information about the access log configuration for the load balancer.
    --
    -- If the access log is enabled, the load balancer captures detailed
    -- information about all requests. It delivers the information to a
    -- specified S3 bucket.
    accessLog :: Prelude.Maybe AwsElbLoadBalancerAccessLog,
    -- | Connection settings for the load balancer.
    --
    -- If an idle timeout is configured, the load balancer allows connections
    -- to remain idle for the specified duration. When a connection is idle, no
    -- data is sent over the connection.
    connectionSettings :: Prelude.Maybe AwsElbLoadBalancerConnectionSettings,
    -- | Information about the connection draining configuration for the load
    -- balancer.
    --
    -- If connection draining is enabled, the load balancer allows existing
    -- requests to complete before it shifts traffic away from a deregistered
    -- or unhealthy instance.
    connectionDraining :: Prelude.Maybe AwsElbLoadBalancerConnectionDraining
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossZoneLoadBalancing', 'awsElbLoadBalancerAttributes_crossZoneLoadBalancing' - Cross-zone load balancing settings for the load balancer.
--
-- If cross-zone load balancing is enabled, the load balancer routes the
-- request traffic evenly across all instances regardless of the
-- Availability Zones.
--
-- 'accessLog', 'awsElbLoadBalancerAttributes_accessLog' - Information about the access log configuration for the load balancer.
--
-- If the access log is enabled, the load balancer captures detailed
-- information about all requests. It delivers the information to a
-- specified S3 bucket.
--
-- 'connectionSettings', 'awsElbLoadBalancerAttributes_connectionSettings' - Connection settings for the load balancer.
--
-- If an idle timeout is configured, the load balancer allows connections
-- to remain idle for the specified duration. When a connection is idle, no
-- data is sent over the connection.
--
-- 'connectionDraining', 'awsElbLoadBalancerAttributes_connectionDraining' - Information about the connection draining configuration for the load
-- balancer.
--
-- If connection draining is enabled, the load balancer allows existing
-- requests to complete before it shifts traffic away from a deregistered
-- or unhealthy instance.
newAwsElbLoadBalancerAttributes ::
  AwsElbLoadBalancerAttributes
newAwsElbLoadBalancerAttributes =
  AwsElbLoadBalancerAttributes'
    { crossZoneLoadBalancing =
        Prelude.Nothing,
      accessLog = Prelude.Nothing,
      connectionSettings = Prelude.Nothing,
      connectionDraining = Prelude.Nothing
    }

-- | Cross-zone load balancing settings for the load balancer.
--
-- If cross-zone load balancing is enabled, the load balancer routes the
-- request traffic evenly across all instances regardless of the
-- Availability Zones.
awsElbLoadBalancerAttributes_crossZoneLoadBalancing :: Lens.Lens' AwsElbLoadBalancerAttributes (Prelude.Maybe AwsElbLoadBalancerCrossZoneLoadBalancing)
awsElbLoadBalancerAttributes_crossZoneLoadBalancing = Lens.lens (\AwsElbLoadBalancerAttributes' {crossZoneLoadBalancing} -> crossZoneLoadBalancing) (\s@AwsElbLoadBalancerAttributes' {} a -> s {crossZoneLoadBalancing = a} :: AwsElbLoadBalancerAttributes)

-- | Information about the access log configuration for the load balancer.
--
-- If the access log is enabled, the load balancer captures detailed
-- information about all requests. It delivers the information to a
-- specified S3 bucket.
awsElbLoadBalancerAttributes_accessLog :: Lens.Lens' AwsElbLoadBalancerAttributes (Prelude.Maybe AwsElbLoadBalancerAccessLog)
awsElbLoadBalancerAttributes_accessLog = Lens.lens (\AwsElbLoadBalancerAttributes' {accessLog} -> accessLog) (\s@AwsElbLoadBalancerAttributes' {} a -> s {accessLog = a} :: AwsElbLoadBalancerAttributes)

-- | Connection settings for the load balancer.
--
-- If an idle timeout is configured, the load balancer allows connections
-- to remain idle for the specified duration. When a connection is idle, no
-- data is sent over the connection.
awsElbLoadBalancerAttributes_connectionSettings :: Lens.Lens' AwsElbLoadBalancerAttributes (Prelude.Maybe AwsElbLoadBalancerConnectionSettings)
awsElbLoadBalancerAttributes_connectionSettings = Lens.lens (\AwsElbLoadBalancerAttributes' {connectionSettings} -> connectionSettings) (\s@AwsElbLoadBalancerAttributes' {} a -> s {connectionSettings = a} :: AwsElbLoadBalancerAttributes)

-- | Information about the connection draining configuration for the load
-- balancer.
--
-- If connection draining is enabled, the load balancer allows existing
-- requests to complete before it shifts traffic away from a deregistered
-- or unhealthy instance.
awsElbLoadBalancerAttributes_connectionDraining :: Lens.Lens' AwsElbLoadBalancerAttributes (Prelude.Maybe AwsElbLoadBalancerConnectionDraining)
awsElbLoadBalancerAttributes_connectionDraining = Lens.lens (\AwsElbLoadBalancerAttributes' {connectionDraining} -> connectionDraining) (\s@AwsElbLoadBalancerAttributes' {} a -> s {connectionDraining = a} :: AwsElbLoadBalancerAttributes)

instance Core.FromJSON AwsElbLoadBalancerAttributes where
  parseJSON =
    Core.withObject
      "AwsElbLoadBalancerAttributes"
      ( \x ->
          AwsElbLoadBalancerAttributes'
            Prelude.<$> (x Core..:? "CrossZoneLoadBalancing")
            Prelude.<*> (x Core..:? "AccessLog")
            Prelude.<*> (x Core..:? "ConnectionSettings")
            Prelude.<*> (x Core..:? "ConnectionDraining")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerAttributes

instance Prelude.NFData AwsElbLoadBalancerAttributes

instance Core.ToJSON AwsElbLoadBalancerAttributes where
  toJSON AwsElbLoadBalancerAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CrossZoneLoadBalancing" Core..=)
              Prelude.<$> crossZoneLoadBalancing,
            ("AccessLog" Core..=) Prelude.<$> accessLog,
            ("ConnectionSettings" Core..=)
              Prelude.<$> connectionSettings,
            ("ConnectionDraining" Core..=)
              Prelude.<$> connectionDraining
          ]
      )
