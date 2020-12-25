{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eAddress,
    eHostedZoneId,
    ePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | This data type represents the information you need to connect to an Amazon RDS DB instance. This data type is used as a response element in the following actions:
--
--
--     * @CreateDBInstance@
--
--
--     * @DescribeDBInstances@
--
--
--     * @DeleteDBInstance@
--
--
-- For the data structure that represents Amazon Aurora DB cluster endpoints, see @DBClusterEndpoint@ .
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | Specifies the DNS address of the DB instance.
    address :: Core.Maybe Types.String,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
    hostedZoneId :: Core.Maybe Types.String,
    -- | Specifies the port that the database engine is listening on.
    port :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint ::
  Endpoint
mkEndpoint =
  Endpoint'
    { address = Core.Nothing,
      hostedZoneId = Core.Nothing,
      port = Core.Nothing
    }

-- | Specifies the DNS address of the DB instance.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAddress :: Lens.Lens' Endpoint (Core.Maybe Types.String)
eAddress = Lens.field @"address"
{-# DEPRECATED eAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eHostedZoneId :: Lens.Lens' Endpoint (Core.Maybe Types.String)
eHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED eHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | Specifies the port that the database engine is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePort :: Lens.Lens' Endpoint (Core.Maybe Core.Int)
ePort = Lens.field @"port"
{-# DEPRECATED ePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Core.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Core.<$> (x Core..@? "Address")
      Core.<*> (x Core..@? "HostedZoneId")
      Core.<*> (x Core..@? "Port")
