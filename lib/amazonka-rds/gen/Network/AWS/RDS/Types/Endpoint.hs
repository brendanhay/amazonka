{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Endpoint
  ( Endpoint (..)
  -- * Smart constructor
  , mkEndpoint
  -- * Lenses
  , eAddress
  , eHostedZoneId
  , ePort
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { address :: Core.Maybe Core.Text
    -- ^ Specifies the DNS address of the DB instance.
  , hostedZoneId :: Core.Maybe Core.Text
    -- ^ Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
  , port :: Core.Maybe Core.Int
    -- ^ Specifies the port that the database engine is listening on.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint
    :: Endpoint
mkEndpoint
  = Endpoint'{address = Core.Nothing, hostedZoneId = Core.Nothing,
              port = Core.Nothing}

-- | Specifies the DNS address of the DB instance.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAddress :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eAddress = Lens.field @"address"
{-# INLINEABLE eAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eHostedZoneId :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE eHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | Specifies the port that the database engine is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePort :: Lens.Lens' Endpoint (Core.Maybe Core.Int)
ePort = Lens.field @"port"
{-# INLINEABLE ePort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

instance Core.FromXML Endpoint where
        parseXML x
          = Endpoint' Core.<$>
              (x Core..@? "Address") Core.<*> x Core..@? "HostedZoneId" Core.<*>
                x Core..@? "Port"
