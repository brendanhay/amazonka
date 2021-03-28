{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.ServiceSummary
  ( ServiceSummary (..)
  -- * Smart constructor
  , mkServiceSummary
  -- * Lenses
  , ssArn
  , ssCreateDate
  , ssDescription
  , ssDnsConfig
  , ssHealthCheckConfig
  , ssHealthCheckCustomConfig
  , ssId
  , ssInstanceCount
  , ssName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.Arn as Types
import qualified Network.AWS.Route53AutoNaming.Types.DnsConfig as Types
import qualified Network.AWS.Route53AutoNaming.Types.HealthCheckConfig as Types
import qualified Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceDescription as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types
import qualified Network.AWS.Route53AutoNaming.Types.ServiceName as Types

-- | A complex type that contains information about a specified service.
--
-- /See:/ 'mkServiceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
  , createDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the service was created.
  , description :: Core.Maybe Types.ResourceDescription
    -- ^ The description that you specify when you create the service.
  , dnsConfig :: Core.Maybe Types.DnsConfig
  , healthCheckConfig :: Core.Maybe Types.HealthCheckConfig
  , healthCheckCustomConfig :: Core.Maybe Types.HealthCheckCustomConfig
  , id :: Core.Maybe Types.ResourceId
    -- ^ The ID that AWS Cloud Map assigned to the service when you created it.
  , instanceCount :: Core.Maybe Core.Int
    -- ^ The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
  , name :: Core.Maybe Types.ServiceName
    -- ^ The name of the service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServiceSummary' value with any optional fields omitted.
mkServiceSummary
    :: ServiceSummary
mkServiceSummary
  = ServiceSummary'{arn = Core.Nothing, createDate = Core.Nothing,
                    description = Core.Nothing, dnsConfig = Core.Nothing,
                    healthCheckConfig = Core.Nothing,
                    healthCheckCustomConfig = Core.Nothing, id = Core.Nothing,
                    instanceCount = Core.Nothing, name = Core.Nothing}

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssArn :: Lens.Lens' ServiceSummary (Core.Maybe Types.Arn)
ssArn = Lens.field @"arn"
{-# INLINEABLE ssArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time that the service was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCreateDate :: Lens.Lens' ServiceSummary (Core.Maybe Core.NominalDiffTime)
ssCreateDate = Lens.field @"createDate"
{-# INLINEABLE ssCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | The description that you specify when you create the service.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' ServiceSummary (Core.Maybe Types.ResourceDescription)
ssDescription = Lens.field @"description"
{-# INLINEABLE ssDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dnsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDnsConfig :: Lens.Lens' ServiceSummary (Core.Maybe Types.DnsConfig)
ssDnsConfig = Lens.field @"dnsConfig"
{-# INLINEABLE ssDnsConfig #-}
{-# DEPRECATED dnsConfig "Use generic-lens or generic-optics with 'dnsConfig' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssHealthCheckConfig :: Lens.Lens' ServiceSummary (Core.Maybe Types.HealthCheckConfig)
ssHealthCheckConfig = Lens.field @"healthCheckConfig"
{-# INLINEABLE ssHealthCheckConfig #-}
{-# DEPRECATED healthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'healthCheckCustomConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssHealthCheckCustomConfig :: Lens.Lens' ServiceSummary (Core.Maybe Types.HealthCheckCustomConfig)
ssHealthCheckCustomConfig = Lens.field @"healthCheckCustomConfig"
{-# INLINEABLE ssHealthCheckCustomConfig #-}
{-# DEPRECATED healthCheckCustomConfig "Use generic-lens or generic-optics with 'healthCheckCustomConfig' instead"  #-}

-- | The ID that AWS Cloud Map assigned to the service when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssId :: Lens.Lens' ServiceSummary (Core.Maybe Types.ResourceId)
ssId = Lens.field @"id"
{-# INLINEABLE ssId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssInstanceCount :: Lens.Lens' ServiceSummary (Core.Maybe Core.Int)
ssInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE ssInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' ServiceSummary (Core.Maybe Types.ServiceName)
ssName = Lens.field @"name"
{-# INLINEABLE ssName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON ServiceSummary where
        parseJSON
          = Core.withObject "ServiceSummary" Core.$
              \ x ->
                ServiceSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "CreateDate" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "DnsConfig"
                    Core.<*> x Core..:? "HealthCheckConfig"
                    Core.<*> x Core..:? "HealthCheckCustomConfig"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "InstanceCount"
                    Core.<*> x Core..:? "Name"
