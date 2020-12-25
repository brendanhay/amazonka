{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceInfo
  ( ServiceInfo (..),

    -- * Smart constructor
    mkServiceInfo,

    -- * Lenses
    siArn,
    siCreateDate,
    siCreatorRequestId,
    siDescription,
    siDnsConfig,
    siHealthCheckConfig,
    siHealthCheckCustomConfig,
    siId,
    siInstanceCount,
    siName,
    siNamespaceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.Arn as Types
import qualified Network.AWS.Route53AutoNaming.Types.Description as Types
import qualified Network.AWS.Route53AutoNaming.Types.DnsConfig as Types
import qualified Network.AWS.Route53AutoNaming.Types.HealthCheckConfig as Types
import qualified Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig as Types
import qualified Network.AWS.Route53AutoNaming.Types.Name as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types

-- | A complex type that contains information about the specified service.
--
-- /See:/ 'mkServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { -- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
    arn :: Core.Maybe Types.Arn,
    -- | The date and time that the service was created, in Unix format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
    createDate :: Core.Maybe Core.NominalDiffTime,
    -- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
    creatorRequestId :: Core.Maybe Types.ResourceId,
    -- | The description of the service.
    description :: Core.Maybe Types.Description,
    -- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
    dnsConfig :: Core.Maybe Types.DnsConfig,
    -- | /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, AWS Cloud Map associates the health check with the records that you specify in @DnsConfig@ .
    --
    -- For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
    healthCheckConfig :: Core.Maybe Types.HealthCheckConfig,
    -- | A complex type that contains information about an optional custom health check.
    --
    -- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
    healthCheckCustomConfig :: Core.Maybe Types.HealthCheckCustomConfig,
    -- | The ID that AWS Cloud Map assigned to the service when you created it.
    id :: Core.Maybe Types.ResourceId,
    -- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
    instanceCount :: Core.Maybe Core.Int,
    -- | The name of the service.
    name :: Core.Maybe Types.Name,
    -- | The ID of the namespace that was used to create the service.
    namespaceId :: Core.Maybe Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ServiceInfo' value with any optional fields omitted.
mkServiceInfo ::
  ServiceInfo
mkServiceInfo =
  ServiceInfo'
    { arn = Core.Nothing,
      createDate = Core.Nothing,
      creatorRequestId = Core.Nothing,
      description = Core.Nothing,
      dnsConfig = Core.Nothing,
      healthCheckConfig = Core.Nothing,
      healthCheckCustomConfig = Core.Nothing,
      id = Core.Nothing,
      instanceCount = Core.Nothing,
      name = Core.Nothing,
      namespaceId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siArn :: Lens.Lens' ServiceInfo (Core.Maybe Types.Arn)
siArn = Lens.field @"arn"
{-# DEPRECATED siArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time that the service was created, in Unix format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreateDate :: Lens.Lens' ServiceInfo (Core.Maybe Core.NominalDiffTime)
siCreateDate = Lens.field @"createDate"
{-# DEPRECATED siCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreatorRequestId :: Lens.Lens' ServiceInfo (Core.Maybe Types.ResourceId)
siCreatorRequestId = Lens.field @"creatorRequestId"
{-# DEPRECATED siCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | The description of the service.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDescription :: Lens.Lens' ServiceInfo (Core.Maybe Types.Description)
siDescription = Lens.field @"description"
{-# DEPRECATED siDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDnsConfig :: Lens.Lens' ServiceInfo (Core.Maybe Types.DnsConfig)
siDnsConfig = Lens.field @"dnsConfig"
{-# DEPRECATED siDnsConfig "Use generic-lens or generic-optics with 'dnsConfig' instead." #-}

-- | /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, AWS Cloud Map associates the health check with the records that you specify in @DnsConfig@ .
--
-- For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siHealthCheckConfig :: Lens.Lens' ServiceInfo (Core.Maybe Types.HealthCheckConfig)
siHealthCheckConfig = Lens.field @"healthCheckConfig"
{-# DEPRECATED siHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

-- | A complex type that contains information about an optional custom health check.
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- /Note:/ Consider using 'healthCheckCustomConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siHealthCheckCustomConfig :: Lens.Lens' ServiceInfo (Core.Maybe Types.HealthCheckCustomConfig)
siHealthCheckCustomConfig = Lens.field @"healthCheckCustomConfig"
{-# DEPRECATED siHealthCheckCustomConfig "Use generic-lens or generic-optics with 'healthCheckCustomConfig' instead." #-}

-- | The ID that AWS Cloud Map assigned to the service when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siId :: Lens.Lens' ServiceInfo (Core.Maybe Types.ResourceId)
siId = Lens.field @"id"
{-# DEPRECATED siId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceCount :: Lens.Lens' ServiceInfo (Core.Maybe Core.Int)
siInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED siInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siName :: Lens.Lens' ServiceInfo (Core.Maybe Types.Name)
siName = Lens.field @"name"
{-# DEPRECATED siName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the namespace that was used to create the service.
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNamespaceId :: Lens.Lens' ServiceInfo (Core.Maybe Types.ResourceId)
siNamespaceId = Lens.field @"namespaceId"
{-# DEPRECATED siNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

instance Core.FromJSON ServiceInfo where
  parseJSON =
    Core.withObject "ServiceInfo" Core.$
      \x ->
        ServiceInfo'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CreateDate")
          Core.<*> (x Core..:? "CreatorRequestId")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "DnsConfig")
          Core.<*> (x Core..:? "HealthCheckConfig")
          Core.<*> (x Core..:? "HealthCheckCustomConfig")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "InstanceCount")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "NamespaceId")
