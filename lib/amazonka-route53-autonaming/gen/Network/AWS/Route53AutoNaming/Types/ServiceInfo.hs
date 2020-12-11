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
    siInstanceCount,
    siARN,
    siHealthCheckConfig,
    siCreatorRequestId,
    siCreateDate,
    siHealthCheckCustomConfig,
    siNamespaceId,
    siName,
    siId,
    siDNSConfig,
    siDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.DNSConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig

-- | A complex type that contains information about the specified service.
--
-- /See:/ 'mkServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { instanceCount ::
      Lude.Maybe Lude.Int,
    arn :: Lude.Maybe Lude.Text,
    healthCheckConfig :: Lude.Maybe HealthCheckConfig,
    creatorRequestId :: Lude.Maybe Lude.Text,
    createDate :: Lude.Maybe Lude.Timestamp,
    healthCheckCustomConfig :: Lude.Maybe HealthCheckCustomConfig,
    namespaceId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    dnsConfig :: Lude.Maybe DNSConfig,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceInfo' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
-- * 'createDate' - The date and time that the service was created, in Unix format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
-- * 'creatorRequestId' - A unique string that identifies the request and that allows failed requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
-- * 'description' - The description of the service.
-- * 'dnsConfig' - A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
-- * 'healthCheckConfig' - /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, AWS Cloud Map associates the health check with the records that you specify in @DnsConfig@ .
--
-- For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
-- * 'healthCheckCustomConfig' - A complex type that contains information about an optional custom health check.
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- * 'id' - The ID that AWS Cloud Map assigned to the service when you created it.
-- * 'instanceCount' - The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
-- * 'name' - The name of the service.
-- * 'namespaceId' - The ID of the namespace that was used to create the service.
mkServiceInfo ::
  ServiceInfo
mkServiceInfo =
  ServiceInfo'
    { instanceCount = Lude.Nothing,
      arn = Lude.Nothing,
      healthCheckConfig = Lude.Nothing,
      creatorRequestId = Lude.Nothing,
      createDate = Lude.Nothing,
      healthCheckCustomConfig = Lude.Nothing,
      namespaceId = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      dnsConfig = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceCount :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Int)
siInstanceCount = Lens.lens (instanceCount :: ServiceInfo -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: ServiceInfo)
{-# DEPRECATED siInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siARN :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siARN = Lens.lens (arn :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ServiceInfo)
{-# DEPRECATED siARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, AWS Cloud Map associates the health check with the records that you specify in @DnsConfig@ .
--
-- For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siHealthCheckConfig :: Lens.Lens' ServiceInfo (Lude.Maybe HealthCheckConfig)
siHealthCheckConfig = Lens.lens (healthCheckConfig :: ServiceInfo -> Lude.Maybe HealthCheckConfig) (\s a -> s {healthCheckConfig = a} :: ServiceInfo)
{-# DEPRECATED siHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

-- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreatorRequestId :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siCreatorRequestId = Lens.lens (creatorRequestId :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {creatorRequestId = a} :: ServiceInfo)
{-# DEPRECATED siCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | The date and time that the service was created, in Unix format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreateDate :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Timestamp)
siCreateDate = Lens.lens (createDate :: ServiceInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: ServiceInfo)
{-# DEPRECATED siCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | A complex type that contains information about an optional custom health check.
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- /Note:/ Consider using 'healthCheckCustomConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siHealthCheckCustomConfig :: Lens.Lens' ServiceInfo (Lude.Maybe HealthCheckCustomConfig)
siHealthCheckCustomConfig = Lens.lens (healthCheckCustomConfig :: ServiceInfo -> Lude.Maybe HealthCheckCustomConfig) (\s a -> s {healthCheckCustomConfig = a} :: ServiceInfo)
{-# DEPRECATED siHealthCheckCustomConfig "Use generic-lens or generic-optics with 'healthCheckCustomConfig' instead." #-}

-- | The ID of the namespace that was used to create the service.
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNamespaceId :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siNamespaceId = Lens.lens (namespaceId :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: ServiceInfo)
{-# DEPRECATED siNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | The name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siName :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siName = Lens.lens (name :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ServiceInfo)
{-# DEPRECATED siName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID that AWS Cloud Map assigned to the service when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siId :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siId = Lens.lens (id :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ServiceInfo)
{-# DEPRECATED siId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDNSConfig :: Lens.Lens' ServiceInfo (Lude.Maybe DNSConfig)
siDNSConfig = Lens.lens (dnsConfig :: ServiceInfo -> Lude.Maybe DNSConfig) (\s a -> s {dnsConfig = a} :: ServiceInfo)
{-# DEPRECATED siDNSConfig "Use generic-lens or generic-optics with 'dnsConfig' instead." #-}

-- | The description of the service.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDescription :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siDescription = Lens.lens (description :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ServiceInfo)
{-# DEPRECATED siDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ServiceInfo where
  parseJSON =
    Lude.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Lude.<$> (x Lude..:? "InstanceCount")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "HealthCheckConfig")
            Lude.<*> (x Lude..:? "CreatorRequestId")
            Lude.<*> (x Lude..:? "CreateDate")
            Lude.<*> (x Lude..:? "HealthCheckCustomConfig")
            Lude.<*> (x Lude..:? "NamespaceId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "DnsConfig")
            Lude.<*> (x Lude..:? "Description")
      )
