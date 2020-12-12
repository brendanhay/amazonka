{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceSummary
  ( ServiceSummary (..),

    -- * Smart constructor
    mkServiceSummary,

    -- * Lenses
    ssInstanceCount,
    ssARN,
    ssHealthCheckConfig,
    ssCreateDate,
    ssHealthCheckCustomConfig,
    ssName,
    ssId,
    ssDNSConfig,
    ssDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.DNSConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig

-- | A complex type that contains information about a specified service.
--
-- /See:/ 'mkServiceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { instanceCount ::
      Lude.Maybe Lude.Int,
    arn :: Lude.Maybe Lude.Text,
    healthCheckConfig :: Lude.Maybe HealthCheckConfig,
    createDate :: Lude.Maybe Lude.Timestamp,
    healthCheckCustomConfig :: Lude.Maybe HealthCheckCustomConfig,
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

-- | Creates a value of 'ServiceSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
-- * 'createDate' - The date and time that the service was created.
-- * 'description' - The description that you specify when you create the service.
-- * 'dnsConfig' - Undocumented field.
-- * 'healthCheckConfig' - Undocumented field.
-- * 'healthCheckCustomConfig' - Undocumented field.
-- * 'id' - The ID that AWS Cloud Map assigned to the service when you created it.
-- * 'instanceCount' - The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
-- * 'name' - The name of the service.
mkServiceSummary ::
  ServiceSummary
mkServiceSummary =
  ServiceSummary'
    { instanceCount = Lude.Nothing,
      arn = Lude.Nothing,
      healthCheckConfig = Lude.Nothing,
      createDate = Lude.Nothing,
      healthCheckCustomConfig = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      dnsConfig = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssInstanceCount :: Lens.Lens' ServiceSummary (Lude.Maybe Lude.Int)
ssInstanceCount = Lens.lens (instanceCount :: ServiceSummary -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: ServiceSummary)
{-# DEPRECATED ssInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssARN :: Lens.Lens' ServiceSummary (Lude.Maybe Lude.Text)
ssARN = Lens.lens (arn :: ServiceSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ServiceSummary)
{-# DEPRECATED ssARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssHealthCheckConfig :: Lens.Lens' ServiceSummary (Lude.Maybe HealthCheckConfig)
ssHealthCheckConfig = Lens.lens (healthCheckConfig :: ServiceSummary -> Lude.Maybe HealthCheckConfig) (\s a -> s {healthCheckConfig = a} :: ServiceSummary)
{-# DEPRECATED ssHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

-- | The date and time that the service was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCreateDate :: Lens.Lens' ServiceSummary (Lude.Maybe Lude.Timestamp)
ssCreateDate = Lens.lens (createDate :: ServiceSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: ServiceSummary)
{-# DEPRECATED ssCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'healthCheckCustomConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssHealthCheckCustomConfig :: Lens.Lens' ServiceSummary (Lude.Maybe HealthCheckCustomConfig)
ssHealthCheckCustomConfig = Lens.lens (healthCheckCustomConfig :: ServiceSummary -> Lude.Maybe HealthCheckCustomConfig) (\s a -> s {healthCheckCustomConfig = a} :: ServiceSummary)
{-# DEPRECATED ssHealthCheckCustomConfig "Use generic-lens or generic-optics with 'healthCheckCustomConfig' instead." #-}

-- | The name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' ServiceSummary (Lude.Maybe Lude.Text)
ssName = Lens.lens (name :: ServiceSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ServiceSummary)
{-# DEPRECATED ssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID that AWS Cloud Map assigned to the service when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssId :: Lens.Lens' ServiceSummary (Lude.Maybe Lude.Text)
ssId = Lens.lens (id :: ServiceSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ServiceSummary)
{-# DEPRECATED ssId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dnsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDNSConfig :: Lens.Lens' ServiceSummary (Lude.Maybe DNSConfig)
ssDNSConfig = Lens.lens (dnsConfig :: ServiceSummary -> Lude.Maybe DNSConfig) (\s a -> s {dnsConfig = a} :: ServiceSummary)
{-# DEPRECATED ssDNSConfig "Use generic-lens or generic-optics with 'dnsConfig' instead." #-}

-- | The description that you specify when you create the service.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' ServiceSummary (Lude.Maybe Lude.Text)
ssDescription = Lens.lens (description :: ServiceSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ServiceSummary)
{-# DEPRECATED ssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ServiceSummary where
  parseJSON =
    Lude.withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            Lude.<$> (x Lude..:? "InstanceCount")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "HealthCheckConfig")
            Lude.<*> (x Lude..:? "CreateDate")
            Lude.<*> (x Lude..:? "HealthCheckCustomConfig")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "DnsConfig")
            Lude.<*> (x Lude..:? "Description")
      )
