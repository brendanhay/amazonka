{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HTTPInstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HTTPInstanceSummary
  ( HTTPInstanceSummary (..),

    -- * Smart constructor
    mkHTTPInstanceSummary,

    -- * Lenses
    httpisInstanceId,
    httpisNamespaceName,
    httpisAttributes,
    httpisServiceName,
    httpisHealthStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.HealthStatus

-- | In a response to a <https://docs.aws.amazon.com/cloud-map/latest/api/API_DiscoverInstances.html DiscoverInstances> request, @HttpInstanceSummary@ contains information about one instance that matches the values that you specified in the request.
--
-- /See:/ 'mkHTTPInstanceSummary' smart constructor.
data HTTPInstanceSummary = HTTPInstanceSummary'
  { instanceId ::
      Lude.Maybe Lude.Text,
    namespaceName :: Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    serviceName :: Lude.Maybe Lude.Text,
    healthStatus :: Lude.Maybe HealthStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPInstanceSummary' with the minimum fields required to make a request.
--
-- * 'attributes' - If you included any attributes when you registered the instance, the values of those attributes.
-- * 'healthStatus' - If you configured health checking in the service, the current health status of the service instance.
-- * 'instanceId' - The ID of an instance that matches the values that you specified in the request.
-- * 'namespaceName' - The name of the namespace that you specified when you registered the instance.
-- * 'serviceName' - The name of the service that you specified when you registered the instance.
mkHTTPInstanceSummary ::
  HTTPInstanceSummary
mkHTTPInstanceSummary =
  HTTPInstanceSummary'
    { instanceId = Lude.Nothing,
      namespaceName = Lude.Nothing,
      attributes = Lude.Nothing,
      serviceName = Lude.Nothing,
      healthStatus = Lude.Nothing
    }

-- | The ID of an instance that matches the values that you specified in the request.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpisInstanceId :: Lens.Lens' HTTPInstanceSummary (Lude.Maybe Lude.Text)
httpisInstanceId = Lens.lens (instanceId :: HTTPInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: HTTPInstanceSummary)
{-# DEPRECATED httpisInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the namespace that you specified when you registered the instance.
--
-- /Note:/ Consider using 'namespaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpisNamespaceName :: Lens.Lens' HTTPInstanceSummary (Lude.Maybe Lude.Text)
httpisNamespaceName = Lens.lens (namespaceName :: HTTPInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {namespaceName = a} :: HTTPInstanceSummary)
{-# DEPRECATED httpisNamespaceName "Use generic-lens or generic-optics with 'namespaceName' instead." #-}

-- | If you included any attributes when you registered the instance, the values of those attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpisAttributes :: Lens.Lens' HTTPInstanceSummary (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
httpisAttributes = Lens.lens (attributes :: HTTPInstanceSummary -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: HTTPInstanceSummary)
{-# DEPRECATED httpisAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The name of the service that you specified when you registered the instance.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpisServiceName :: Lens.Lens' HTTPInstanceSummary (Lude.Maybe Lude.Text)
httpisServiceName = Lens.lens (serviceName :: HTTPInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: HTTPInstanceSummary)
{-# DEPRECATED httpisServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | If you configured health checking in the service, the current health status of the service instance.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpisHealthStatus :: Lens.Lens' HTTPInstanceSummary (Lude.Maybe HealthStatus)
httpisHealthStatus = Lens.lens (healthStatus :: HTTPInstanceSummary -> Lude.Maybe HealthStatus) (\s a -> s {healthStatus = a} :: HTTPInstanceSummary)
{-# DEPRECATED httpisHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

instance Lude.FromJSON HTTPInstanceSummary where
  parseJSON =
    Lude.withObject
      "HTTPInstanceSummary"
      ( \x ->
          HTTPInstanceSummary'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "NamespaceName")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ServiceName")
            Lude.<*> (x Lude..:? "HealthStatus")
      )
