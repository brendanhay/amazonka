-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.LinkedService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.LinkedService
  ( LinkedService (..),

    -- * Smart constructor
    mkLinkedService,

    -- * Lenses
    lsServicePrincipal,
    lsDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | If a health check or hosted zone was created by another service, @LinkedService@ is a complex type that describes the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- /See:/ 'mkLinkedService' smart constructor.
data LinkedService = LinkedService'
  { servicePrincipal ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LinkedService' with the minimum fields required to make a request.
--
-- * 'description' - If the health check or hosted zone was created by another service, an optional description that can be provided by the other service. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
-- * 'servicePrincipal' - If the health check or hosted zone was created by another service, the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
mkLinkedService ::
  LinkedService
mkLinkedService =
  LinkedService'
    { servicePrincipal = Lude.Nothing,
      description = Lude.Nothing
    }

-- | If the health check or hosted zone was created by another service, the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsServicePrincipal :: Lens.Lens' LinkedService (Lude.Maybe Lude.Text)
lsServicePrincipal = Lens.lens (servicePrincipal :: LinkedService -> Lude.Maybe Lude.Text) (\s a -> s {servicePrincipal = a} :: LinkedService)
{-# DEPRECATED lsServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

-- | If the health check or hosted zone was created by another service, an optional description that can be provided by the other service. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsDescription :: Lens.Lens' LinkedService (Lude.Maybe Lude.Text)
lsDescription = Lens.lens (description :: LinkedService -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: LinkedService)
{-# DEPRECATED lsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML LinkedService where
  parseXML x =
    LinkedService'
      Lude.<$> (x Lude..@? "ServicePrincipal") Lude.<*> (x Lude..@? "Description")
