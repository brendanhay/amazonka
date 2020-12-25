{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    lsDescription,
    lsServicePrincipal,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.Description as Types
import qualified Network.AWS.Route53.Types.ServicePrincipal as Types

-- | If a health check or hosted zone was created by another service, @LinkedService@ is a complex type that describes the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- /See:/ 'mkLinkedService' smart constructor.
data LinkedService = LinkedService'
  { -- | If the health check or hosted zone was created by another service, an optional description that can be provided by the other service. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
    description :: Core.Maybe Types.Description,
    -- | If the health check or hosted zone was created by another service, the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
    servicePrincipal :: Core.Maybe Types.ServicePrincipal
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LinkedService' value with any optional fields omitted.
mkLinkedService ::
  LinkedService
mkLinkedService =
  LinkedService'
    { description = Core.Nothing,
      servicePrincipal = Core.Nothing
    }

-- | If the health check or hosted zone was created by another service, an optional description that can be provided by the other service. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsDescription :: Lens.Lens' LinkedService (Core.Maybe Types.Description)
lsDescription = Lens.field @"description"
{-# DEPRECATED lsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If the health check or hosted zone was created by another service, the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsServicePrincipal :: Lens.Lens' LinkedService (Core.Maybe Types.ServicePrincipal)
lsServicePrincipal = Lens.field @"servicePrincipal"
{-# DEPRECATED lsServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

instance Core.FromXML LinkedService where
  parseXML x =
    LinkedService'
      Core.<$> (x Core..@? "Description") Core.<*> (x Core..@? "ServicePrincipal")
