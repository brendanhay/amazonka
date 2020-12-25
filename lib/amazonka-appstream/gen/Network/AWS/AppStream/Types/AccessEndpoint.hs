{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.AccessEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.AccessEndpoint
  ( AccessEndpoint (..),

    -- * Smart constructor
    mkAccessEndpoint,

    -- * Lenses
    aeEndpointType,
    aeVpceId,
  )
where

import qualified Network.AWS.AppStream.Types.AccessEndpointType as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an interface VPC endpoint (interface endpoint) that lets you create a private connection between the virtual private cloud (VPC) that you specify and AppStream 2.0. When you specify an interface endpoint for a stack, users of the stack can connect to AppStream 2.0 only through that endpoint. When you specify an interface endpoint for an image builder, administrators can connect to the image builder only through that endpoint.
--
-- /See:/ 'mkAccessEndpoint' smart constructor.
data AccessEndpoint = AccessEndpoint'
  { -- | The type of interface endpoint.
    endpointType :: Types.AccessEndpointType,
    -- | The identifier (ID) of the VPC in which the interface endpoint is used.
    vpceId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessEndpoint' value with any optional fields omitted.
mkAccessEndpoint ::
  -- | 'endpointType'
  Types.AccessEndpointType ->
  AccessEndpoint
mkAccessEndpoint endpointType =
  AccessEndpoint' {endpointType, vpceId = Core.Nothing}

-- | The type of interface endpoint.
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEndpointType :: Lens.Lens' AccessEndpoint Types.AccessEndpointType
aeEndpointType = Lens.field @"endpointType"
{-# DEPRECATED aeEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | The identifier (ID) of the VPC in which the interface endpoint is used.
--
-- /Note:/ Consider using 'vpceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeVpceId :: Lens.Lens' AccessEndpoint (Core.Maybe Types.String)
aeVpceId = Lens.field @"vpceId"
{-# DEPRECATED aeVpceId "Use generic-lens or generic-optics with 'vpceId' instead." #-}

instance Core.FromJSON AccessEndpoint where
  toJSON AccessEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointType" Core..= endpointType),
            ("VpceId" Core..=) Core.<$> vpceId
          ]
      )

instance Core.FromJSON AccessEndpoint where
  parseJSON =
    Core.withObject "AccessEndpoint" Core.$
      \x ->
        AccessEndpoint'
          Core.<$> (x Core..: "EndpointType") Core.<*> (x Core..:? "VpceId")
