{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eAttributes,
    eEndpointArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SNS.Types.String as Types

-- | Endpoint for mobile app and device.
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | Attributes for endpoint.
    attributes :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | EndpointArn for mobile app and device.
    endpointArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint ::
  Endpoint
mkEndpoint =
  Endpoint' {attributes = Core.Nothing, endpointArn = Core.Nothing}

-- | Attributes for endpoint.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAttributes :: Lens.Lens' Endpoint (Core.Maybe (Core.HashMap Types.String Types.String))
eAttributes = Lens.field @"attributes"
{-# DEPRECATED eAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | EndpointArn for mobile app and device.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointArn :: Lens.Lens' Endpoint (Core.Maybe Types.String)
eEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED eEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

instance Core.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Core.<$> ( x Core..@? "Attributes"
                   Core..<@> Core.parseXMLMap "entry" "key" "value"
               )
      Core.<*> (x Core..@? "EndpointArn")
