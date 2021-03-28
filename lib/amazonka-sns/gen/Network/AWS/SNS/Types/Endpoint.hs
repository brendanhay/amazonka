{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SNS.Types.Endpoint
  ( Endpoint (..)
  -- * Smart constructor
  , mkEndpoint
  -- * Lenses
  , eAttributes
  , eEndpointArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Endpoint for mobile app and device.
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Attributes for endpoint.
  , endpointArn :: Core.Maybe Core.Text
    -- ^ EndpointArn for mobile app and device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint
    :: Endpoint
mkEndpoint
  = Endpoint'{attributes = Core.Nothing, endpointArn = Core.Nothing}

-- | Attributes for endpoint.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAttributes :: Lens.Lens' Endpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
eAttributes = Lens.field @"attributes"
{-# INLINEABLE eAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | EndpointArn for mobile app and device.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointArn :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE eEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

instance Core.FromXML Endpoint where
        parseXML x
          = Endpoint' Core.<$>
              (x Core..@? "Attributes" Core..<@>
                 Core.parseXMLMap "entry" "key" "value")
                Core.<*> x Core..@? "EndpointArn"
