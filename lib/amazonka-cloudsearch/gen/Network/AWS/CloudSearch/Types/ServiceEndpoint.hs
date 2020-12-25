{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ServiceEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ServiceEndpoint
  ( ServiceEndpoint (..),

    -- * Smart constructor
    mkServiceEndpoint,

    -- * Lenses
    seEndpoint,
  )
where

import qualified Network.AWS.CloudSearch.Types.Endpoint as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The endpoint to which service requests can be submitted.
--
-- /See:/ 'mkServiceEndpoint' smart constructor.
newtype ServiceEndpoint = ServiceEndpoint'
  { endpoint :: Core.Maybe Types.Endpoint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceEndpoint' value with any optional fields omitted.
mkServiceEndpoint ::
  ServiceEndpoint
mkServiceEndpoint = ServiceEndpoint' {endpoint = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seEndpoint :: Lens.Lens' ServiceEndpoint (Core.Maybe Types.Endpoint)
seEndpoint = Lens.field @"endpoint"
{-# DEPRECATED seEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Core.FromXML ServiceEndpoint where
  parseXML x = ServiceEndpoint' Core.<$> (x Core..@? "Endpoint")
