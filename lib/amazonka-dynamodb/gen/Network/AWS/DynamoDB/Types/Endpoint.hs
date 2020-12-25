{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eAddress,
    eCachePeriodInMinutes,
  )
where

import qualified Network.AWS.DynamoDB.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An endpoint information details.
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | IP address of the endpoint.
    address :: Types.String,
    -- | Endpoint cache time to live (TTL) value.
    cachePeriodInMinutes :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint ::
  -- | 'address'
  Types.String ->
  -- | 'cachePeriodInMinutes'
  Core.Integer ->
  Endpoint
mkEndpoint address cachePeriodInMinutes =
  Endpoint' {address, cachePeriodInMinutes}

-- | IP address of the endpoint.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAddress :: Lens.Lens' Endpoint Types.String
eAddress = Lens.field @"address"
{-# DEPRECATED eAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | Endpoint cache time to live (TTL) value.
--
-- /Note:/ Consider using 'cachePeriodInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCachePeriodInMinutes :: Lens.Lens' Endpoint Core.Integer
eCachePeriodInMinutes = Lens.field @"cachePeriodInMinutes"
{-# DEPRECATED eCachePeriodInMinutes "Use generic-lens or generic-optics with 'cachePeriodInMinutes' instead." #-}

instance Core.FromJSON Endpoint where
  parseJSON =
    Core.withObject "Endpoint" Core.$
      \x ->
        Endpoint'
          Core.<$> (x Core..: "Address") Core.<*> (x Core..: "CachePeriodInMinutes")
