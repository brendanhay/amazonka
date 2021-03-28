{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
  ( RealtimeEndpointInfo (..)
  -- * Smart constructor
  , mkRealtimeEndpointInfo
  -- * Lenses
  , reiCreatedAt
  , reiEndpointStatus
  , reiEndpointUrl
  , reiPeakRequestsPerSecond
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.EndpointUrl as Types
import qualified Network.AWS.MachineLearning.Types.RealtimeEndpointStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the real-time endpoint information for an @MLModel@ .
--
-- /See:/ 'mkRealtimeEndpointInfo' smart constructor.
data RealtimeEndpointInfo = RealtimeEndpointInfo'
  { createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the request to create the real-time endpoint for the @MLModel@ was received. The time is expressed in epoch time.
  , endpointStatus :: Core.Maybe Types.RealtimeEndpointStatus
    -- ^ The current status of the real-time endpoint for the @MLModel@ . This element can have one of the following values: 
--
--
--     * @NONE@ - Endpoint does not exist or was previously deleted.
--
--     * @READY@ - Endpoint is ready to be used for real-time predictions.
--
--     * @UPDATING@ - Updating/creating the endpoint. 
--
  , endpointUrl :: Core.Maybe Types.EndpointUrl
    -- ^ The URI that specifies where to send real-time prediction requests for the @MLModel@ .
  , peakRequestsPerSecond :: Core.Maybe Core.Int
    -- ^ The maximum processing rate for the real-time endpoint for @MLModel@ , measured in incoming requests per second.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RealtimeEndpointInfo' value with any optional fields omitted.
mkRealtimeEndpointInfo
    :: RealtimeEndpointInfo
mkRealtimeEndpointInfo
  = RealtimeEndpointInfo'{createdAt = Core.Nothing,
                          endpointStatus = Core.Nothing, endpointUrl = Core.Nothing,
                          peakRequestsPerSecond = Core.Nothing}

-- | The time that the request to create the real-time endpoint for the @MLModel@ was received. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiCreatedAt :: Lens.Lens' RealtimeEndpointInfo (Core.Maybe Core.NominalDiffTime)
reiCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE reiCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The current status of the real-time endpoint for the @MLModel@ . This element can have one of the following values: 
--
--
--     * @NONE@ - Endpoint does not exist or was previously deleted.
--
--     * @READY@ - Endpoint is ready to be used for real-time predictions.
--
--     * @UPDATING@ - Updating/creating the endpoint. 
--
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEndpointStatus :: Lens.Lens' RealtimeEndpointInfo (Core.Maybe Types.RealtimeEndpointStatus)
reiEndpointStatus = Lens.field @"endpointStatus"
{-# INLINEABLE reiEndpointStatus #-}
{-# DEPRECATED endpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead"  #-}

-- | The URI that specifies where to send real-time prediction requests for the @MLModel@ .
--
-- /Note:/ Consider using 'endpointUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEndpointUrl :: Lens.Lens' RealtimeEndpointInfo (Core.Maybe Types.EndpointUrl)
reiEndpointUrl = Lens.field @"endpointUrl"
{-# INLINEABLE reiEndpointUrl #-}
{-# DEPRECATED endpointUrl "Use generic-lens or generic-optics with 'endpointUrl' instead"  #-}

-- | The maximum processing rate for the real-time endpoint for @MLModel@ , measured in incoming requests per second.
--
-- /Note:/ Consider using 'peakRequestsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiPeakRequestsPerSecond :: Lens.Lens' RealtimeEndpointInfo (Core.Maybe Core.Int)
reiPeakRequestsPerSecond = Lens.field @"peakRequestsPerSecond"
{-# INLINEABLE reiPeakRequestsPerSecond #-}
{-# DEPRECATED peakRequestsPerSecond "Use generic-lens or generic-optics with 'peakRequestsPerSecond' instead"  #-}

instance Core.FromJSON RealtimeEndpointInfo where
        parseJSON
          = Core.withObject "RealtimeEndpointInfo" Core.$
              \ x ->
                RealtimeEndpointInfo' Core.<$>
                  (x Core..:? "CreatedAt") Core.<*> x Core..:? "EndpointStatus"
                    Core.<*> x Core..:? "EndpointUrl"
                    Core.<*> x Core..:? "PeakRequestsPerSecond"
