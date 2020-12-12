{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
  ( RealtimeEndpointInfo (..),

    -- * Smart constructor
    mkRealtimeEndpointInfo,

    -- * Lenses
    reiCreatedAt,
    reiEndpointURL,
    reiEndpointStatus,
    reiPeakRequestsPerSecond,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
import qualified Network.AWS.Prelude as Lude

-- | Describes the real-time endpoint information for an @MLModel@ .
--
-- /See:/ 'mkRealtimeEndpointInfo' smart constructor.
data RealtimeEndpointInfo = RealtimeEndpointInfo'
  { createdAt ::
      Lude.Maybe Lude.Timestamp,
    endpointURL :: Lude.Maybe Lude.Text,
    endpointStatus ::
      Lude.Maybe RealtimeEndpointStatus,
    peakRequestsPerSecond :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RealtimeEndpointInfo' with the minimum fields required to make a request.
--
-- * 'createdAt' - The time that the request to create the real-time endpoint for the @MLModel@ was received. The time is expressed in epoch time.
-- * 'endpointStatus' - The current status of the real-time endpoint for the @MLModel@ . This element can have one of the following values:
--
--
--     * @NONE@ - Endpoint does not exist or was previously deleted.
--
--     * @READY@ - Endpoint is ready to be used for real-time predictions.
--
--     * @UPDATING@ - Updating/creating the endpoint.
--
-- * 'endpointURL' - The URI that specifies where to send real-time prediction requests for the @MLModel@ .
-- * 'peakRequestsPerSecond' - The maximum processing rate for the real-time endpoint for @MLModel@ , measured in incoming requests per second.
mkRealtimeEndpointInfo ::
  RealtimeEndpointInfo
mkRealtimeEndpointInfo =
  RealtimeEndpointInfo'
    { createdAt = Lude.Nothing,
      endpointURL = Lude.Nothing,
      endpointStatus = Lude.Nothing,
      peakRequestsPerSecond = Lude.Nothing
    }

-- | The time that the request to create the real-time endpoint for the @MLModel@ was received. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiCreatedAt :: Lens.Lens' RealtimeEndpointInfo (Lude.Maybe Lude.Timestamp)
reiCreatedAt = Lens.lens (createdAt :: RealtimeEndpointInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: RealtimeEndpointInfo)
{-# DEPRECATED reiCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The URI that specifies where to send real-time prediction requests for the @MLModel@ .
--
-- /Note:/ Consider using 'endpointURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEndpointURL :: Lens.Lens' RealtimeEndpointInfo (Lude.Maybe Lude.Text)
reiEndpointURL = Lens.lens (endpointURL :: RealtimeEndpointInfo -> Lude.Maybe Lude.Text) (\s a -> s {endpointURL = a} :: RealtimeEndpointInfo)
{-# DEPRECATED reiEndpointURL "Use generic-lens or generic-optics with 'endpointURL' instead." #-}

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
reiEndpointStatus :: Lens.Lens' RealtimeEndpointInfo (Lude.Maybe RealtimeEndpointStatus)
reiEndpointStatus = Lens.lens (endpointStatus :: RealtimeEndpointInfo -> Lude.Maybe RealtimeEndpointStatus) (\s a -> s {endpointStatus = a} :: RealtimeEndpointInfo)
{-# DEPRECATED reiEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | The maximum processing rate for the real-time endpoint for @MLModel@ , measured in incoming requests per second.
--
-- /Note:/ Consider using 'peakRequestsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiPeakRequestsPerSecond :: Lens.Lens' RealtimeEndpointInfo (Lude.Maybe Lude.Int)
reiPeakRequestsPerSecond = Lens.lens (peakRequestsPerSecond :: RealtimeEndpointInfo -> Lude.Maybe Lude.Int) (\s a -> s {peakRequestsPerSecond = a} :: RealtimeEndpointInfo)
{-# DEPRECATED reiPeakRequestsPerSecond "Use generic-lens or generic-optics with 'peakRequestsPerSecond' instead." #-}

instance Lude.FromJSON RealtimeEndpointInfo where
  parseJSON =
    Lude.withObject
      "RealtimeEndpointInfo"
      ( \x ->
          RealtimeEndpointInfo'
            Lude.<$> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "EndpointUrl")
            Lude.<*> (x Lude..:? "EndpointStatus")
            Lude.<*> (x Lude..:? "PeakRequestsPerSecond")
      )
