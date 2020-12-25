{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpOutputSettings
  ( RtmpOutputSettings (..),

    -- * Smart constructor
    mkRtmpOutputSettings,

    -- * Lenses
    rosDestination,
    rosCertificateMode,
    rosConnectionRetryInterval,
    rosNumRetries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.OutputLocationRef as Types
import qualified Network.AWS.MediaLive.Types.RtmpOutputCertificateMode as Types
import qualified Network.AWS.Prelude as Core

-- | Rtmp Output Settings
--
-- /See:/ 'mkRtmpOutputSettings' smart constructor.
data RtmpOutputSettings = RtmpOutputSettings'
  { -- | The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
    destination :: Types.OutputLocationRef,
    -- | If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
    certificateMode :: Core.Maybe Types.RtmpOutputCertificateMode,
    -- | Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
    connectionRetryInterval :: Core.Maybe Core.Natural,
    -- | Number of retry attempts.
    numRetries :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RtmpOutputSettings' value with any optional fields omitted.
mkRtmpOutputSettings ::
  -- | 'destination'
  Types.OutputLocationRef ->
  RtmpOutputSettings
mkRtmpOutputSettings destination =
  RtmpOutputSettings'
    { destination,
      certificateMode = Core.Nothing,
      connectionRetryInterval = Core.Nothing,
      numRetries = Core.Nothing
    }

-- | The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rosDestination :: Lens.Lens' RtmpOutputSettings Types.OutputLocationRef
rosDestination = Lens.field @"destination"
{-# DEPRECATED rosDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
--
-- /Note:/ Consider using 'certificateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rosCertificateMode :: Lens.Lens' RtmpOutputSettings (Core.Maybe Types.RtmpOutputCertificateMode)
rosCertificateMode = Lens.field @"certificateMode"
{-# DEPRECATED rosCertificateMode "Use generic-lens or generic-optics with 'certificateMode' instead." #-}

-- | Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rosConnectionRetryInterval :: Lens.Lens' RtmpOutputSettings (Core.Maybe Core.Natural)
rosConnectionRetryInterval = Lens.field @"connectionRetryInterval"
{-# DEPRECATED rosConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | Number of retry attempts.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rosNumRetries :: Lens.Lens' RtmpOutputSettings (Core.Maybe Core.Natural)
rosNumRetries = Lens.field @"numRetries"
{-# DEPRECATED rosNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

instance Core.FromJSON RtmpOutputSettings where
  toJSON RtmpOutputSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("destination" Core..= destination),
            ("certificateMode" Core..=) Core.<$> certificateMode,
            ("connectionRetryInterval" Core..=)
              Core.<$> connectionRetryInterval,
            ("numRetries" Core..=) Core.<$> numRetries
          ]
      )

instance Core.FromJSON RtmpOutputSettings where
  parseJSON =
    Core.withObject "RtmpOutputSettings" Core.$
      \x ->
        RtmpOutputSettings'
          Core.<$> (x Core..: "destination")
          Core.<*> (x Core..:? "certificateMode")
          Core.<*> (x Core..:? "connectionRetryInterval")
          Core.<*> (x Core..:? "numRetries")
