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
    rosNumRetries,
    rosCertificateMode,
    rosConnectionRetryInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
import qualified Network.AWS.Prelude as Lude

-- | Rtmp Output Settings
--
-- /See:/ 'mkRtmpOutputSettings' smart constructor.
data RtmpOutputSettings = RtmpOutputSettings'
  { -- | The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
    destination :: OutputLocationRef,
    -- | Number of retry attempts.
    numRetries :: Lude.Maybe Lude.Natural,
    -- | If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
    certificateMode :: Lude.Maybe RtmpOutputCertificateMode,
    -- | Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
    connectionRetryInterval :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RtmpOutputSettings' with the minimum fields required to make a request.
--
-- * 'destination' - The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
-- * 'numRetries' - Number of retry attempts.
-- * 'certificateMode' - If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
-- * 'connectionRetryInterval' - Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
mkRtmpOutputSettings ::
  -- | 'destination'
  OutputLocationRef ->
  RtmpOutputSettings
mkRtmpOutputSettings pDestination_ =
  RtmpOutputSettings'
    { destination = pDestination_,
      numRetries = Lude.Nothing,
      certificateMode = Lude.Nothing,
      connectionRetryInterval = Lude.Nothing
    }

-- | The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rosDestination :: Lens.Lens' RtmpOutputSettings OutputLocationRef
rosDestination = Lens.lens (destination :: RtmpOutputSettings -> OutputLocationRef) (\s a -> s {destination = a} :: RtmpOutputSettings)
{-# DEPRECATED rosDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Number of retry attempts.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rosNumRetries :: Lens.Lens' RtmpOutputSettings (Lude.Maybe Lude.Natural)
rosNumRetries = Lens.lens (numRetries :: RtmpOutputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {numRetries = a} :: RtmpOutputSettings)
{-# DEPRECATED rosNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
--
-- /Note:/ Consider using 'certificateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rosCertificateMode :: Lens.Lens' RtmpOutputSettings (Lude.Maybe RtmpOutputCertificateMode)
rosCertificateMode = Lens.lens (certificateMode :: RtmpOutputSettings -> Lude.Maybe RtmpOutputCertificateMode) (\s a -> s {certificateMode = a} :: RtmpOutputSettings)
{-# DEPRECATED rosCertificateMode "Use generic-lens or generic-optics with 'certificateMode' instead." #-}

-- | Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rosConnectionRetryInterval :: Lens.Lens' RtmpOutputSettings (Lude.Maybe Lude.Natural)
rosConnectionRetryInterval = Lens.lens (connectionRetryInterval :: RtmpOutputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {connectionRetryInterval = a} :: RtmpOutputSettings)
{-# DEPRECATED rosConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

instance Lude.FromJSON RtmpOutputSettings where
  parseJSON =
    Lude.withObject
      "RtmpOutputSettings"
      ( \x ->
          RtmpOutputSettings'
            Lude.<$> (x Lude..: "destination")
            Lude.<*> (x Lude..:? "numRetries")
            Lude.<*> (x Lude..:? "certificateMode")
            Lude.<*> (x Lude..:? "connectionRetryInterval")
      )

instance Lude.ToJSON RtmpOutputSettings where
  toJSON RtmpOutputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("destination" Lude..= destination),
            ("numRetries" Lude..=) Lude.<$> numRetries,
            ("certificateMode" Lude..=) Lude.<$> certificateMode,
            ("connectionRetryInterval" Lude..=)
              Lude.<$> connectionRetryInterval
          ]
      )
