{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpOutputSettings
  ( UdpOutputSettings (..),

    -- * Smart constructor
    mkUdpOutputSettings,

    -- * Lenses
    uosFecOutputSettings,
    uosBufferMsec,
    uosDestination,
    uosContainerSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FecOutputSettings
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.MediaLive.Types.UdpContainerSettings
import qualified Network.AWS.Prelude as Lude

-- | Udp Output Settings
--
-- /See:/ 'mkUdpOutputSettings' smart constructor.
data UdpOutputSettings = UdpOutputSettings'
  { fecOutputSettings ::
      Lude.Maybe FecOutputSettings,
    bufferMsec :: Lude.Maybe Lude.Natural,
    destination :: OutputLocationRef,
    containerSettings :: UdpContainerSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UdpOutputSettings' with the minimum fields required to make a request.
--
-- * 'bufferMsec' - UDP output buffering in milliseconds. Larger values increase latency through the transcoder but simultaneously assist the transcoder in maintaining a constant, low-jitter UDP/RTP output while accommodating clock recovery, input switching, input disruptions, picture reordering, etc.
-- * 'containerSettings' - Undocumented field.
-- * 'destination' - Destination address and port number for RTP or UDP packets. Can be unicast or multicast RTP or UDP (eg. rtp://239.10.10.10:5001 or udp://10.100.100.100:5002).
-- * 'fecOutputSettings' - Settings for enabling and adjusting Forward Error Correction on UDP outputs.
mkUdpOutputSettings ::
  -- | 'destination'
  OutputLocationRef ->
  -- | 'containerSettings'
  UdpContainerSettings ->
  UdpOutputSettings
mkUdpOutputSettings pDestination_ pContainerSettings_ =
  UdpOutputSettings'
    { fecOutputSettings = Lude.Nothing,
      bufferMsec = Lude.Nothing,
      destination = pDestination_,
      containerSettings = pContainerSettings_
    }

-- | Settings for enabling and adjusting Forward Error Correction on UDP outputs.
--
-- /Note:/ Consider using 'fecOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uosFecOutputSettings :: Lens.Lens' UdpOutputSettings (Lude.Maybe FecOutputSettings)
uosFecOutputSettings = Lens.lens (fecOutputSettings :: UdpOutputSettings -> Lude.Maybe FecOutputSettings) (\s a -> s {fecOutputSettings = a} :: UdpOutputSettings)
{-# DEPRECATED uosFecOutputSettings "Use generic-lens or generic-optics with 'fecOutputSettings' instead." #-}

-- | UDP output buffering in milliseconds. Larger values increase latency through the transcoder but simultaneously assist the transcoder in maintaining a constant, low-jitter UDP/RTP output while accommodating clock recovery, input switching, input disruptions, picture reordering, etc.
--
-- /Note:/ Consider using 'bufferMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uosBufferMsec :: Lens.Lens' UdpOutputSettings (Lude.Maybe Lude.Natural)
uosBufferMsec = Lens.lens (bufferMsec :: UdpOutputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bufferMsec = a} :: UdpOutputSettings)
{-# DEPRECATED uosBufferMsec "Use generic-lens or generic-optics with 'bufferMsec' instead." #-}

-- | Destination address and port number for RTP or UDP packets. Can be unicast or multicast RTP or UDP (eg. rtp://239.10.10.10:5001 or udp://10.100.100.100:5002).
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uosDestination :: Lens.Lens' UdpOutputSettings OutputLocationRef
uosDestination = Lens.lens (destination :: UdpOutputSettings -> OutputLocationRef) (\s a -> s {destination = a} :: UdpOutputSettings)
{-# DEPRECATED uosDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'containerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uosContainerSettings :: Lens.Lens' UdpOutputSettings UdpContainerSettings
uosContainerSettings = Lens.lens (containerSettings :: UdpOutputSettings -> UdpContainerSettings) (\s a -> s {containerSettings = a} :: UdpOutputSettings)
{-# DEPRECATED uosContainerSettings "Use generic-lens or generic-optics with 'containerSettings' instead." #-}

instance Lude.FromJSON UdpOutputSettings where
  parseJSON =
    Lude.withObject
      "UdpOutputSettings"
      ( \x ->
          UdpOutputSettings'
            Lude.<$> (x Lude..:? "fecOutputSettings")
            Lude.<*> (x Lude..:? "bufferMsec")
            Lude.<*> (x Lude..: "destination")
            Lude.<*> (x Lude..: "containerSettings")
      )

instance Lude.ToJSON UdpOutputSettings where
  toJSON UdpOutputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fecOutputSettings" Lude..=) Lude.<$> fecOutputSettings,
            ("bufferMsec" Lude..=) Lude.<$> bufferMsec,
            Lude.Just ("destination" Lude..= destination),
            Lude.Just ("containerSettings" Lude..= containerSettings)
          ]
      )
