{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EsamSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EsamSettings
  ( EsamSettings (..),

    -- * Smart constructor
    mkEsamSettings,

    -- * Lenses
    esManifestConfirmConditionNotification,
    esResponseSignalPreroll,
    esSignalProcessingNotification,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
import Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification
import qualified Network.AWS.Prelude as Lude

-- | Settings for Event Signaling And Messaging (ESAM). If you don't do ad insertion, you can ignore these settings.
--
-- /See:/ 'mkEsamSettings' smart constructor.
data EsamSettings = EsamSettings'
  { manifestConfirmConditionNotification ::
      Lude.Maybe EsamManifestConfirmConditionNotification,
    responseSignalPreroll :: Lude.Maybe Lude.Natural,
    signalProcessingNotification ::
      Lude.Maybe EsamSignalProcessingNotification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EsamSettings' with the minimum fields required to make a request.
--
-- * 'manifestConfirmConditionNotification' - Specifies an ESAM ManifestConfirmConditionNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning instructions that you provide in the setting MCC XML (mccXml).
-- * 'responseSignalPreroll' - Specifies the stream distance, in milliseconds, between the SCTE 35 messages that the transcoder places and the splice points that they refer to. If the time between the start of the asset and the SCTE-35 message is less than this value, then the transcoder places the SCTE-35 marker at the beginning of the stream.
-- * 'signalProcessingNotification' - Specifies an ESAM SignalProcessingNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing instructions that you provide in the setting SCC XML (sccXml).
mkEsamSettings ::
  EsamSettings
mkEsamSettings =
  EsamSettings'
    { manifestConfirmConditionNotification =
        Lude.Nothing,
      responseSignalPreroll = Lude.Nothing,
      signalProcessingNotification = Lude.Nothing
    }

-- | Specifies an ESAM ManifestConfirmConditionNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning instructions that you provide in the setting MCC XML (mccXml).
--
-- /Note:/ Consider using 'manifestConfirmConditionNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esManifestConfirmConditionNotification :: Lens.Lens' EsamSettings (Lude.Maybe EsamManifestConfirmConditionNotification)
esManifestConfirmConditionNotification = Lens.lens (manifestConfirmConditionNotification :: EsamSettings -> Lude.Maybe EsamManifestConfirmConditionNotification) (\s a -> s {manifestConfirmConditionNotification = a} :: EsamSettings)
{-# DEPRECATED esManifestConfirmConditionNotification "Use generic-lens or generic-optics with 'manifestConfirmConditionNotification' instead." #-}

-- | Specifies the stream distance, in milliseconds, between the SCTE 35 messages that the transcoder places and the splice points that they refer to. If the time between the start of the asset and the SCTE-35 message is less than this value, then the transcoder places the SCTE-35 marker at the beginning of the stream.
--
-- /Note:/ Consider using 'responseSignalPreroll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esResponseSignalPreroll :: Lens.Lens' EsamSettings (Lude.Maybe Lude.Natural)
esResponseSignalPreroll = Lens.lens (responseSignalPreroll :: EsamSettings -> Lude.Maybe Lude.Natural) (\s a -> s {responseSignalPreroll = a} :: EsamSettings)
{-# DEPRECATED esResponseSignalPreroll "Use generic-lens or generic-optics with 'responseSignalPreroll' instead." #-}

-- | Specifies an ESAM SignalProcessingNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing instructions that you provide in the setting SCC XML (sccXml).
--
-- /Note:/ Consider using 'signalProcessingNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSignalProcessingNotification :: Lens.Lens' EsamSettings (Lude.Maybe EsamSignalProcessingNotification)
esSignalProcessingNotification = Lens.lens (signalProcessingNotification :: EsamSettings -> Lude.Maybe EsamSignalProcessingNotification) (\s a -> s {signalProcessingNotification = a} :: EsamSettings)
{-# DEPRECATED esSignalProcessingNotification "Use generic-lens or generic-optics with 'signalProcessingNotification' instead." #-}

instance Lude.FromJSON EsamSettings where
  parseJSON =
    Lude.withObject
      "EsamSettings"
      ( \x ->
          EsamSettings'
            Lude.<$> (x Lude..:? "manifestConfirmConditionNotification")
            Lude.<*> (x Lude..:? "responseSignalPreroll")
            Lude.<*> (x Lude..:? "signalProcessingNotification")
      )

instance Lude.ToJSON EsamSettings where
  toJSON EsamSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("manifestConfirmConditionNotification" Lude..=)
              Lude.<$> manifestConfirmConditionNotification,
            ("responseSignalPreroll" Lude..=) Lude.<$> responseSignalPreroll,
            ("signalProcessingNotification" Lude..=)
              Lude.<$> signalProcessingNotification
          ]
      )
