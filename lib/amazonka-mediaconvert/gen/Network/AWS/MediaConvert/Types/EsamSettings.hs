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
import qualified Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification as Types
import qualified Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for Event Signaling And Messaging (ESAM). If you don't do ad insertion, you can ignore these settings.
--
-- /See:/ 'mkEsamSettings' smart constructor.
data EsamSettings = EsamSettings'
  { -- | Specifies an ESAM ManifestConfirmConditionNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning instructions that you provide in the setting MCC XML (mccXml).
    manifestConfirmConditionNotification :: Core.Maybe Types.EsamManifestConfirmConditionNotification,
    -- | Specifies the stream distance, in milliseconds, between the SCTE 35 messages that the transcoder places and the splice points that they refer to. If the time between the start of the asset and the SCTE-35 message is less than this value, then the transcoder places the SCTE-35 marker at the beginning of the stream.
    responseSignalPreroll :: Core.Maybe Core.Natural,
    -- | Specifies an ESAM SignalProcessingNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing instructions that you provide in the setting SCC XML (sccXml).
    signalProcessingNotification :: Core.Maybe Types.EsamSignalProcessingNotification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EsamSettings' value with any optional fields omitted.
mkEsamSettings ::
  EsamSettings
mkEsamSettings =
  EsamSettings'
    { manifestConfirmConditionNotification =
        Core.Nothing,
      responseSignalPreroll = Core.Nothing,
      signalProcessingNotification = Core.Nothing
    }

-- | Specifies an ESAM ManifestConfirmConditionNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning instructions that you provide in the setting MCC XML (mccXml).
--
-- /Note:/ Consider using 'manifestConfirmConditionNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esManifestConfirmConditionNotification :: Lens.Lens' EsamSettings (Core.Maybe Types.EsamManifestConfirmConditionNotification)
esManifestConfirmConditionNotification = Lens.field @"manifestConfirmConditionNotification"
{-# DEPRECATED esManifestConfirmConditionNotification "Use generic-lens or generic-optics with 'manifestConfirmConditionNotification' instead." #-}

-- | Specifies the stream distance, in milliseconds, between the SCTE 35 messages that the transcoder places and the splice points that they refer to. If the time between the start of the asset and the SCTE-35 message is less than this value, then the transcoder places the SCTE-35 marker at the beginning of the stream.
--
-- /Note:/ Consider using 'responseSignalPreroll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esResponseSignalPreroll :: Lens.Lens' EsamSettings (Core.Maybe Core.Natural)
esResponseSignalPreroll = Lens.field @"responseSignalPreroll"
{-# DEPRECATED esResponseSignalPreroll "Use generic-lens or generic-optics with 'responseSignalPreroll' instead." #-}

-- | Specifies an ESAM SignalProcessingNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing instructions that you provide in the setting SCC XML (sccXml).
--
-- /Note:/ Consider using 'signalProcessingNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSignalProcessingNotification :: Lens.Lens' EsamSettings (Core.Maybe Types.EsamSignalProcessingNotification)
esSignalProcessingNotification = Lens.field @"signalProcessingNotification"
{-# DEPRECATED esSignalProcessingNotification "Use generic-lens or generic-optics with 'signalProcessingNotification' instead." #-}

instance Core.FromJSON EsamSettings where
  toJSON EsamSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("manifestConfirmConditionNotification" Core..=)
              Core.<$> manifestConfirmConditionNotification,
            ("responseSignalPreroll" Core..=) Core.<$> responseSignalPreroll,
            ("signalProcessingNotification" Core..=)
              Core.<$> signalProcessingNotification
          ]
      )

instance Core.FromJSON EsamSettings where
  parseJSON =
    Core.withObject "EsamSettings" Core.$
      \x ->
        EsamSettings'
          Core.<$> (x Core..:? "manifestConfirmConditionNotification")
          Core.<*> (x Core..:? "responseSignalPreroll")
          Core.<*> (x Core..:? "signalProcessingNotification")
