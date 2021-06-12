{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EsamSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EsamSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
import Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification

-- | Settings for Event Signaling And Messaging (ESAM). If you don\'t do ad
-- insertion, you can ignore these settings.
--
-- /See:/ 'newEsamSettings' smart constructor.
data EsamSettings = EsamSettings'
  { -- | Specifies the stream distance, in milliseconds, between the SCTE 35
    -- messages that the transcoder places and the splice points that they
    -- refer to. If the time between the start of the asset and the SCTE-35
    -- message is less than this value, then the transcoder places the SCTE-35
    -- marker at the beginning of the stream.
    responseSignalPreroll :: Core.Maybe Core.Natural,
    -- | Specifies an ESAM ManifestConfirmConditionNotification XML as per
    -- OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning
    -- instructions that you provide in the setting MCC XML (mccXml).
    manifestConfirmConditionNotification :: Core.Maybe EsamManifestConfirmConditionNotification,
    -- | Specifies an ESAM SignalProcessingNotification XML as per
    -- OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing
    -- instructions that you provide in the setting SCC XML (sccXml).
    signalProcessingNotification :: Core.Maybe EsamSignalProcessingNotification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EsamSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseSignalPreroll', 'esamSettings_responseSignalPreroll' - Specifies the stream distance, in milliseconds, between the SCTE 35
-- messages that the transcoder places and the splice points that they
-- refer to. If the time between the start of the asset and the SCTE-35
-- message is less than this value, then the transcoder places the SCTE-35
-- marker at the beginning of the stream.
--
-- 'manifestConfirmConditionNotification', 'esamSettings_manifestConfirmConditionNotification' - Specifies an ESAM ManifestConfirmConditionNotification XML as per
-- OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning
-- instructions that you provide in the setting MCC XML (mccXml).
--
-- 'signalProcessingNotification', 'esamSettings_signalProcessingNotification' - Specifies an ESAM SignalProcessingNotification XML as per
-- OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing
-- instructions that you provide in the setting SCC XML (sccXml).
newEsamSettings ::
  EsamSettings
newEsamSettings =
  EsamSettings'
    { responseSignalPreroll = Core.Nothing,
      manifestConfirmConditionNotification = Core.Nothing,
      signalProcessingNotification = Core.Nothing
    }

-- | Specifies the stream distance, in milliseconds, between the SCTE 35
-- messages that the transcoder places and the splice points that they
-- refer to. If the time between the start of the asset and the SCTE-35
-- message is less than this value, then the transcoder places the SCTE-35
-- marker at the beginning of the stream.
esamSettings_responseSignalPreroll :: Lens.Lens' EsamSettings (Core.Maybe Core.Natural)
esamSettings_responseSignalPreroll = Lens.lens (\EsamSettings' {responseSignalPreroll} -> responseSignalPreroll) (\s@EsamSettings' {} a -> s {responseSignalPreroll = a} :: EsamSettings)

-- | Specifies an ESAM ManifestConfirmConditionNotification XML as per
-- OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning
-- instructions that you provide in the setting MCC XML (mccXml).
esamSettings_manifestConfirmConditionNotification :: Lens.Lens' EsamSettings (Core.Maybe EsamManifestConfirmConditionNotification)
esamSettings_manifestConfirmConditionNotification = Lens.lens (\EsamSettings' {manifestConfirmConditionNotification} -> manifestConfirmConditionNotification) (\s@EsamSettings' {} a -> s {manifestConfirmConditionNotification = a} :: EsamSettings)

-- | Specifies an ESAM SignalProcessingNotification XML as per
-- OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing
-- instructions that you provide in the setting SCC XML (sccXml).
esamSettings_signalProcessingNotification :: Lens.Lens' EsamSettings (Core.Maybe EsamSignalProcessingNotification)
esamSettings_signalProcessingNotification = Lens.lens (\EsamSettings' {signalProcessingNotification} -> signalProcessingNotification) (\s@EsamSettings' {} a -> s {signalProcessingNotification = a} :: EsamSettings)

instance Core.FromJSON EsamSettings where
  parseJSON =
    Core.withObject
      "EsamSettings"
      ( \x ->
          EsamSettings'
            Core.<$> (x Core..:? "responseSignalPreroll")
            Core.<*> (x Core..:? "manifestConfirmConditionNotification")
            Core.<*> (x Core..:? "signalProcessingNotification")
      )

instance Core.Hashable EsamSettings

instance Core.NFData EsamSettings

instance Core.ToJSON EsamSettings where
  toJSON EsamSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("responseSignalPreroll" Core..=)
              Core.<$> responseSignalPreroll,
            ("manifestConfirmConditionNotification" Core..=)
              Core.<$> manifestConfirmConditionNotification,
            ("signalProcessingNotification" Core..=)
              Core.<$> signalProcessingNotification
          ]
      )
