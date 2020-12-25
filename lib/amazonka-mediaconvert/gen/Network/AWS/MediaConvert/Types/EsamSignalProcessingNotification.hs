{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification
  ( EsamSignalProcessingNotification (..),

    -- * Smart constructor
    mkEsamSignalProcessingNotification,

    -- * Lenses
    espnSccXml,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | ESAM SignalProcessingNotification data defined by OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'mkEsamSignalProcessingNotification' smart constructor.
newtype EsamSignalProcessingNotification = EsamSignalProcessingNotification'
  { -- | Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing instructions in the message that you supply. Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. For your MPEG2-TS file outputs, if you want the service to place SCTE-35 markers at the insertion points you specify in the XML document, you must also enable SCTE-35 ESAM (scte35Esam). Note that you can either specify an ESAM XML document or enable SCTE-35 passthrough. You can't do both.
    sccXml :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EsamSignalProcessingNotification' value with any optional fields omitted.
mkEsamSignalProcessingNotification ::
  EsamSignalProcessingNotification
mkEsamSignalProcessingNotification =
  EsamSignalProcessingNotification' {sccXml = Core.Nothing}

-- | Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing instructions in the message that you supply. Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. For your MPEG2-TS file outputs, if you want the service to place SCTE-35 markers at the insertion points you specify in the XML document, you must also enable SCTE-35 ESAM (scte35Esam). Note that you can either specify an ESAM XML document or enable SCTE-35 passthrough. You can't do both.
--
-- /Note:/ Consider using 'sccXml' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
espnSccXml :: Lens.Lens' EsamSignalProcessingNotification (Core.Maybe Core.Text)
espnSccXml = Lens.field @"sccXml"
{-# DEPRECATED espnSccXml "Use generic-lens or generic-optics with 'sccXml' instead." #-}

instance Core.FromJSON EsamSignalProcessingNotification where
  toJSON EsamSignalProcessingNotification {..} =
    Core.object (Core.catMaybes [("sccXml" Core..=) Core.<$> sccXml])

instance Core.FromJSON EsamSignalProcessingNotification where
  parseJSON =
    Core.withObject "EsamSignalProcessingNotification" Core.$
      \x ->
        EsamSignalProcessingNotification' Core.<$> (x Core..:? "sccXml")
