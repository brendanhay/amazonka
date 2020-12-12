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
    espnSccXML,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | ESAM SignalProcessingNotification data defined by OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'mkEsamSignalProcessingNotification' smart constructor.
newtype EsamSignalProcessingNotification = EsamSignalProcessingNotification'
  { sccXML ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EsamSignalProcessingNotification' with the minimum fields required to make a request.
--
-- * 'sccXML' - Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing instructions in the message that you supply. Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. For your MPEG2-TS file outputs, if you want the service to place SCTE-35 markers at the insertion points you specify in the XML document, you must also enable SCTE-35 ESAM (scte35Esam). Note that you can either specify an ESAM XML document or enable SCTE-35 passthrough. You can't do both.
mkEsamSignalProcessingNotification ::
  EsamSignalProcessingNotification
mkEsamSignalProcessingNotification =
  EsamSignalProcessingNotification' {sccXML = Lude.Nothing}

-- | Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing instructions in the message that you supply. Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. For your MPEG2-TS file outputs, if you want the service to place SCTE-35 markers at the insertion points you specify in the XML document, you must also enable SCTE-35 ESAM (scte35Esam). Note that you can either specify an ESAM XML document or enable SCTE-35 passthrough. You can't do both.
--
-- /Note:/ Consider using 'sccXML' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
espnSccXML :: Lens.Lens' EsamSignalProcessingNotification (Lude.Maybe Lude.Text)
espnSccXML = Lens.lens (sccXML :: EsamSignalProcessingNotification -> Lude.Maybe Lude.Text) (\s a -> s {sccXML = a} :: EsamSignalProcessingNotification)
{-# DEPRECATED espnSccXML "Use generic-lens or generic-optics with 'sccXML' instead." #-}

instance Lude.FromJSON EsamSignalProcessingNotification where
  parseJSON =
    Lude.withObject
      "EsamSignalProcessingNotification"
      ( \x ->
          EsamSignalProcessingNotification' Lude.<$> (x Lude..:? "sccXml")
      )

instance Lude.ToJSON EsamSignalProcessingNotification where
  toJSON EsamSignalProcessingNotification' {..} =
    Lude.object (Lude.catMaybes [("sccXml" Lude..=) Lude.<$> sccXML])
