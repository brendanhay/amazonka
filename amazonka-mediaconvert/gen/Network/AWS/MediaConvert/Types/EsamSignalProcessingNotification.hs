{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | ESAM SignalProcessingNotification data defined by
-- OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'newEsamSignalProcessingNotification' smart constructor.
data EsamSignalProcessingNotification = EsamSignalProcessingNotification'
  { -- | Provide your ESAM SignalProcessingNotification XML document inside your
    -- JSON job settings. Form the XML document as per
    -- OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing
    -- instructions in the message that you supply. Provide your ESAM
    -- SignalProcessingNotification XML document inside your JSON job settings.
    -- For your MPEG2-TS file outputs, if you want the service to place SCTE-35
    -- markers at the insertion points you specify in the XML document, you
    -- must also enable SCTE-35 ESAM (scte35Esam). Note that you can either
    -- specify an ESAM XML document or enable SCTE-35 passthrough. You can\'t
    -- do both.
    sccXml :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EsamSignalProcessingNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sccXml', 'esamSignalProcessingNotification_sccXml' - Provide your ESAM SignalProcessingNotification XML document inside your
-- JSON job settings. Form the XML document as per
-- OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing
-- instructions in the message that you supply. Provide your ESAM
-- SignalProcessingNotification XML document inside your JSON job settings.
-- For your MPEG2-TS file outputs, if you want the service to place SCTE-35
-- markers at the insertion points you specify in the XML document, you
-- must also enable SCTE-35 ESAM (scte35Esam). Note that you can either
-- specify an ESAM XML document or enable SCTE-35 passthrough. You can\'t
-- do both.
newEsamSignalProcessingNotification ::
  EsamSignalProcessingNotification
newEsamSignalProcessingNotification =
  EsamSignalProcessingNotification'
    { sccXml =
        Prelude.Nothing
    }

-- | Provide your ESAM SignalProcessingNotification XML document inside your
-- JSON job settings. Form the XML document as per
-- OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing
-- instructions in the message that you supply. Provide your ESAM
-- SignalProcessingNotification XML document inside your JSON job settings.
-- For your MPEG2-TS file outputs, if you want the service to place SCTE-35
-- markers at the insertion points you specify in the XML document, you
-- must also enable SCTE-35 ESAM (scte35Esam). Note that you can either
-- specify an ESAM XML document or enable SCTE-35 passthrough. You can\'t
-- do both.
esamSignalProcessingNotification_sccXml :: Lens.Lens' EsamSignalProcessingNotification (Prelude.Maybe Prelude.Text)
esamSignalProcessingNotification_sccXml = Lens.lens (\EsamSignalProcessingNotification' {sccXml} -> sccXml) (\s@EsamSignalProcessingNotification' {} a -> s {sccXml = a} :: EsamSignalProcessingNotification)

instance
  Prelude.FromJSON
    EsamSignalProcessingNotification
  where
  parseJSON =
    Prelude.withObject
      "EsamSignalProcessingNotification"
      ( \x ->
          EsamSignalProcessingNotification'
            Prelude.<$> (x Prelude..:? "sccXml")
      )

instance
  Prelude.Hashable
    EsamSignalProcessingNotification

instance
  Prelude.NFData
    EsamSignalProcessingNotification

instance
  Prelude.ToJSON
    EsamSignalProcessingNotification
  where
  toJSON EsamSignalProcessingNotification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("sccXml" Prelude..=) Prelude.<$> sccXml]
      )
