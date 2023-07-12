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
-- Module      : Amazonka.MediaConnect.Types.Fmtp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Fmtp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Colorimetry
import Amazonka.MediaConnect.Types.Range
import Amazonka.MediaConnect.Types.ScanMode
import Amazonka.MediaConnect.Types.Tcs
import qualified Amazonka.Prelude as Prelude

-- | FMTP
--
-- /See:/ 'newFmtp' smart constructor.
data Fmtp = Fmtp'
  { -- | The format of the audio channel.
    channelOrder :: Prelude.Maybe Prelude.Text,
    -- | The format that is used for the representation of color.
    colorimetry :: Prelude.Maybe Colorimetry,
    -- | The frame rate for the video stream, in frames\/second. For example:
    -- 60000\/1001. If you specify a whole number, MediaConnect uses a ratio of
    -- N\/1. For example, if you specify 60, MediaConnect uses 60\/1 as the
    -- exactFramerate.
    exactFramerate :: Prelude.Maybe Prelude.Text,
    -- | The pixel aspect ratio (PAR) of the video.
    par :: Prelude.Maybe Prelude.Text,
    -- | The encoding range of the video.
    range :: Prelude.Maybe Range,
    -- | The type of compression that was used to smooth the video’s appearance
    scanMode :: Prelude.Maybe ScanMode,
    -- | The transfer characteristic system (TCS) that is used in the video.
    tcs :: Prelude.Maybe Tcs
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Fmtp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelOrder', 'fmtp_channelOrder' - The format of the audio channel.
--
-- 'colorimetry', 'fmtp_colorimetry' - The format that is used for the representation of color.
--
-- 'exactFramerate', 'fmtp_exactFramerate' - The frame rate for the video stream, in frames\/second. For example:
-- 60000\/1001. If you specify a whole number, MediaConnect uses a ratio of
-- N\/1. For example, if you specify 60, MediaConnect uses 60\/1 as the
-- exactFramerate.
--
-- 'par', 'fmtp_par' - The pixel aspect ratio (PAR) of the video.
--
-- 'range', 'fmtp_range' - The encoding range of the video.
--
-- 'scanMode', 'fmtp_scanMode' - The type of compression that was used to smooth the video’s appearance
--
-- 'tcs', 'fmtp_tcs' - The transfer characteristic system (TCS) that is used in the video.
newFmtp ::
  Fmtp
newFmtp =
  Fmtp'
    { channelOrder = Prelude.Nothing,
      colorimetry = Prelude.Nothing,
      exactFramerate = Prelude.Nothing,
      par = Prelude.Nothing,
      range = Prelude.Nothing,
      scanMode = Prelude.Nothing,
      tcs = Prelude.Nothing
    }

-- | The format of the audio channel.
fmtp_channelOrder :: Lens.Lens' Fmtp (Prelude.Maybe Prelude.Text)
fmtp_channelOrder = Lens.lens (\Fmtp' {channelOrder} -> channelOrder) (\s@Fmtp' {} a -> s {channelOrder = a} :: Fmtp)

-- | The format that is used for the representation of color.
fmtp_colorimetry :: Lens.Lens' Fmtp (Prelude.Maybe Colorimetry)
fmtp_colorimetry = Lens.lens (\Fmtp' {colorimetry} -> colorimetry) (\s@Fmtp' {} a -> s {colorimetry = a} :: Fmtp)

-- | The frame rate for the video stream, in frames\/second. For example:
-- 60000\/1001. If you specify a whole number, MediaConnect uses a ratio of
-- N\/1. For example, if you specify 60, MediaConnect uses 60\/1 as the
-- exactFramerate.
fmtp_exactFramerate :: Lens.Lens' Fmtp (Prelude.Maybe Prelude.Text)
fmtp_exactFramerate = Lens.lens (\Fmtp' {exactFramerate} -> exactFramerate) (\s@Fmtp' {} a -> s {exactFramerate = a} :: Fmtp)

-- | The pixel aspect ratio (PAR) of the video.
fmtp_par :: Lens.Lens' Fmtp (Prelude.Maybe Prelude.Text)
fmtp_par = Lens.lens (\Fmtp' {par} -> par) (\s@Fmtp' {} a -> s {par = a} :: Fmtp)

-- | The encoding range of the video.
fmtp_range :: Lens.Lens' Fmtp (Prelude.Maybe Range)
fmtp_range = Lens.lens (\Fmtp' {range} -> range) (\s@Fmtp' {} a -> s {range = a} :: Fmtp)

-- | The type of compression that was used to smooth the video’s appearance
fmtp_scanMode :: Lens.Lens' Fmtp (Prelude.Maybe ScanMode)
fmtp_scanMode = Lens.lens (\Fmtp' {scanMode} -> scanMode) (\s@Fmtp' {} a -> s {scanMode = a} :: Fmtp)

-- | The transfer characteristic system (TCS) that is used in the video.
fmtp_tcs :: Lens.Lens' Fmtp (Prelude.Maybe Tcs)
fmtp_tcs = Lens.lens (\Fmtp' {tcs} -> tcs) (\s@Fmtp' {} a -> s {tcs = a} :: Fmtp)

instance Data.FromJSON Fmtp where
  parseJSON =
    Data.withObject
      "Fmtp"
      ( \x ->
          Fmtp'
            Prelude.<$> (x Data..:? "channelOrder")
            Prelude.<*> (x Data..:? "colorimetry")
            Prelude.<*> (x Data..:? "exactFramerate")
            Prelude.<*> (x Data..:? "par")
            Prelude.<*> (x Data..:? "range")
            Prelude.<*> (x Data..:? "scanMode")
            Prelude.<*> (x Data..:? "tcs")
      )

instance Prelude.Hashable Fmtp where
  hashWithSalt _salt Fmtp' {..} =
    _salt
      `Prelude.hashWithSalt` channelOrder
      `Prelude.hashWithSalt` colorimetry
      `Prelude.hashWithSalt` exactFramerate
      `Prelude.hashWithSalt` par
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` scanMode
      `Prelude.hashWithSalt` tcs

instance Prelude.NFData Fmtp where
  rnf Fmtp' {..} =
    Prelude.rnf channelOrder
      `Prelude.seq` Prelude.rnf colorimetry
      `Prelude.seq` Prelude.rnf exactFramerate
      `Prelude.seq` Prelude.rnf par
      `Prelude.seq` Prelude.rnf range
      `Prelude.seq` Prelude.rnf scanMode
      `Prelude.seq` Prelude.rnf tcs
