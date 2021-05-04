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
-- Module      : Network.AWS.MediaConvert.Types.MovSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.MovClapAtom
import Network.AWS.MediaConvert.Types.MovCslgAtom
import Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
import Network.AWS.MediaConvert.Types.MovPaddingControl
import Network.AWS.MediaConvert.Types.MovReference
import qualified Network.AWS.Prelude as Prelude

-- | Settings for MOV Container.
--
-- /See:/ 'newMovSettings' smart constructor.
data MovSettings = MovSettings'
  { -- | To make this output compatible with Omenon, keep the default value,
    -- OMNEON. Unless you need Omneon compatibility, set this value to NONE.
    -- When you keep the default value, OMNEON, MediaConvert increases the
    -- length of the edit list atom. This might cause file rejections when a
    -- recipient of the output file doesn\'t expct this extra padding.
    paddingControl :: Prelude.Maybe MovPaddingControl,
    -- | When enabled, file composition times will start at zero, composition
    -- times in the \'ctts\' (composition time to sample) box for B-frames will
    -- be negative, and a \'cslg\' (composition shift least greatest) box will
    -- be included per 14496-1 amendment 1. This improves compatibility with
    -- Apple players and tools.
    cslgAtom :: Prelude.Maybe MovCslgAtom,
    -- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file
    -- using XDCAM fourcc codes. This increases compatibility with Apple
    -- editors and players, but may decrease compatibility with other players.
    -- Only applicable when the video codec is MPEG2.
    mpeg2FourCCControl :: Prelude.Maybe MovMpeg2FourCCControl,
    -- | When enabled, include \'clap\' atom if appropriate for the video output
    -- settings.
    clapAtom :: Prelude.Maybe MovClapAtom,
    -- | Always keep the default value (SELF_CONTAINED) for this setting.
    reference :: Prelude.Maybe MovReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MovSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paddingControl', 'movSettings_paddingControl' - To make this output compatible with Omenon, keep the default value,
-- OMNEON. Unless you need Omneon compatibility, set this value to NONE.
-- When you keep the default value, OMNEON, MediaConvert increases the
-- length of the edit list atom. This might cause file rejections when a
-- recipient of the output file doesn\'t expct this extra padding.
--
-- 'cslgAtom', 'movSettings_cslgAtom' - When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
--
-- 'mpeg2FourCCControl', 'movSettings_mpeg2FourCCControl' - When set to XDCAM, writes MPEG2 video streams into the QuickTime file
-- using XDCAM fourcc codes. This increases compatibility with Apple
-- editors and players, but may decrease compatibility with other players.
-- Only applicable when the video codec is MPEG2.
--
-- 'clapAtom', 'movSettings_clapAtom' - When enabled, include \'clap\' atom if appropriate for the video output
-- settings.
--
-- 'reference', 'movSettings_reference' - Always keep the default value (SELF_CONTAINED) for this setting.
newMovSettings ::
  MovSettings
newMovSettings =
  MovSettings'
    { paddingControl = Prelude.Nothing,
      cslgAtom = Prelude.Nothing,
      mpeg2FourCCControl = Prelude.Nothing,
      clapAtom = Prelude.Nothing,
      reference = Prelude.Nothing
    }

-- | To make this output compatible with Omenon, keep the default value,
-- OMNEON. Unless you need Omneon compatibility, set this value to NONE.
-- When you keep the default value, OMNEON, MediaConvert increases the
-- length of the edit list atom. This might cause file rejections when a
-- recipient of the output file doesn\'t expct this extra padding.
movSettings_paddingControl :: Lens.Lens' MovSettings (Prelude.Maybe MovPaddingControl)
movSettings_paddingControl = Lens.lens (\MovSettings' {paddingControl} -> paddingControl) (\s@MovSettings' {} a -> s {paddingControl = a} :: MovSettings)

-- | When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
movSettings_cslgAtom :: Lens.Lens' MovSettings (Prelude.Maybe MovCslgAtom)
movSettings_cslgAtom = Lens.lens (\MovSettings' {cslgAtom} -> cslgAtom) (\s@MovSettings' {} a -> s {cslgAtom = a} :: MovSettings)

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file
-- using XDCAM fourcc codes. This increases compatibility with Apple
-- editors and players, but may decrease compatibility with other players.
-- Only applicable when the video codec is MPEG2.
movSettings_mpeg2FourCCControl :: Lens.Lens' MovSettings (Prelude.Maybe MovMpeg2FourCCControl)
movSettings_mpeg2FourCCControl = Lens.lens (\MovSettings' {mpeg2FourCCControl} -> mpeg2FourCCControl) (\s@MovSettings' {} a -> s {mpeg2FourCCControl = a} :: MovSettings)

-- | When enabled, include \'clap\' atom if appropriate for the video output
-- settings.
movSettings_clapAtom :: Lens.Lens' MovSettings (Prelude.Maybe MovClapAtom)
movSettings_clapAtom = Lens.lens (\MovSettings' {clapAtom} -> clapAtom) (\s@MovSettings' {} a -> s {clapAtom = a} :: MovSettings)

-- | Always keep the default value (SELF_CONTAINED) for this setting.
movSettings_reference :: Lens.Lens' MovSettings (Prelude.Maybe MovReference)
movSettings_reference = Lens.lens (\MovSettings' {reference} -> reference) (\s@MovSettings' {} a -> s {reference = a} :: MovSettings)

instance Prelude.FromJSON MovSettings where
  parseJSON =
    Prelude.withObject
      "MovSettings"
      ( \x ->
          MovSettings'
            Prelude.<$> (x Prelude..:? "paddingControl")
            Prelude.<*> (x Prelude..:? "cslgAtom")
            Prelude.<*> (x Prelude..:? "mpeg2FourCCControl")
            Prelude.<*> (x Prelude..:? "clapAtom")
            Prelude.<*> (x Prelude..:? "reference")
      )

instance Prelude.Hashable MovSettings

instance Prelude.NFData MovSettings

instance Prelude.ToJSON MovSettings where
  toJSON MovSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("paddingControl" Prelude..=)
              Prelude.<$> paddingControl,
            ("cslgAtom" Prelude..=) Prelude.<$> cslgAtom,
            ("mpeg2FourCCControl" Prelude..=)
              Prelude.<$> mpeg2FourCCControl,
            ("clapAtom" Prelude..=) Prelude.<$> clapAtom,
            ("reference" Prelude..=) Prelude.<$> reference
          ]
      )
