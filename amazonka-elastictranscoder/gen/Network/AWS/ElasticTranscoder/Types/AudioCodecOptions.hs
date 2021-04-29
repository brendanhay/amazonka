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
-- Module      : Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.AudioCodecOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options associated with your audio codec.
--
-- /See:/ 'newAudioCodecOptions' smart constructor.
data AudioCodecOptions = AudioCodecOptions'
  { -- | You can only choose an audio bit depth when you specify @flac@ or @pcm@
    -- for the value of Audio:Codec.
    --
    -- The bit depth of a sample is how many bits of information are included
    -- in the audio samples. The higher the bit depth, the better the audio,
    -- but the larger the file.
    --
    -- Valid values are @16@ and @24@.
    --
    -- The most common bit depth is @24@.
    bitDepth :: Prelude.Maybe Prelude.Text,
    -- | You can only choose whether an audio sample is signed when you specify
    -- @pcm@ for the value of Audio:Codec.
    --
    -- Whether audio samples are represented with negative and positive numbers
    -- (signed) or only positive numbers (unsigned).
    --
    -- The supported value is @Signed@.
    signed :: Prelude.Maybe Prelude.Text,
    -- | You can only choose an audio bit order when you specify @pcm@ for the
    -- value of Audio:Codec.
    --
    -- The order the bits of a PCM sample are stored in.
    --
    -- The supported value is @LittleEndian@.
    bitOrder :: Prelude.Maybe Prelude.Text,
    -- | You can only choose an audio profile when you specify AAC for the value
    -- of Audio:Codec.
    --
    -- Specify the AAC profile for the output file. Elastic Transcoder supports
    -- the following profiles:
    --
    -- -   @auto@: If you specify @auto@, Elastic Transcoder selects the
    --     profile based on the bit rate selected for the output file.
    --
    -- -   @AAC-LC@: The most common AAC profile. Use for bit rates larger than
    --     64 kbps.
    --
    -- -   @HE-AAC@: Not supported on some older players and devices. Use for
    --     bit rates between 40 and 80 kbps.
    --
    -- -   @HE-AACv2@: Not supported on some players and devices. Use for bit
    --     rates less than 48 kbps.
    --
    -- All outputs in a @Smooth@ playlist must have the same value for
    -- @Profile@.
    --
    -- If you created any presets before AAC profiles were added, Elastic
    -- Transcoder automatically updated your presets to use AAC-LC. You can
    -- change the value as required.
    profile :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AudioCodecOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitDepth', 'audioCodecOptions_bitDepth' - You can only choose an audio bit depth when you specify @flac@ or @pcm@
-- for the value of Audio:Codec.
--
-- The bit depth of a sample is how many bits of information are included
-- in the audio samples. The higher the bit depth, the better the audio,
-- but the larger the file.
--
-- Valid values are @16@ and @24@.
--
-- The most common bit depth is @24@.
--
-- 'signed', 'audioCodecOptions_signed' - You can only choose whether an audio sample is signed when you specify
-- @pcm@ for the value of Audio:Codec.
--
-- Whether audio samples are represented with negative and positive numbers
-- (signed) or only positive numbers (unsigned).
--
-- The supported value is @Signed@.
--
-- 'bitOrder', 'audioCodecOptions_bitOrder' - You can only choose an audio bit order when you specify @pcm@ for the
-- value of Audio:Codec.
--
-- The order the bits of a PCM sample are stored in.
--
-- The supported value is @LittleEndian@.
--
-- 'profile', 'audioCodecOptions_profile' - You can only choose an audio profile when you specify AAC for the value
-- of Audio:Codec.
--
-- Specify the AAC profile for the output file. Elastic Transcoder supports
-- the following profiles:
--
-- -   @auto@: If you specify @auto@, Elastic Transcoder selects the
--     profile based on the bit rate selected for the output file.
--
-- -   @AAC-LC@: The most common AAC profile. Use for bit rates larger than
--     64 kbps.
--
-- -   @HE-AAC@: Not supported on some older players and devices. Use for
--     bit rates between 40 and 80 kbps.
--
-- -   @HE-AACv2@: Not supported on some players and devices. Use for bit
--     rates less than 48 kbps.
--
-- All outputs in a @Smooth@ playlist must have the same value for
-- @Profile@.
--
-- If you created any presets before AAC profiles were added, Elastic
-- Transcoder automatically updated your presets to use AAC-LC. You can
-- change the value as required.
newAudioCodecOptions ::
  AudioCodecOptions
newAudioCodecOptions =
  AudioCodecOptions'
    { bitDepth = Prelude.Nothing,
      signed = Prelude.Nothing,
      bitOrder = Prelude.Nothing,
      profile = Prelude.Nothing
    }

-- | You can only choose an audio bit depth when you specify @flac@ or @pcm@
-- for the value of Audio:Codec.
--
-- The bit depth of a sample is how many bits of information are included
-- in the audio samples. The higher the bit depth, the better the audio,
-- but the larger the file.
--
-- Valid values are @16@ and @24@.
--
-- The most common bit depth is @24@.
audioCodecOptions_bitDepth :: Lens.Lens' AudioCodecOptions (Prelude.Maybe Prelude.Text)
audioCodecOptions_bitDepth = Lens.lens (\AudioCodecOptions' {bitDepth} -> bitDepth) (\s@AudioCodecOptions' {} a -> s {bitDepth = a} :: AudioCodecOptions)

-- | You can only choose whether an audio sample is signed when you specify
-- @pcm@ for the value of Audio:Codec.
--
-- Whether audio samples are represented with negative and positive numbers
-- (signed) or only positive numbers (unsigned).
--
-- The supported value is @Signed@.
audioCodecOptions_signed :: Lens.Lens' AudioCodecOptions (Prelude.Maybe Prelude.Text)
audioCodecOptions_signed = Lens.lens (\AudioCodecOptions' {signed} -> signed) (\s@AudioCodecOptions' {} a -> s {signed = a} :: AudioCodecOptions)

-- | You can only choose an audio bit order when you specify @pcm@ for the
-- value of Audio:Codec.
--
-- The order the bits of a PCM sample are stored in.
--
-- The supported value is @LittleEndian@.
audioCodecOptions_bitOrder :: Lens.Lens' AudioCodecOptions (Prelude.Maybe Prelude.Text)
audioCodecOptions_bitOrder = Lens.lens (\AudioCodecOptions' {bitOrder} -> bitOrder) (\s@AudioCodecOptions' {} a -> s {bitOrder = a} :: AudioCodecOptions)

-- | You can only choose an audio profile when you specify AAC for the value
-- of Audio:Codec.
--
-- Specify the AAC profile for the output file. Elastic Transcoder supports
-- the following profiles:
--
-- -   @auto@: If you specify @auto@, Elastic Transcoder selects the
--     profile based on the bit rate selected for the output file.
--
-- -   @AAC-LC@: The most common AAC profile. Use for bit rates larger than
--     64 kbps.
--
-- -   @HE-AAC@: Not supported on some older players and devices. Use for
--     bit rates between 40 and 80 kbps.
--
-- -   @HE-AACv2@: Not supported on some players and devices. Use for bit
--     rates less than 48 kbps.
--
-- All outputs in a @Smooth@ playlist must have the same value for
-- @Profile@.
--
-- If you created any presets before AAC profiles were added, Elastic
-- Transcoder automatically updated your presets to use AAC-LC. You can
-- change the value as required.
audioCodecOptions_profile :: Lens.Lens' AudioCodecOptions (Prelude.Maybe Prelude.Text)
audioCodecOptions_profile = Lens.lens (\AudioCodecOptions' {profile} -> profile) (\s@AudioCodecOptions' {} a -> s {profile = a} :: AudioCodecOptions)

instance Prelude.FromJSON AudioCodecOptions where
  parseJSON =
    Prelude.withObject
      "AudioCodecOptions"
      ( \x ->
          AudioCodecOptions'
            Prelude.<$> (x Prelude..:? "BitDepth")
            Prelude.<*> (x Prelude..:? "Signed")
            Prelude.<*> (x Prelude..:? "BitOrder")
            Prelude.<*> (x Prelude..:? "Profile")
      )

instance Prelude.Hashable AudioCodecOptions

instance Prelude.NFData AudioCodecOptions

instance Prelude.ToJSON AudioCodecOptions where
  toJSON AudioCodecOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("BitDepth" Prelude..=) Prelude.<$> bitDepth,
            ("Signed" Prelude..=) Prelude.<$> signed,
            ("BitOrder" Prelude..=) Prelude.<$> bitOrder,
            ("Profile" Prelude..=) Prelude.<$> profile
          ]
      )
