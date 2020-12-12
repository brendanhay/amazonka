{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Id3Insertion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Id3Insertion
  ( Id3Insertion (..),

    -- * Smart constructor
    mkId3Insertion,

    -- * Lenses
    iiId3,
    iiTimecode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3) to specify the base 64 encoded string and use Timecode (TimeCode) to specify the time when the tag should be inserted. To insert multiple ID3 tags in your output, create multiple instances of ID3 insertion (Id3Insertion).
--
-- /See:/ 'mkId3Insertion' smart constructor.
data Id3Insertion = Id3Insertion'
  { id3 :: Lude.Maybe Lude.Text,
    timecode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Id3Insertion' with the minimum fields required to make a request.
--
-- * 'id3' - Use ID3 tag (Id3) to provide a tag value in base64-encode format.
-- * 'timecode' - Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
mkId3Insertion ::
  Id3Insertion
mkId3Insertion =
  Id3Insertion' {id3 = Lude.Nothing, timecode = Lude.Nothing}

-- | Use ID3 tag (Id3) to provide a tag value in base64-encode format.
--
-- /Note:/ Consider using 'id3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiId3 :: Lens.Lens' Id3Insertion (Lude.Maybe Lude.Text)
iiId3 = Lens.lens (id3 :: Id3Insertion -> Lude.Maybe Lude.Text) (\s a -> s {id3 = a} :: Id3Insertion)
{-# DEPRECATED iiId3 "Use generic-lens or generic-optics with 'id3' instead." #-}

-- | Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
--
-- /Note:/ Consider using 'timecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiTimecode :: Lens.Lens' Id3Insertion (Lude.Maybe Lude.Text)
iiTimecode = Lens.lens (timecode :: Id3Insertion -> Lude.Maybe Lude.Text) (\s a -> s {timecode = a} :: Id3Insertion)
{-# DEPRECATED iiTimecode "Use generic-lens or generic-optics with 'timecode' instead." #-}

instance Lude.FromJSON Id3Insertion where
  parseJSON =
    Lude.withObject
      "Id3Insertion"
      ( \x ->
          Id3Insertion'
            Lude.<$> (x Lude..:? "id3") Lude.<*> (x Lude..:? "timecode")
      )

instance Lude.ToJSON Id3Insertion where
  toJSON Id3Insertion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("id3" Lude..=) Lude.<$> id3,
            ("timecode" Lude..=) Lude.<$> timecode
          ]
      )
