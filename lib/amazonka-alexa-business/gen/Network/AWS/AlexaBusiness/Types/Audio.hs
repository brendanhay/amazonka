-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Audio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Audio
  ( Audio (..),

    -- * Smart constructor
    mkAudio,

    -- * Lenses
    aLocale,
    aLocation,
  )
where

import Network.AWS.AlexaBusiness.Types.Locale
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The audio message. There is a 1 MB limit on the audio file input and the only supported format is MP3. To convert your MP3 audio files to an Alexa-friendly,
--
-- required codec version (MPEG version 2) and bit rate (48 kbps), you might use converter software. One option for this is a command-line tool, FFmpeg. For more information, see <https://www.ffmpeg.org/ FFmpeg> . The following command converts the provided <input-file> to an MP3 file that is played in the announcement:
-- @ffmpeg -i <input-file> -ac 2 -codec:a libmp3lame -b:a 48k -ar 16000 <output-file.mp3>@
--
-- /See:/ 'mkAudio' smart constructor.
data Audio = Audio' {locale :: Locale, location :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Audio' with the minimum fields required to make a request.
--
-- * 'locale' - The locale of the audio message. Currently, en-US is supported.
-- * 'location' - The location of the audio file. Currently, S3 URLs are supported. Only S3 locations comprised of safe characters are valid. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters> .
mkAudio ::
  -- | 'locale'
  Locale ->
  -- | 'location'
  Lude.Text ->
  Audio
mkAudio pLocale_ pLocation_ =
  Audio' {locale = pLocale_, location = pLocation_}

-- | The locale of the audio message. Currently, en-US is supported.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLocale :: Lens.Lens' Audio Locale
aLocale = Lens.lens (locale :: Audio -> Locale) (\s a -> s {locale = a} :: Audio)
{-# DEPRECATED aLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The location of the audio file. Currently, S3 URLs are supported. Only S3 locations comprised of safe characters are valid. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters> .
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLocation :: Lens.Lens' Audio Lude.Text
aLocation = Lens.lens (location :: Audio -> Lude.Text) (\s a -> s {location = a} :: Audio)
{-# DEPRECATED aLocation "Use generic-lens or generic-optics with 'location' instead." #-}

instance Lude.ToJSON Audio where
  toJSON Audio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Locale" Lude..= locale),
            Lude.Just ("Location" Lude..= location)
          ]
      )
