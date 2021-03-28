{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Audio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Audio
  ( Audio (..)
  -- * Smart constructor
  , mkAudio
  -- * Lenses
  , aLocale
  , aLocation
  ) where

import qualified Network.AWS.AlexaBusiness.Types.AudioLocation as Types
import qualified Network.AWS.AlexaBusiness.Types.Locale as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The audio message. There is a 1 MB limit on the audio file input and the only supported format is MP3. To convert your MP3 audio files to an Alexa-friendly, 
--
-- required codec version (MPEG version 2) and bit rate (48 kbps), you might use converter software. One option for this is a command-line tool, FFmpeg. For more information, see <https://www.ffmpeg.org/ FFmpeg> . The following command converts the provided <input-file> to an MP3 file that is played in the announcement:
-- @ffmpeg -i <input-file> -ac 2 -codec:a libmp3lame -b:a 48k -ar 16000 <output-file.mp3>@ 
--
-- /See:/ 'mkAudio' smart constructor.
data Audio = Audio'
  { locale :: Types.Locale
    -- ^ The locale of the audio message. Currently, en-US is supported.
  , location :: Types.AudioLocation
    -- ^ The location of the audio file. Currently, S3 URLs are supported. Only S3 locations comprised of safe characters are valid. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Audio' value with any optional fields omitted.
mkAudio
    :: Types.Locale -- ^ 'locale'
    -> Types.AudioLocation -- ^ 'location'
    -> Audio
mkAudio locale location = Audio'{locale, location}

-- | The locale of the audio message. Currently, en-US is supported.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLocale :: Lens.Lens' Audio Types.Locale
aLocale = Lens.field @"locale"
{-# INLINEABLE aLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The location of the audio file. Currently, S3 URLs are supported. Only S3 locations comprised of safe characters are valid. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters> .
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLocation :: Lens.Lens' Audio Types.AudioLocation
aLocation = Lens.field @"location"
{-# INLINEABLE aLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

instance Core.FromJSON Audio where
        toJSON Audio{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Locale" Core..= locale),
                  Core.Just ("Location" Core..= location)])
