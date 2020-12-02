{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Audio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Audio where

import Network.AWS.AlexaBusiness.Types.Locale
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The audio message. There is a 1 MB limit on the audio file input and the only supported format is MP3. To convert your MP3 audio files to an Alexa-friendly,
--
--
-- required codec version (MPEG version 2) and bit rate (48 kbps), you might use converter software. One option for this is a command-line tool, FFmpeg. For more information, see <https://www.ffmpeg.org/ FFmpeg> . The following command converts the provided <input-file> to an MP3 file that is played in the announcement:
--
-- @ffmpeg -i <input-file> -ac 2 -codec:a libmp3lame -b:a 48k -ar 16000 <output-file.mp3>@
--
--
-- /See:/ 'audio' smart constructor.
data Audio = Audio' {_aLocale :: !Locale, _aLocation :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Audio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aLocale' - The locale of the audio message. Currently, en-US is supported.
--
-- * 'aLocation' - The location of the audio file. Currently, S3 URLs are supported. Only S3 locations comprised of safe characters are valid. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters> .
audio ::
  -- | 'aLocale'
  Locale ->
  -- | 'aLocation'
  Text ->
  Audio
audio pLocale_ pLocation_ =
  Audio' {_aLocale = pLocale_, _aLocation = pLocation_}

-- | The locale of the audio message. Currently, en-US is supported.
aLocale :: Lens' Audio Locale
aLocale = lens _aLocale (\s a -> s {_aLocale = a})

-- | The location of the audio file. Currently, S3 URLs are supported. Only S3 locations comprised of safe characters are valid. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters> .
aLocation :: Lens' Audio Text
aLocation = lens _aLocation (\s a -> s {_aLocation = a})

instance Hashable Audio

instance NFData Audio

instance ToJSON Audio where
  toJSON Audio' {..} =
    object
      ( catMaybes
          [Just ("Locale" .= _aLocale), Just ("Location" .= _aLocation)]
      )
