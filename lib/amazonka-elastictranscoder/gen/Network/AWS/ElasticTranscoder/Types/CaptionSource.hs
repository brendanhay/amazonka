{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CaptionSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CaptionSource where

import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A source file for the input sidecar captions used during the transcoding process.
--
--
--
-- /See:/ 'captionSource' smart constructor.
data CaptionSource = CaptionSource'
  { _csTimeOffset :: !(Maybe Text),
    _csEncryption :: !(Maybe Encryption),
    _csKey :: !(Maybe Text),
    _csLanguage :: !(Maybe Text),
    _csLabel :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csTimeOffset' - For clip generation or captions that do not start at the same time as the associated video file, the @TimeOffset@ tells Elastic Transcoder how much of the video to encode before including captions. Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
--
-- * 'csEncryption' - The encryption settings, if any, that Elastic Transcoder needs to decyrpt your caption sources, or that you want Elastic Transcoder to apply to your caption sources.
--
-- * 'csKey' - The name of the sidecar caption file that you want Elastic Transcoder to include in the output file.
--
-- * 'csLanguage' - A string that specifies the language of the caption. If you specified multiple inputs with captions, the caption language must match in order to be included in the output. Specify this as one of:     * 2-character ISO 639-1 code     * 3-character ISO 639-2 code For more information on ISO language codes and language names, see the List of ISO 639-1 codes.
--
-- * 'csLabel' - The label of the caption shown in the player when choosing a language. We recommend that you put the caption language name here, in the language of the captions.
captionSource ::
  CaptionSource
captionSource =
  CaptionSource'
    { _csTimeOffset = Nothing,
      _csEncryption = Nothing,
      _csKey = Nothing,
      _csLanguage = Nothing,
      _csLabel = Nothing
    }

-- | For clip generation or captions that do not start at the same time as the associated video file, the @TimeOffset@ tells Elastic Transcoder how much of the video to encode before including captions. Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
csTimeOffset :: Lens' CaptionSource (Maybe Text)
csTimeOffset = lens _csTimeOffset (\s a -> s {_csTimeOffset = a})

-- | The encryption settings, if any, that Elastic Transcoder needs to decyrpt your caption sources, or that you want Elastic Transcoder to apply to your caption sources.
csEncryption :: Lens' CaptionSource (Maybe Encryption)
csEncryption = lens _csEncryption (\s a -> s {_csEncryption = a})

-- | The name of the sidecar caption file that you want Elastic Transcoder to include in the output file.
csKey :: Lens' CaptionSource (Maybe Text)
csKey = lens _csKey (\s a -> s {_csKey = a})

-- | A string that specifies the language of the caption. If you specified multiple inputs with captions, the caption language must match in order to be included in the output. Specify this as one of:     * 2-character ISO 639-1 code     * 3-character ISO 639-2 code For more information on ISO language codes and language names, see the List of ISO 639-1 codes.
csLanguage :: Lens' CaptionSource (Maybe Text)
csLanguage = lens _csLanguage (\s a -> s {_csLanguage = a})

-- | The label of the caption shown in the player when choosing a language. We recommend that you put the caption language name here, in the language of the captions.
csLabel :: Lens' CaptionSource (Maybe Text)
csLabel = lens _csLabel (\s a -> s {_csLabel = a})

instance FromJSON CaptionSource where
  parseJSON =
    withObject
      "CaptionSource"
      ( \x ->
          CaptionSource'
            <$> (x .:? "TimeOffset")
            <*> (x .:? "Encryption")
            <*> (x .:? "Key")
            <*> (x .:? "Language")
            <*> (x .:? "Label")
      )

instance Hashable CaptionSource

instance NFData CaptionSource

instance ToJSON CaptionSource where
  toJSON CaptionSource' {..} =
    object
      ( catMaybes
          [ ("TimeOffset" .=) <$> _csTimeOffset,
            ("Encryption" .=) <$> _csEncryption,
            ("Key" .=) <$> _csKey,
            ("Language" .=) <$> _csLanguage,
            ("Label" .=) <$> _csLabel
          ]
      )
