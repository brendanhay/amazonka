{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CaptionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CaptionFormat where

import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The file format of the output captions. If you leave this value blank, Elastic Transcoder returns an error.
--
--
--
-- /See:/ 'captionFormat' smart constructor.
data CaptionFormat = CaptionFormat'
  { _cfPattern :: !(Maybe Text),
    _cfFormat :: !(Maybe Text),
    _cfEncryption :: !(Maybe Encryption)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfPattern' - The prefix for caption filenames, in the form /description/ -@{language}@ , where:     * /description/ is a description of the video.     * @{language}@ is a literal value that Elastic Transcoder replaces with the two- or three-letter code for the language of the caption in the output file names. If you don't include @{language}@ in the file name pattern, Elastic Transcoder automatically appends "@{language}@ " to the value that you specify for the description. In addition, Elastic Transcoder automatically appends the count to the end of the segment files. For example, suppose you're transcoding into srt format. When you enter "Sydney-{language}-sunrise", and the language of the captions is English (en), the name of the first caption file is be Sydney-en-sunrise00000.srt.
--
-- * 'cfFormat' - The format you specify determines whether Elastic Transcoder generates an embedded or sidecar caption for this output.     * __Valid Embedded Caption Formats:__      * __for FLAC__ : None     * __For MP3__ : None     * __For MP4__ : mov-text     * __For MPEG-TS__ : None     * __For ogg__ : None     * __For webm__ : None     * __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp (first div element only), scc, srt, and webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.     * __For FMP4__ : dfxp     * __Non-FMP4 outputs__ : All sidecar types @fmp4@ captions have an extension of @.ismt@
--
-- * 'cfEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your caption formats.
captionFormat ::
  CaptionFormat
captionFormat =
  CaptionFormat'
    { _cfPattern = Nothing,
      _cfFormat = Nothing,
      _cfEncryption = Nothing
    }

-- | The prefix for caption filenames, in the form /description/ -@{language}@ , where:     * /description/ is a description of the video.     * @{language}@ is a literal value that Elastic Transcoder replaces with the two- or three-letter code for the language of the caption in the output file names. If you don't include @{language}@ in the file name pattern, Elastic Transcoder automatically appends "@{language}@ " to the value that you specify for the description. In addition, Elastic Transcoder automatically appends the count to the end of the segment files. For example, suppose you're transcoding into srt format. When you enter "Sydney-{language}-sunrise", and the language of the captions is English (en), the name of the first caption file is be Sydney-en-sunrise00000.srt.
cfPattern :: Lens' CaptionFormat (Maybe Text)
cfPattern = lens _cfPattern (\s a -> s {_cfPattern = a})

-- | The format you specify determines whether Elastic Transcoder generates an embedded or sidecar caption for this output.     * __Valid Embedded Caption Formats:__      * __for FLAC__ : None     * __For MP3__ : None     * __For MP4__ : mov-text     * __For MPEG-TS__ : None     * __For ogg__ : None     * __For webm__ : None     * __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp (first div element only), scc, srt, and webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.     * __For FMP4__ : dfxp     * __Non-FMP4 outputs__ : All sidecar types @fmp4@ captions have an extension of @.ismt@
cfFormat :: Lens' CaptionFormat (Maybe Text)
cfFormat = lens _cfFormat (\s a -> s {_cfFormat = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your caption formats.
cfEncryption :: Lens' CaptionFormat (Maybe Encryption)
cfEncryption = lens _cfEncryption (\s a -> s {_cfEncryption = a})

instance FromJSON CaptionFormat where
  parseJSON =
    withObject
      "CaptionFormat"
      ( \x ->
          CaptionFormat'
            <$> (x .:? "Pattern") <*> (x .:? "Format") <*> (x .:? "Encryption")
      )

instance Hashable CaptionFormat

instance NFData CaptionFormat

instance ToJSON CaptionFormat where
  toJSON CaptionFormat' {..} =
    object
      ( catMaybes
          [ ("Pattern" .=) <$> _cfPattern,
            ("Format" .=) <$> _cfFormat,
            ("Encryption" .=) <$> _cfEncryption
          ]
      )
