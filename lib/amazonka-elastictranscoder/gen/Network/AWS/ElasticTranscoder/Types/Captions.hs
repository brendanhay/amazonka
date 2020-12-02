{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Captions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Captions where

import Network.AWS.ElasticTranscoder.Types.CaptionFormat
import Network.AWS.ElasticTranscoder.Types.CaptionSource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The captions to be created, if any.
--
--
--
-- /See:/ 'captions' smart constructor.
data Captions = Captions'
  { _cMergePolicy :: !(Maybe Text),
    _cCaptionSources :: !(Maybe [CaptionSource]),
    _cCaptionFormats :: !(Maybe [CaptionFormat])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Captions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cMergePolicy' - A policy that determines how Elastic Transcoder handles the existence of multiple captions.     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ . @MergePolicy@ cannot be null.
--
-- * 'cCaptionSources' - Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
--
-- * 'cCaptionFormats' - The array of file formats for the output captions. If you leave this value blank, Elastic Transcoder returns an error.
captions ::
  Captions
captions =
  Captions'
    { _cMergePolicy = Nothing,
      _cCaptionSources = Nothing,
      _cCaptionFormats = Nothing
    }

-- | A policy that determines how Elastic Transcoder handles the existence of multiple captions.     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ . @MergePolicy@ cannot be null.
cMergePolicy :: Lens' Captions (Maybe Text)
cMergePolicy = lens _cMergePolicy (\s a -> s {_cMergePolicy = a})

-- | Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
cCaptionSources :: Lens' Captions [CaptionSource]
cCaptionSources = lens _cCaptionSources (\s a -> s {_cCaptionSources = a}) . _Default . _Coerce

-- | The array of file formats for the output captions. If you leave this value blank, Elastic Transcoder returns an error.
cCaptionFormats :: Lens' Captions [CaptionFormat]
cCaptionFormats = lens _cCaptionFormats (\s a -> s {_cCaptionFormats = a}) . _Default . _Coerce

instance FromJSON Captions where
  parseJSON =
    withObject
      "Captions"
      ( \x ->
          Captions'
            <$> (x .:? "MergePolicy")
            <*> (x .:? "CaptionSources" .!= mempty)
            <*> (x .:? "CaptionFormats" .!= mempty)
      )

instance Hashable Captions

instance NFData Captions

instance ToJSON Captions where
  toJSON Captions' {..} =
    object
      ( catMaybes
          [ ("MergePolicy" .=) <$> _cMergePolicy,
            ("CaptionSources" .=) <$> _cCaptionSources,
            ("CaptionFormats" .=) <$> _cCaptionFormats
          ]
      )
