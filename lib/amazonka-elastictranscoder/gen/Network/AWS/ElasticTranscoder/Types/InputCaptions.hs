{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.InputCaptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.InputCaptions where

import Network.AWS.ElasticTranscoder.Types.CaptionSource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The captions to be created, if any.
--
--
--
-- /See:/ 'inputCaptions' smart constructor.
data InputCaptions = InputCaptions'
  { _icMergePolicy ::
      !(Maybe Text),
    _icCaptionSources :: !(Maybe [CaptionSource])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputCaptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icMergePolicy' - A policy that determines how Elastic Transcoder handles the existence of multiple captions.     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ . @MergePolicy@ cannot be null.
--
-- * 'icCaptionSources' - Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
inputCaptions ::
  InputCaptions
inputCaptions =
  InputCaptions'
    { _icMergePolicy = Nothing,
      _icCaptionSources = Nothing
    }

-- | A policy that determines how Elastic Transcoder handles the existence of multiple captions.     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ . @MergePolicy@ cannot be null.
icMergePolicy :: Lens' InputCaptions (Maybe Text)
icMergePolicy = lens _icMergePolicy (\s a -> s {_icMergePolicy = a})

-- | Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
icCaptionSources :: Lens' InputCaptions [CaptionSource]
icCaptionSources = lens _icCaptionSources (\s a -> s {_icCaptionSources = a}) . _Default . _Coerce

instance FromJSON InputCaptions where
  parseJSON =
    withObject
      "InputCaptions"
      ( \x ->
          InputCaptions'
            <$> (x .:? "MergePolicy") <*> (x .:? "CaptionSources" .!= mempty)
      )

instance Hashable InputCaptions

instance NFData InputCaptions

instance ToJSON InputCaptions where
  toJSON InputCaptions' {..} =
    object
      ( catMaybes
          [ ("MergePolicy" .=) <$> _icMergePolicy,
            ("CaptionSources" .=) <$> _icCaptionSources
          ]
      )
