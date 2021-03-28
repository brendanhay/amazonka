{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.WavFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.WavFormat
  ( WavFormat
    ( WavFormat'
    , WavFormatRiff
    , WavFormatRF64
    , fromWavFormat
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The service defaults to using RIFF for WAV outputs. If your output audio is likely to exceed 4 GB in file size, or if you otherwise need the extended support of the RF64 format, set your output WAV file format to RF64.
newtype WavFormat = WavFormat'{fromWavFormat :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern WavFormatRiff :: WavFormat
pattern WavFormatRiff = WavFormat' "RIFF"

pattern WavFormatRF64 :: WavFormat
pattern WavFormatRF64 = WavFormat' "RF64"

{-# COMPLETE 
  WavFormatRiff,

  WavFormatRF64,
  WavFormat'
  #-}
