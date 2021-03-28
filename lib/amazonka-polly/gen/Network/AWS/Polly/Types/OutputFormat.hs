{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.OutputFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.OutputFormat
  ( OutputFormat
    ( OutputFormat'
    , OutputFormatJson
    , OutputFormatMP3
    , OutputFormatOggVorbis
    , OutputFormatPcm
    , fromOutputFormat
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OutputFormat = OutputFormat'{fromOutputFormat :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern OutputFormatJson :: OutputFormat
pattern OutputFormatJson = OutputFormat' "json"

pattern OutputFormatMP3 :: OutputFormat
pattern OutputFormatMP3 = OutputFormat' "mp3"

pattern OutputFormatOggVorbis :: OutputFormat
pattern OutputFormatOggVorbis = OutputFormat' "ogg_vorbis"

pattern OutputFormatPcm :: OutputFormat
pattern OutputFormatPcm = OutputFormat' "pcm"

{-# COMPLETE 
  OutputFormatJson,

  OutputFormatMP3,

  OutputFormatOggVorbis,

  OutputFormatPcm,
  OutputFormat'
  #-}
