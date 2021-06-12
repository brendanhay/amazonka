{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.OutputFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.OutputFormat
  ( OutputFormat
      ( ..,
        OutputFormat_Json,
        OutputFormat_Mp3,
        OutputFormat_Ogg_vorbis,
        OutputFormat_Pcm
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OutputFormat = OutputFormat'
  { fromOutputFormat ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern OutputFormat_Json :: OutputFormat
pattern OutputFormat_Json = OutputFormat' "json"

pattern OutputFormat_Mp3 :: OutputFormat
pattern OutputFormat_Mp3 = OutputFormat' "mp3"

pattern OutputFormat_Ogg_vorbis :: OutputFormat
pattern OutputFormat_Ogg_vorbis = OutputFormat' "ogg_vorbis"

pattern OutputFormat_Pcm :: OutputFormat
pattern OutputFormat_Pcm = OutputFormat' "pcm"

{-# COMPLETE
  OutputFormat_Json,
  OutputFormat_Mp3,
  OutputFormat_Ogg_vorbis,
  OutputFormat_Pcm,
  OutputFormat'
  #-}
