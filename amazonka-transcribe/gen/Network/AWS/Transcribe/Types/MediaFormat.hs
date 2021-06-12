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
-- Module      : Network.AWS.Transcribe.Types.MediaFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MediaFormat
  ( MediaFormat
      ( ..,
        MediaFormat_Amr,
        MediaFormat_Flac,
        MediaFormat_Mp3,
        MediaFormat_Mp4,
        MediaFormat_Ogg,
        MediaFormat_Wav,
        MediaFormat_Webm
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MediaFormat = MediaFormat'
  { fromMediaFormat ::
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

pattern MediaFormat_Amr :: MediaFormat
pattern MediaFormat_Amr = MediaFormat' "amr"

pattern MediaFormat_Flac :: MediaFormat
pattern MediaFormat_Flac = MediaFormat' "flac"

pattern MediaFormat_Mp3 :: MediaFormat
pattern MediaFormat_Mp3 = MediaFormat' "mp3"

pattern MediaFormat_Mp4 :: MediaFormat
pattern MediaFormat_Mp4 = MediaFormat' "mp4"

pattern MediaFormat_Ogg :: MediaFormat
pattern MediaFormat_Ogg = MediaFormat' "ogg"

pattern MediaFormat_Wav :: MediaFormat
pattern MediaFormat_Wav = MediaFormat' "wav"

pattern MediaFormat_Webm :: MediaFormat
pattern MediaFormat_Webm = MediaFormat' "webm"

{-# COMPLETE
  MediaFormat_Amr,
  MediaFormat_Flac,
  MediaFormat_Mp3,
  MediaFormat_Mp4,
  MediaFormat_Ogg,
  MediaFormat_Wav,
  MediaFormat_Webm,
  MediaFormat'
  #-}
