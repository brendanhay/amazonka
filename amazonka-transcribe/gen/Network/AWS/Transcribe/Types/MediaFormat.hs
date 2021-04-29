{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype MediaFormat = MediaFormat'
  { fromMediaFormat ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
