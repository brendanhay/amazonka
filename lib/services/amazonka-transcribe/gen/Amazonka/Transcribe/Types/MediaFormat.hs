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
-- Module      : Amazonka.Transcribe.Types.MediaFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.MediaFormat
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MediaFormat = MediaFormat'
  { fromMediaFormat ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
