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
-- Module      : Network.AWS.MediaConvert.Types.WavFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WavFormat
  ( WavFormat
      ( ..,
        WavFormat_RF64,
        WavFormat_RIFF
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The service defaults to using RIFF for WAV outputs. If your output audio
-- is likely to exceed 4 GB in file size, or if you otherwise need the
-- extended support of the RF64 format, set your output WAV file format to
-- RF64.
newtype WavFormat = WavFormat'
  { fromWavFormat ::
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

pattern WavFormat_RF64 :: WavFormat
pattern WavFormat_RF64 = WavFormat' "RF64"

pattern WavFormat_RIFF :: WavFormat
pattern WavFormat_RIFF = WavFormat' "RIFF"

{-# COMPLETE
  WavFormat_RF64,
  WavFormat_RIFF,
  WavFormat'
  #-}
