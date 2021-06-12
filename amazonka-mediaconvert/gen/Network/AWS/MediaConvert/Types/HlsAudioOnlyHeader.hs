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
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
  ( HlsAudioOnlyHeader
      ( ..,
        HlsAudioOnlyHeader_EXCLUDE,
        HlsAudioOnlyHeader_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix
-- and you encounter playback issues. Keep the default value, Include
-- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
-- remove the audio-only headers from your audio segments.
newtype HlsAudioOnlyHeader = HlsAudioOnlyHeader'
  { fromHlsAudioOnlyHeader ::
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

pattern HlsAudioOnlyHeader_EXCLUDE :: HlsAudioOnlyHeader
pattern HlsAudioOnlyHeader_EXCLUDE = HlsAudioOnlyHeader' "EXCLUDE"

pattern HlsAudioOnlyHeader_INCLUDE :: HlsAudioOnlyHeader
pattern HlsAudioOnlyHeader_INCLUDE = HlsAudioOnlyHeader' "INCLUDE"

{-# COMPLETE
  HlsAudioOnlyHeader_EXCLUDE,
  HlsAudioOnlyHeader_INCLUDE,
  HlsAudioOnlyHeader'
  #-}
