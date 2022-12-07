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
-- Module      : Amazonka.MediaConvert.Types.HlsAudioOnlyHeader
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsAudioOnlyHeader
  ( HlsAudioOnlyHeader
      ( ..,
        HlsAudioOnlyHeader_EXCLUDE,
        HlsAudioOnlyHeader_INCLUDE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix
-- and you encounter playback issues. Keep the default value, Include
-- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
-- remove the audio-only headers from your audio segments.
newtype HlsAudioOnlyHeader = HlsAudioOnlyHeader'
  { fromHlsAudioOnlyHeader ::
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

pattern HlsAudioOnlyHeader_EXCLUDE :: HlsAudioOnlyHeader
pattern HlsAudioOnlyHeader_EXCLUDE = HlsAudioOnlyHeader' "EXCLUDE"

pattern HlsAudioOnlyHeader_INCLUDE :: HlsAudioOnlyHeader
pattern HlsAudioOnlyHeader_INCLUDE = HlsAudioOnlyHeader' "INCLUDE"

{-# COMPLETE
  HlsAudioOnlyHeader_EXCLUDE,
  HlsAudioOnlyHeader_INCLUDE,
  HlsAudioOnlyHeader'
  #-}
