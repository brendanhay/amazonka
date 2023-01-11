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
-- Module      : Amazonka.MediaConvert.Types.AudioChannelTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioChannelTag
  ( AudioChannelTag
      ( ..,
        AudioChannelTag_C,
        AudioChannelTag_CS,
        AudioChannelTag_L,
        AudioChannelTag_LC,
        AudioChannelTag_LFE,
        AudioChannelTag_LS,
        AudioChannelTag_LSD,
        AudioChannelTag_R,
        AudioChannelTag_RC,
        AudioChannelTag_RS,
        AudioChannelTag_RSD,
        AudioChannelTag_TCS,
        AudioChannelTag_VHC,
        AudioChannelTag_VHL,
        AudioChannelTag_VHR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | You can add a tag for this mono-channel audio track to mimic its
-- placement in a multi-channel layout. For example, if this track is the
-- left surround channel, choose Left surround (LS).
newtype AudioChannelTag = AudioChannelTag'
  { fromAudioChannelTag ::
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

pattern AudioChannelTag_C :: AudioChannelTag
pattern AudioChannelTag_C = AudioChannelTag' "C"

pattern AudioChannelTag_CS :: AudioChannelTag
pattern AudioChannelTag_CS = AudioChannelTag' "CS"

pattern AudioChannelTag_L :: AudioChannelTag
pattern AudioChannelTag_L = AudioChannelTag' "L"

pattern AudioChannelTag_LC :: AudioChannelTag
pattern AudioChannelTag_LC = AudioChannelTag' "LC"

pattern AudioChannelTag_LFE :: AudioChannelTag
pattern AudioChannelTag_LFE = AudioChannelTag' "LFE"

pattern AudioChannelTag_LS :: AudioChannelTag
pattern AudioChannelTag_LS = AudioChannelTag' "LS"

pattern AudioChannelTag_LSD :: AudioChannelTag
pattern AudioChannelTag_LSD = AudioChannelTag' "LSD"

pattern AudioChannelTag_R :: AudioChannelTag
pattern AudioChannelTag_R = AudioChannelTag' "R"

pattern AudioChannelTag_RC :: AudioChannelTag
pattern AudioChannelTag_RC = AudioChannelTag' "RC"

pattern AudioChannelTag_RS :: AudioChannelTag
pattern AudioChannelTag_RS = AudioChannelTag' "RS"

pattern AudioChannelTag_RSD :: AudioChannelTag
pattern AudioChannelTag_RSD = AudioChannelTag' "RSD"

pattern AudioChannelTag_TCS :: AudioChannelTag
pattern AudioChannelTag_TCS = AudioChannelTag' "TCS"

pattern AudioChannelTag_VHC :: AudioChannelTag
pattern AudioChannelTag_VHC = AudioChannelTag' "VHC"

pattern AudioChannelTag_VHL :: AudioChannelTag
pattern AudioChannelTag_VHL = AudioChannelTag' "VHL"

pattern AudioChannelTag_VHR :: AudioChannelTag
pattern AudioChannelTag_VHR = AudioChannelTag' "VHR"

{-# COMPLETE
  AudioChannelTag_C,
  AudioChannelTag_CS,
  AudioChannelTag_L,
  AudioChannelTag_LC,
  AudioChannelTag_LFE,
  AudioChannelTag_LS,
  AudioChannelTag_LSD,
  AudioChannelTag_R,
  AudioChannelTag_RC,
  AudioChannelTag_RS,
  AudioChannelTag_RSD,
  AudioChannelTag_TCS,
  AudioChannelTag_VHC,
  AudioChannelTag_VHL,
  AudioChannelTag_VHR,
  AudioChannelTag'
  #-}
