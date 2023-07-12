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
-- Module      : Amazonka.MediaConvert.Types.AudioTypeControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioTypeControl
  ( AudioTypeControl
      ( ..,
        AudioTypeControl_FOLLOW_INPUT,
        AudioTypeControl_USE_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type,
-- then that value is passed through to the output. If the input contains
-- no ISO 639 audio_type, the value in Audio Type is included in the
-- output. Otherwise the value in Audio Type is included in the output.
-- Note that this field and audioType are both ignored if
-- audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
newtype AudioTypeControl = AudioTypeControl'
  { fromAudioTypeControl ::
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

pattern AudioTypeControl_FOLLOW_INPUT :: AudioTypeControl
pattern AudioTypeControl_FOLLOW_INPUT = AudioTypeControl' "FOLLOW_INPUT"

pattern AudioTypeControl_USE_CONFIGURED :: AudioTypeControl
pattern AudioTypeControl_USE_CONFIGURED = AudioTypeControl' "USE_CONFIGURED"

{-# COMPLETE
  AudioTypeControl_FOLLOW_INPUT,
  AudioTypeControl_USE_CONFIGURED,
  AudioTypeControl'
  #-}
