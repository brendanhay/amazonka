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
-- Module      : Amazonka.MediaLive.Types.AudioDescriptionAudioTypeControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioDescriptionAudioTypeControl
  ( AudioDescriptionAudioTypeControl
      ( ..,
        AudioDescriptionAudioTypeControl_FOLLOW_INPUT,
        AudioDescriptionAudioTypeControl_USE_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Audio Description Audio Type Control
newtype AudioDescriptionAudioTypeControl = AudioDescriptionAudioTypeControl'
  { fromAudioDescriptionAudioTypeControl ::
      Core.Text
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

pattern AudioDescriptionAudioTypeControl_FOLLOW_INPUT :: AudioDescriptionAudioTypeControl
pattern AudioDescriptionAudioTypeControl_FOLLOW_INPUT = AudioDescriptionAudioTypeControl' "FOLLOW_INPUT"

pattern AudioDescriptionAudioTypeControl_USE_CONFIGURED :: AudioDescriptionAudioTypeControl
pattern AudioDescriptionAudioTypeControl_USE_CONFIGURED = AudioDescriptionAudioTypeControl' "USE_CONFIGURED"

{-# COMPLETE
  AudioDescriptionAudioTypeControl_FOLLOW_INPUT,
  AudioDescriptionAudioTypeControl_USE_CONFIGURED,
  AudioDescriptionAudioTypeControl'
  #-}
