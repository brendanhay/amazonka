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
-- Module      : Amazonka.MediaLive.Types.Ac3BitstreamMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Ac3BitstreamMode
  ( Ac3BitstreamMode
      ( ..,
        Ac3BitstreamMode_COMMENTARY,
        Ac3BitstreamMode_COMPLETE_MAIN,
        Ac3BitstreamMode_DIALOGUE,
        Ac3BitstreamMode_EMERGENCY,
        Ac3BitstreamMode_HEARING_IMPAIRED,
        Ac3BitstreamMode_MUSIC_AND_EFFECTS,
        Ac3BitstreamMode_VISUALLY_IMPAIRED,
        Ac3BitstreamMode_VOICE_OVER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Ac3 Bitstream Mode
newtype Ac3BitstreamMode = Ac3BitstreamMode'
  { fromAc3BitstreamMode ::
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

pattern Ac3BitstreamMode_COMMENTARY :: Ac3BitstreamMode
pattern Ac3BitstreamMode_COMMENTARY = Ac3BitstreamMode' "COMMENTARY"

pattern Ac3BitstreamMode_COMPLETE_MAIN :: Ac3BitstreamMode
pattern Ac3BitstreamMode_COMPLETE_MAIN = Ac3BitstreamMode' "COMPLETE_MAIN"

pattern Ac3BitstreamMode_DIALOGUE :: Ac3BitstreamMode
pattern Ac3BitstreamMode_DIALOGUE = Ac3BitstreamMode' "DIALOGUE"

pattern Ac3BitstreamMode_EMERGENCY :: Ac3BitstreamMode
pattern Ac3BitstreamMode_EMERGENCY = Ac3BitstreamMode' "EMERGENCY"

pattern Ac3BitstreamMode_HEARING_IMPAIRED :: Ac3BitstreamMode
pattern Ac3BitstreamMode_HEARING_IMPAIRED = Ac3BitstreamMode' "HEARING_IMPAIRED"

pattern Ac3BitstreamMode_MUSIC_AND_EFFECTS :: Ac3BitstreamMode
pattern Ac3BitstreamMode_MUSIC_AND_EFFECTS = Ac3BitstreamMode' "MUSIC_AND_EFFECTS"

pattern Ac3BitstreamMode_VISUALLY_IMPAIRED :: Ac3BitstreamMode
pattern Ac3BitstreamMode_VISUALLY_IMPAIRED = Ac3BitstreamMode' "VISUALLY_IMPAIRED"

pattern Ac3BitstreamMode_VOICE_OVER :: Ac3BitstreamMode
pattern Ac3BitstreamMode_VOICE_OVER = Ac3BitstreamMode' "VOICE_OVER"

{-# COMPLETE
  Ac3BitstreamMode_COMMENTARY,
  Ac3BitstreamMode_COMPLETE_MAIN,
  Ac3BitstreamMode_DIALOGUE,
  Ac3BitstreamMode_EMERGENCY,
  Ac3BitstreamMode_HEARING_IMPAIRED,
  Ac3BitstreamMode_MUSIC_AND_EFFECTS,
  Ac3BitstreamMode_VISUALLY_IMPAIRED,
  Ac3BitstreamMode_VOICE_OVER,
  Ac3BitstreamMode'
  #-}
