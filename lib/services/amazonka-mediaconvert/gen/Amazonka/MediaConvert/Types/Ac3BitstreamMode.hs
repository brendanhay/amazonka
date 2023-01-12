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
-- Module      : Amazonka.MediaConvert.Types.Ac3BitstreamMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Ac3BitstreamMode
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
newtype Ac3BitstreamMode = Ac3BitstreamMode'
  { fromAc3BitstreamMode ::
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
