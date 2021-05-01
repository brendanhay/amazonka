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
-- Module      : Network.AWS.MediaConvert.Types.Ac3BitstreamMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3BitstreamMode
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

import qualified Network.AWS.Prelude as Prelude

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
newtype Ac3BitstreamMode = Ac3BitstreamMode'
  { fromAc3BitstreamMode ::
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
