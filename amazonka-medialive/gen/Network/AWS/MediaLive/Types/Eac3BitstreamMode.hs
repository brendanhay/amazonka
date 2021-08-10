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
-- Module      : Network.AWS.MediaLive.Types.Eac3BitstreamMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3BitstreamMode
  ( Eac3BitstreamMode
      ( ..,
        Eac3BitstreamMode_COMMENTARY,
        Eac3BitstreamMode_COMPLETE_MAIN,
        Eac3BitstreamMode_EMERGENCY,
        Eac3BitstreamMode_HEARING_IMPAIRED,
        Eac3BitstreamMode_VISUALLY_IMPAIRED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Eac3 Bitstream Mode
newtype Eac3BitstreamMode = Eac3BitstreamMode'
  { fromEac3BitstreamMode ::
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

pattern Eac3BitstreamMode_COMMENTARY :: Eac3BitstreamMode
pattern Eac3BitstreamMode_COMMENTARY = Eac3BitstreamMode' "COMMENTARY"

pattern Eac3BitstreamMode_COMPLETE_MAIN :: Eac3BitstreamMode
pattern Eac3BitstreamMode_COMPLETE_MAIN = Eac3BitstreamMode' "COMPLETE_MAIN"

pattern Eac3BitstreamMode_EMERGENCY :: Eac3BitstreamMode
pattern Eac3BitstreamMode_EMERGENCY = Eac3BitstreamMode' "EMERGENCY"

pattern Eac3BitstreamMode_HEARING_IMPAIRED :: Eac3BitstreamMode
pattern Eac3BitstreamMode_HEARING_IMPAIRED = Eac3BitstreamMode' "HEARING_IMPAIRED"

pattern Eac3BitstreamMode_VISUALLY_IMPAIRED :: Eac3BitstreamMode
pattern Eac3BitstreamMode_VISUALLY_IMPAIRED = Eac3BitstreamMode' "VISUALLY_IMPAIRED"

{-# COMPLETE
  Eac3BitstreamMode_COMMENTARY,
  Eac3BitstreamMode_COMPLETE_MAIN,
  Eac3BitstreamMode_EMERGENCY,
  Eac3BitstreamMode_HEARING_IMPAIRED,
  Eac3BitstreamMode_VISUALLY_IMPAIRED,
  Eac3BitstreamMode'
  #-}
