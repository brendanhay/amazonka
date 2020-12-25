{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3SurroundMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3SurroundMode
  ( Eac3SurroundMode
      ( Eac3SurroundMode',
        Eac3SurroundModeDisabled,
        Eac3SurroundModeEnabled,
        Eac3SurroundModeNotIndicated,
        fromEac3SurroundMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Surround Mode
newtype Eac3SurroundMode = Eac3SurroundMode'
  { fromEac3SurroundMode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Eac3SurroundModeDisabled :: Eac3SurroundMode
pattern Eac3SurroundModeDisabled = Eac3SurroundMode' "DISABLED"

pattern Eac3SurroundModeEnabled :: Eac3SurroundMode
pattern Eac3SurroundModeEnabled = Eac3SurroundMode' "ENABLED"

pattern Eac3SurroundModeNotIndicated :: Eac3SurroundMode
pattern Eac3SurroundModeNotIndicated = Eac3SurroundMode' "NOT_INDICATED"

{-# COMPLETE
  Eac3SurroundModeDisabled,
  Eac3SurroundModeEnabled,
  Eac3SurroundModeNotIndicated,
  Eac3SurroundMode'
  #-}
