{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3SurroundExMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3SurroundExMode
  ( Eac3SurroundExMode
      ( Eac3SurroundExMode',
        Eac3SurroundExModeDisabled,
        Eac3SurroundExModeEnabled,
        Eac3SurroundExModeNotIndicated,
        fromEac3SurroundExMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Surround Ex Mode
newtype Eac3SurroundExMode = Eac3SurroundExMode'
  { fromEac3SurroundExMode ::
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

pattern Eac3SurroundExModeDisabled :: Eac3SurroundExMode
pattern Eac3SurroundExModeDisabled = Eac3SurroundExMode' "DISABLED"

pattern Eac3SurroundExModeEnabled :: Eac3SurroundExMode
pattern Eac3SurroundExModeEnabled = Eac3SurroundExMode' "ENABLED"

pattern Eac3SurroundExModeNotIndicated :: Eac3SurroundExMode
pattern Eac3SurroundExModeNotIndicated = Eac3SurroundExMode' "NOT_INDICATED"

{-# COMPLETE
  Eac3SurroundExModeDisabled,
  Eac3SurroundExModeEnabled,
  Eac3SurroundExModeNotIndicated,
  Eac3SurroundExMode'
  #-}
