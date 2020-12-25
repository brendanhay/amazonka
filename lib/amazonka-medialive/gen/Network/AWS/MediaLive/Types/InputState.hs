{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputState
  ( InputState
      ( InputState',
        InputStateCreating,
        InputStateDetached,
        InputStateAttached,
        InputStateDeleting,
        InputStateDeleted,
        fromInputState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for InputState
newtype InputState = InputState' {fromInputState :: Core.Text}
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

pattern InputStateCreating :: InputState
pattern InputStateCreating = InputState' "CREATING"

pattern InputStateDetached :: InputState
pattern InputStateDetached = InputState' "DETACHED"

pattern InputStateAttached :: InputState
pattern InputStateAttached = InputState' "ATTACHED"

pattern InputStateDeleting :: InputState
pattern InputStateDeleting = InputState' "DELETING"

pattern InputStateDeleted :: InputState
pattern InputStateDeleted = InputState' "DELETED"

{-# COMPLETE
  InputStateCreating,
  InputStateDetached,
  InputStateAttached,
  InputStateDeleting,
  InputStateDeleted,
  InputState'
  #-}
