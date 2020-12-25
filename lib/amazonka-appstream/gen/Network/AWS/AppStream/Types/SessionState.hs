{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.SessionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.SessionState
  ( SessionState
      ( SessionState',
        SessionStateActive,
        SessionStatePending,
        SessionStateExpired,
        fromSessionState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Possible values for the state of a streaming session.
newtype SessionState = SessionState' {fromSessionState :: Core.Text}
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

pattern SessionStateActive :: SessionState
pattern SessionStateActive = SessionState' "ACTIVE"

pattern SessionStatePending :: SessionState
pattern SessionStatePending = SessionState' "PENDING"

pattern SessionStateExpired :: SessionState
pattern SessionStateExpired = SessionState' "EXPIRED"

{-# COMPLETE
  SessionStateActive,
  SessionStatePending,
  SessionStateExpired,
  SessionState'
  #-}
