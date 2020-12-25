{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.State
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.State
  ( State
      ( State',
        StatePendingAcceptance,
        StatePending,
        StateAvailable,
        StateDeleting,
        StateDeleted,
        StateRejected,
        StateFailed,
        StateExpired,
        fromState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype State = State' {fromState :: Core.Text}
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

pattern StatePendingAcceptance :: State
pattern StatePendingAcceptance = State' "PendingAcceptance"

pattern StatePending :: State
pattern StatePending = State' "Pending"

pattern StateAvailable :: State
pattern StateAvailable = State' "Available"

pattern StateDeleting :: State
pattern StateDeleting = State' "Deleting"

pattern StateDeleted :: State
pattern StateDeleted = State' "Deleted"

pattern StateRejected :: State
pattern StateRejected = State' "Rejected"

pattern StateFailed :: State
pattern StateFailed = State' "Failed"

pattern StateExpired :: State
pattern StateExpired = State' "Expired"

{-# COMPLETE
  StatePendingAcceptance,
  StatePending,
  StateAvailable,
  StateDeleting,
  StateDeleted,
  StateRejected,
  StateFailed,
  StateExpired,
  State'
  #-}
