{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSetStatus
  ( ChangeSetStatus
      ( ChangeSetStatus',
        ChangeSetStatusCreatePending,
        ChangeSetStatusCreateInProgress,
        ChangeSetStatusCreateComplete,
        ChangeSetStatusDeletePending,
        ChangeSetStatusDeleteInProgress,
        ChangeSetStatusDeleteComplete,
        ChangeSetStatusDeleteFailed,
        ChangeSetStatusFailed,
        fromChangeSetStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ChangeSetStatus = ChangeSetStatus'
  { fromChangeSetStatus ::
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

pattern ChangeSetStatusCreatePending :: ChangeSetStatus
pattern ChangeSetStatusCreatePending = ChangeSetStatus' "CREATE_PENDING"

pattern ChangeSetStatusCreateInProgress :: ChangeSetStatus
pattern ChangeSetStatusCreateInProgress = ChangeSetStatus' "CREATE_IN_PROGRESS"

pattern ChangeSetStatusCreateComplete :: ChangeSetStatus
pattern ChangeSetStatusCreateComplete = ChangeSetStatus' "CREATE_COMPLETE"

pattern ChangeSetStatusDeletePending :: ChangeSetStatus
pattern ChangeSetStatusDeletePending = ChangeSetStatus' "DELETE_PENDING"

pattern ChangeSetStatusDeleteInProgress :: ChangeSetStatus
pattern ChangeSetStatusDeleteInProgress = ChangeSetStatus' "DELETE_IN_PROGRESS"

pattern ChangeSetStatusDeleteComplete :: ChangeSetStatus
pattern ChangeSetStatusDeleteComplete = ChangeSetStatus' "DELETE_COMPLETE"

pattern ChangeSetStatusDeleteFailed :: ChangeSetStatus
pattern ChangeSetStatusDeleteFailed = ChangeSetStatus' "DELETE_FAILED"

pattern ChangeSetStatusFailed :: ChangeSetStatus
pattern ChangeSetStatusFailed = ChangeSetStatus' "FAILED"

{-# COMPLETE
  ChangeSetStatusCreatePending,
  ChangeSetStatusCreateInProgress,
  ChangeSetStatusCreateComplete,
  ChangeSetStatusDeletePending,
  ChangeSetStatusDeleteInProgress,
  ChangeSetStatusDeleteComplete,
  ChangeSetStatusDeleteFailed,
  ChangeSetStatusFailed,
  ChangeSetStatus'
  #-}
