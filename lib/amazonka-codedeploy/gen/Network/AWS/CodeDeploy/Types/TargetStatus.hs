{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetStatus
  ( TargetStatus
      ( TargetStatus',
        TSPending,
        TSInProgress,
        TSSucceeded,
        TSFailed,
        TSSkipped,
        TSUnknown,
        TSReady
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TargetStatus = TargetStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern TSPending :: TargetStatus
pattern TSPending = TargetStatus' "Pending"

pattern TSInProgress :: TargetStatus
pattern TSInProgress = TargetStatus' "InProgress"

pattern TSSucceeded :: TargetStatus
pattern TSSucceeded = TargetStatus' "Succeeded"

pattern TSFailed :: TargetStatus
pattern TSFailed = TargetStatus' "Failed"

pattern TSSkipped :: TargetStatus
pattern TSSkipped = TargetStatus' "Skipped"

pattern TSUnknown :: TargetStatus
pattern TSUnknown = TargetStatus' "Unknown"

pattern TSReady :: TargetStatus
pattern TSReady = TargetStatus' "Ready"

{-# COMPLETE
  TSPending,
  TSInProgress,
  TSSucceeded,
  TSFailed,
  TSSkipped,
  TSUnknown,
  TSReady,
  TargetStatus'
  #-}
