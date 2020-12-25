{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.StatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.StatusType
  ( StatusType
      ( StatusType',
        StatusTypeSucceeded,
        StatusTypeFailed,
        StatusTypeFault,
        StatusTypeTimedOut,
        StatusTypeInProgress,
        StatusTypeStopped,
        fromStatusType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StatusType = StatusType' {fromStatusType :: Core.Text}
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

pattern StatusTypeSucceeded :: StatusType
pattern StatusTypeSucceeded = StatusType' "SUCCEEDED"

pattern StatusTypeFailed :: StatusType
pattern StatusTypeFailed = StatusType' "FAILED"

pattern StatusTypeFault :: StatusType
pattern StatusTypeFault = StatusType' "FAULT"

pattern StatusTypeTimedOut :: StatusType
pattern StatusTypeTimedOut = StatusType' "TIMED_OUT"

pattern StatusTypeInProgress :: StatusType
pattern StatusTypeInProgress = StatusType' "IN_PROGRESS"

pattern StatusTypeStopped :: StatusType
pattern StatusTypeStopped = StatusType' "STOPPED"

{-# COMPLETE
  StatusTypeSucceeded,
  StatusTypeFailed,
  StatusTypeFault,
  StatusTypeTimedOut,
  StatusTypeInProgress,
  StatusTypeStopped,
  StatusType'
  #-}
