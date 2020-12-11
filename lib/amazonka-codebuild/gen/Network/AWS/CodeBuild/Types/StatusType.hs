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
        Failed,
        Fault,
        InProgress,
        Stopped,
        Succeeded,
        TimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StatusType = StatusType' Lude.Text
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

pattern Failed :: StatusType
pattern Failed = StatusType' "FAILED"

pattern Fault :: StatusType
pattern Fault = StatusType' "FAULT"

pattern InProgress :: StatusType
pattern InProgress = StatusType' "IN_PROGRESS"

pattern Stopped :: StatusType
pattern Stopped = StatusType' "STOPPED"

pattern Succeeded :: StatusType
pattern Succeeded = StatusType' "SUCCEEDED"

pattern TimedOut :: StatusType
pattern TimedOut = StatusType' "TIMED_OUT"

{-# COMPLETE
  Failed,
  Fault,
  InProgress,
  Stopped,
  Succeeded,
  TimedOut,
  StatusType'
  #-}
