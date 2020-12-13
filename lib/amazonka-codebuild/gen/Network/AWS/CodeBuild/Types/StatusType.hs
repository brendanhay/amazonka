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
        Succeeded,
        Failed,
        Fault,
        TimedOut,
        InProgress,
        Stopped
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

pattern Succeeded :: StatusType
pattern Succeeded = StatusType' "SUCCEEDED"

pattern Failed :: StatusType
pattern Failed = StatusType' "FAILED"

pattern Fault :: StatusType
pattern Fault = StatusType' "FAULT"

pattern TimedOut :: StatusType
pattern TimedOut = StatusType' "TIMED_OUT"

pattern InProgress :: StatusType
pattern InProgress = StatusType' "IN_PROGRESS"

pattern Stopped :: StatusType
pattern Stopped = StatusType' "STOPPED"

{-# COMPLETE
  Succeeded,
  Failed,
  Fault,
  TimedOut,
  InProgress,
  Stopped,
  StatusType'
  #-}
