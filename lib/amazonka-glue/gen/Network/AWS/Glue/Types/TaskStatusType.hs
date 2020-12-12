{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskStatusType
  ( TaskStatusType
      ( TaskStatusType',
        Failed,
        Running,
        Starting,
        Stopped,
        Stopping,
        Succeeded,
        Timeout
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TaskStatusType = TaskStatusType' Lude.Text
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

pattern Failed :: TaskStatusType
pattern Failed = TaskStatusType' "FAILED"

pattern Running :: TaskStatusType
pattern Running = TaskStatusType' "RUNNING"

pattern Starting :: TaskStatusType
pattern Starting = TaskStatusType' "STARTING"

pattern Stopped :: TaskStatusType
pattern Stopped = TaskStatusType' "STOPPED"

pattern Stopping :: TaskStatusType
pattern Stopping = TaskStatusType' "STOPPING"

pattern Succeeded :: TaskStatusType
pattern Succeeded = TaskStatusType' "SUCCEEDED"

pattern Timeout :: TaskStatusType
pattern Timeout = TaskStatusType' "TIMEOUT"

{-# COMPLETE
  Failed,
  Running,
  Starting,
  Stopped,
  Stopping,
  Succeeded,
  Timeout,
  TaskStatusType'
  #-}
