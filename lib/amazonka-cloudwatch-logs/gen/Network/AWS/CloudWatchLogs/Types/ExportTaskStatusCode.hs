-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
  ( ExportTaskStatusCode
      ( ExportTaskStatusCode',
        ETSCCancelled,
        ETSCCompleted,
        ETSCFailed,
        ETSCPending,
        ETSCPendingCancel,
        ETSCRunning
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExportTaskStatusCode = ExportTaskStatusCode' Lude.Text
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

pattern ETSCCancelled :: ExportTaskStatusCode
pattern ETSCCancelled = ExportTaskStatusCode' "CANCELLED"

pattern ETSCCompleted :: ExportTaskStatusCode
pattern ETSCCompleted = ExportTaskStatusCode' "COMPLETED"

pattern ETSCFailed :: ExportTaskStatusCode
pattern ETSCFailed = ExportTaskStatusCode' "FAILED"

pattern ETSCPending :: ExportTaskStatusCode
pattern ETSCPending = ExportTaskStatusCode' "PENDING"

pattern ETSCPendingCancel :: ExportTaskStatusCode
pattern ETSCPendingCancel = ExportTaskStatusCode' "PENDING_CANCEL"

pattern ETSCRunning :: ExportTaskStatusCode
pattern ETSCRunning = ExportTaskStatusCode' "RUNNING"

{-# COMPLETE
  ETSCCancelled,
  ETSCCompleted,
  ETSCFailed,
  ETSCPending,
  ETSCPendingCancel,
  ETSCRunning,
  ExportTaskStatusCode'
  #-}
