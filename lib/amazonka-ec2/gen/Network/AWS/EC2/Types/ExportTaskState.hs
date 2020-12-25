{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskState
  ( ExportTaskState
      ( ExportTaskState',
        ExportTaskStateActive,
        ExportTaskStateCancelling,
        ExportTaskStateCancelled,
        ExportTaskStateCompleted,
        fromExportTaskState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ExportTaskState = ExportTaskState'
  { fromExportTaskState ::
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

pattern ExportTaskStateActive :: ExportTaskState
pattern ExportTaskStateActive = ExportTaskState' "active"

pattern ExportTaskStateCancelling :: ExportTaskState
pattern ExportTaskStateCancelling = ExportTaskState' "cancelling"

pattern ExportTaskStateCancelled :: ExportTaskState
pattern ExportTaskStateCancelled = ExportTaskState' "cancelled"

pattern ExportTaskStateCompleted :: ExportTaskState
pattern ExportTaskStateCompleted = ExportTaskState' "completed"

{-# COMPLETE
  ExportTaskStateActive,
  ExportTaskStateCancelling,
  ExportTaskStateCancelled,
  ExportTaskStateCompleted,
  ExportTaskState'
  #-}
