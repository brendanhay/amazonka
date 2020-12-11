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
        ETSActive,
        ETSCancelled,
        ETSCancelling,
        ETSCompleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExportTaskState = ExportTaskState' Lude.Text
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

pattern ETSActive :: ExportTaskState
pattern ETSActive = ExportTaskState' "active"

pattern ETSCancelled :: ExportTaskState
pattern ETSCancelled = ExportTaskState' "cancelled"

pattern ETSCancelling :: ExportTaskState
pattern ETSCancelling = ExportTaskState' "cancelling"

pattern ETSCompleted :: ExportTaskState
pattern ETSCompleted = ExportTaskState' "completed"

{-# COMPLETE
  ETSActive,
  ETSCancelled,
  ETSCancelling,
  ETSCompleted,
  ExportTaskState'
  #-}
