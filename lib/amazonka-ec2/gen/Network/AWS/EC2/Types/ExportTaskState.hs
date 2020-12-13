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
        ETSActive,
        ETSCancelling,
        ETSCancelled,
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

pattern ETSCancelling :: ExportTaskState
pattern ETSCancelling = ExportTaskState' "cancelling"

pattern ETSCancelled :: ExportTaskState
pattern ETSCancelled = ExportTaskState' "cancelled"

pattern ETSCompleted :: ExportTaskState
pattern ETSCompleted = ExportTaskState' "completed"

{-# COMPLETE
  ETSActive,
  ETSCancelling,
  ETSCancelled,
  ETSCompleted,
  ExportTaskState'
  #-}
