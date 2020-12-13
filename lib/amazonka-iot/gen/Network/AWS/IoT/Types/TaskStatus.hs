{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatus
  ( TaskStatus
      ( TaskStatus',
        InProgress,
        Completed,
        Failed,
        Cancelled,
        Cancelling
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TaskStatus = TaskStatus' Lude.Text
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

pattern InProgress :: TaskStatus
pattern InProgress = TaskStatus' "InProgress"

pattern Completed :: TaskStatus
pattern Completed = TaskStatus' "Completed"

pattern Failed :: TaskStatus
pattern Failed = TaskStatus' "Failed"

pattern Cancelled :: TaskStatus
pattern Cancelled = TaskStatus' "Cancelled"

pattern Cancelling :: TaskStatus
pattern Cancelling = TaskStatus' "Cancelling"

{-# COMPLETE
  InProgress,
  Completed,
  Failed,
  Cancelled,
  Cancelling,
  TaskStatus'
  #-}
