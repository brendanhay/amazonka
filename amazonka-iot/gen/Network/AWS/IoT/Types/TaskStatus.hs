{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatus
  ( TaskStatus
      ( ..,
        TaskStatus_Cancelled,
        TaskStatus_Cancelling,
        TaskStatus_Completed,
        TaskStatus_Failed,
        TaskStatus_InProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TaskStatus = TaskStatus'
  { fromTaskStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern TaskStatus_Cancelled :: TaskStatus
pattern TaskStatus_Cancelled = TaskStatus' "Cancelled"

pattern TaskStatus_Cancelling :: TaskStatus
pattern TaskStatus_Cancelling = TaskStatus' "Cancelling"

pattern TaskStatus_Completed :: TaskStatus
pattern TaskStatus_Completed = TaskStatus' "Completed"

pattern TaskStatus_Failed :: TaskStatus
pattern TaskStatus_Failed = TaskStatus' "Failed"

pattern TaskStatus_InProgress :: TaskStatus
pattern TaskStatus_InProgress = TaskStatus' "InProgress"

{-# COMPLETE
  TaskStatus_Cancelled,
  TaskStatus_Cancelling,
  TaskStatus_Completed,
  TaskStatus_Failed,
  TaskStatus_InProgress,
  TaskStatus'
  #-}
