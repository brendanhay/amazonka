{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.TaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.TaskStatus
  ( TaskStatus
      ( ..,
        TaskStatus_Completed,
        TaskStatus_Failed,
        TaskStatus_InProgress,
        TaskStatus_Scheduled
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TaskStatus = TaskStatus'
  { fromTaskStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern TaskStatus_Completed :: TaskStatus
pattern TaskStatus_Completed = TaskStatus' "completed"

pattern TaskStatus_Failed :: TaskStatus
pattern TaskStatus_Failed = TaskStatus' "failed"

pattern TaskStatus_InProgress :: TaskStatus
pattern TaskStatus_InProgress = TaskStatus' "inProgress"

pattern TaskStatus_Scheduled :: TaskStatus
pattern TaskStatus_Scheduled = TaskStatus' "scheduled"

{-# COMPLETE
  TaskStatus_Completed,
  TaskStatus_Failed,
  TaskStatus_InProgress,
  TaskStatus_Scheduled,
  TaskStatus'
  #-}
