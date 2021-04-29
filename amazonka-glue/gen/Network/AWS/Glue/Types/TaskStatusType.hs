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
-- Module      : Network.AWS.Glue.Types.TaskStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskStatusType
  ( TaskStatusType
      ( ..,
        TaskStatusType_FAILED,
        TaskStatusType_RUNNING,
        TaskStatusType_STARTING,
        TaskStatusType_STOPPED,
        TaskStatusType_STOPPING,
        TaskStatusType_SUCCEEDED,
        TaskStatusType_TIMEOUT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TaskStatusType = TaskStatusType'
  { fromTaskStatusType ::
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

pattern TaskStatusType_FAILED :: TaskStatusType
pattern TaskStatusType_FAILED = TaskStatusType' "FAILED"

pattern TaskStatusType_RUNNING :: TaskStatusType
pattern TaskStatusType_RUNNING = TaskStatusType' "RUNNING"

pattern TaskStatusType_STARTING :: TaskStatusType
pattern TaskStatusType_STARTING = TaskStatusType' "STARTING"

pattern TaskStatusType_STOPPED :: TaskStatusType
pattern TaskStatusType_STOPPED = TaskStatusType' "STOPPED"

pattern TaskStatusType_STOPPING :: TaskStatusType
pattern TaskStatusType_STOPPING = TaskStatusType' "STOPPING"

pattern TaskStatusType_SUCCEEDED :: TaskStatusType
pattern TaskStatusType_SUCCEEDED = TaskStatusType' "SUCCEEDED"

pattern TaskStatusType_TIMEOUT :: TaskStatusType
pattern TaskStatusType_TIMEOUT = TaskStatusType' "TIMEOUT"

{-# COMPLETE
  TaskStatusType_FAILED,
  TaskStatusType_RUNNING,
  TaskStatusType_STARTING,
  TaskStatusType_STOPPED,
  TaskStatusType_STOPPING,
  TaskStatusType_SUCCEEDED,
  TaskStatusType_TIMEOUT,
  TaskStatusType'
  #-}
