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
-- Module      : Amazonka.Glue.Types.TaskStatusType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TaskStatusType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaskStatusType = TaskStatusType'
  { fromTaskStatusType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
