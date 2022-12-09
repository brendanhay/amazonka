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
-- Module      : Amazonka.Omics.Types.TaskStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.TaskStatus
  ( TaskStatus
      ( ..,
        TaskStatus_CANCELLED,
        TaskStatus_COMPLETED,
        TaskStatus_FAILED,
        TaskStatus_PENDING,
        TaskStatus_RUNNING,
        TaskStatus_STARTING,
        TaskStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaskStatus = TaskStatus'
  { fromTaskStatus ::
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

pattern TaskStatus_CANCELLED :: TaskStatus
pattern TaskStatus_CANCELLED = TaskStatus' "CANCELLED"

pattern TaskStatus_COMPLETED :: TaskStatus
pattern TaskStatus_COMPLETED = TaskStatus' "COMPLETED"

pattern TaskStatus_FAILED :: TaskStatus
pattern TaskStatus_FAILED = TaskStatus' "FAILED"

pattern TaskStatus_PENDING :: TaskStatus
pattern TaskStatus_PENDING = TaskStatus' "PENDING"

pattern TaskStatus_RUNNING :: TaskStatus
pattern TaskStatus_RUNNING = TaskStatus' "RUNNING"

pattern TaskStatus_STARTING :: TaskStatus
pattern TaskStatus_STARTING = TaskStatus' "STARTING"

pattern TaskStatus_STOPPING :: TaskStatus
pattern TaskStatus_STOPPING = TaskStatus' "STOPPING"

{-# COMPLETE
  TaskStatus_CANCELLED,
  TaskStatus_COMPLETED,
  TaskStatus_FAILED,
  TaskStatus_PENDING,
  TaskStatus_RUNNING,
  TaskStatus_STARTING,
  TaskStatus_STOPPING,
  TaskStatus'
  #-}
