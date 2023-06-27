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
-- Module      : Amazonka.TNB.Types.TaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.TaskStatus
  ( TaskStatus
      ( ..,
        TaskStatus_CANCELLED,
        TaskStatus_COMPLETED,
        TaskStatus_ERROR,
        TaskStatus_IN_PROGRESS,
        TaskStatus_SCHEDULED,
        TaskStatus_SKIPPED,
        TaskStatus_STARTED
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

pattern TaskStatus_ERROR :: TaskStatus
pattern TaskStatus_ERROR = TaskStatus' "ERROR"

pattern TaskStatus_IN_PROGRESS :: TaskStatus
pattern TaskStatus_IN_PROGRESS = TaskStatus' "IN_PROGRESS"

pattern TaskStatus_SCHEDULED :: TaskStatus
pattern TaskStatus_SCHEDULED = TaskStatus' "SCHEDULED"

pattern TaskStatus_SKIPPED :: TaskStatus
pattern TaskStatus_SKIPPED = TaskStatus' "SKIPPED"

pattern TaskStatus_STARTED :: TaskStatus
pattern TaskStatus_STARTED = TaskStatus' "STARTED"

{-# COMPLETE
  TaskStatus_CANCELLED,
  TaskStatus_COMPLETED,
  TaskStatus_ERROR,
  TaskStatus_IN_PROGRESS,
  TaskStatus_SCHEDULED,
  TaskStatus_SKIPPED,
  TaskStatus_STARTED,
  TaskStatus'
  #-}
