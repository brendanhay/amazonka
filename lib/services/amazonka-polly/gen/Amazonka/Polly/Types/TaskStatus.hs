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
-- Module      : Amazonka.Polly.Types.TaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types.TaskStatus
  ( TaskStatus
      ( ..,
        TaskStatus_Completed,
        TaskStatus_Failed,
        TaskStatus_InProgress,
        TaskStatus_Scheduled
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
