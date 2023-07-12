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
-- Module      : Amazonka.IoT.Types.TaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TaskStatus
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
