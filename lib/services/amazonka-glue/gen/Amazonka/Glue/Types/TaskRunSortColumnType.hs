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
-- Module      : Amazonka.Glue.Types.TaskRunSortColumnType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TaskRunSortColumnType
  ( TaskRunSortColumnType
      ( ..,
        TaskRunSortColumnType_STARTED,
        TaskRunSortColumnType_STATUS,
        TaskRunSortColumnType_TASK_RUN_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaskRunSortColumnType = TaskRunSortColumnType'
  { fromTaskRunSortColumnType ::
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

pattern TaskRunSortColumnType_STARTED :: TaskRunSortColumnType
pattern TaskRunSortColumnType_STARTED = TaskRunSortColumnType' "STARTED"

pattern TaskRunSortColumnType_STATUS :: TaskRunSortColumnType
pattern TaskRunSortColumnType_STATUS = TaskRunSortColumnType' "STATUS"

pattern TaskRunSortColumnType_TASK_RUN_TYPE :: TaskRunSortColumnType
pattern TaskRunSortColumnType_TASK_RUN_TYPE = TaskRunSortColumnType' "TASK_RUN_TYPE"

{-# COMPLETE
  TaskRunSortColumnType_STARTED,
  TaskRunSortColumnType_STATUS,
  TaskRunSortColumnType_TASK_RUN_TYPE,
  TaskRunSortColumnType'
  #-}
