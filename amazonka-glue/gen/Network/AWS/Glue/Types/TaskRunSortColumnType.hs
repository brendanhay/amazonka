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
-- Module      : Network.AWS.Glue.Types.TaskRunSortColumnType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunSortColumnType
  ( TaskRunSortColumnType
      ( ..,
        TaskRunSortColumnType_STARTED,
        TaskRunSortColumnType_STATUS,
        TaskRunSortColumnType_TASK_RUN_TYPE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TaskRunSortColumnType = TaskRunSortColumnType'
  { fromTaskRunSortColumnType ::
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
