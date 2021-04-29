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
-- Module      : Network.AWS.Glue.Types.TaskType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskType
  ( TaskType
      ( ..,
        TaskType_EVALUATION,
        TaskType_EXPORT_LABELS,
        TaskType_FIND_MATCHES,
        TaskType_IMPORT_LABELS,
        TaskType_LABELING_SET_GENERATION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TaskType = TaskType'
  { fromTaskType ::
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

pattern TaskType_EVALUATION :: TaskType
pattern TaskType_EVALUATION = TaskType' "EVALUATION"

pattern TaskType_EXPORT_LABELS :: TaskType
pattern TaskType_EXPORT_LABELS = TaskType' "EXPORT_LABELS"

pattern TaskType_FIND_MATCHES :: TaskType
pattern TaskType_FIND_MATCHES = TaskType' "FIND_MATCHES"

pattern TaskType_IMPORT_LABELS :: TaskType
pattern TaskType_IMPORT_LABELS = TaskType' "IMPORT_LABELS"

pattern TaskType_LABELING_SET_GENERATION :: TaskType
pattern TaskType_LABELING_SET_GENERATION = TaskType' "LABELING_SET_GENERATION"

{-# COMPLETE
  TaskType_EVALUATION,
  TaskType_EXPORT_LABELS,
  TaskType_FIND_MATCHES,
  TaskType_IMPORT_LABELS,
  TaskType_LABELING_SET_GENERATION,
  TaskType'
  #-}
