{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskType
  ( TaskType
      ( TaskType',
        TaskTypeEvaluation,
        TaskTypeLabelingSetGeneration,
        TaskTypeImportLabels,
        TaskTypeExportLabels,
        TaskTypeFindMatches,
        fromTaskType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TaskType = TaskType' {fromTaskType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TaskTypeEvaluation :: TaskType
pattern TaskTypeEvaluation = TaskType' "EVALUATION"

pattern TaskTypeLabelingSetGeneration :: TaskType
pattern TaskTypeLabelingSetGeneration = TaskType' "LABELING_SET_GENERATION"

pattern TaskTypeImportLabels :: TaskType
pattern TaskTypeImportLabels = TaskType' "IMPORT_LABELS"

pattern TaskTypeExportLabels :: TaskType
pattern TaskTypeExportLabels = TaskType' "EXPORT_LABELS"

pattern TaskTypeFindMatches :: TaskType
pattern TaskTypeFindMatches = TaskType' "FIND_MATCHES"

{-# COMPLETE
  TaskTypeEvaluation,
  TaskTypeLabelingSetGeneration,
  TaskTypeImportLabels,
  TaskTypeExportLabels,
  TaskTypeFindMatches,
  TaskType'
  #-}
