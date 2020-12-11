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
        TTEvaluation,
        TTExportLabels,
        TTFindMatches,
        TTImportLabels,
        TTLabelingSetGeneration
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TaskType = TaskType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern TTEvaluation :: TaskType
pattern TTEvaluation = TaskType' "EVALUATION"

pattern TTExportLabels :: TaskType
pattern TTExportLabels = TaskType' "EXPORT_LABELS"

pattern TTFindMatches :: TaskType
pattern TTFindMatches = TaskType' "FIND_MATCHES"

pattern TTImportLabels :: TaskType
pattern TTImportLabels = TaskType' "IMPORT_LABELS"

pattern TTLabelingSetGeneration :: TaskType
pattern TTLabelingSetGeneration = TaskType' "LABELING_SET_GENERATION"

{-# COMPLETE
  TTEvaluation,
  TTExportLabels,
  TTFindMatches,
  TTImportLabels,
  TTLabelingSetGeneration,
  TaskType'
  #-}
