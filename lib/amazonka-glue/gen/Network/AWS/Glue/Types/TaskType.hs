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
        Evaluation,
        LabelingSetGeneration,
        ImportLabels,
        ExportLabels,
        FindMatches
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

pattern Evaluation :: TaskType
pattern Evaluation = TaskType' "EVALUATION"

pattern LabelingSetGeneration :: TaskType
pattern LabelingSetGeneration = TaskType' "LABELING_SET_GENERATION"

pattern ImportLabels :: TaskType
pattern ImportLabels = TaskType' "IMPORT_LABELS"

pattern ExportLabels :: TaskType
pattern ExportLabels = TaskType' "EXPORT_LABELS"

pattern FindMatches :: TaskType
pattern FindMatches = TaskType' "FIND_MATCHES"

{-# COMPLETE
  Evaluation,
  LabelingSetGeneration,
  ImportLabels,
  ExportLabels,
  FindMatches,
  TaskType'
  #-}
