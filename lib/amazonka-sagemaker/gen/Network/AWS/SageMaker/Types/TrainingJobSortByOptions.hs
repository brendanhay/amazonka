-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobSortByOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobSortByOptions
  ( TrainingJobSortByOptions
      ( TrainingJobSortByOptions',
        TJSBOCreationTime,
        TJSBOFinalObjectiveMetricValue,
        TJSBOName,
        TJSBOStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TrainingJobSortByOptions = TrainingJobSortByOptions' Lude.Text
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

pattern TJSBOCreationTime :: TrainingJobSortByOptions
pattern TJSBOCreationTime = TrainingJobSortByOptions' "CreationTime"

pattern TJSBOFinalObjectiveMetricValue :: TrainingJobSortByOptions
pattern TJSBOFinalObjectiveMetricValue = TrainingJobSortByOptions' "FinalObjectiveMetricValue"

pattern TJSBOName :: TrainingJobSortByOptions
pattern TJSBOName = TrainingJobSortByOptions' "Name"

pattern TJSBOStatus :: TrainingJobSortByOptions
pattern TJSBOStatus = TrainingJobSortByOptions' "Status"

{-# COMPLETE
  TJSBOCreationTime,
  TJSBOFinalObjectiveMetricValue,
  TJSBOName,
  TJSBOStatus,
  TrainingJobSortByOptions'
  #-}
