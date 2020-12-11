-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CandidateSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CandidateSortBy
  ( CandidateSortBy
      ( CandidateSortBy',
        CSBCreationTime,
        CSBFinalObjectiveMetricValue,
        CSBStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CandidateSortBy = CandidateSortBy' Lude.Text
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

pattern CSBCreationTime :: CandidateSortBy
pattern CSBCreationTime = CandidateSortBy' "CreationTime"

pattern CSBFinalObjectiveMetricValue :: CandidateSortBy
pattern CSBFinalObjectiveMetricValue = CandidateSortBy' "FinalObjectiveMetricValue"

pattern CSBStatus :: CandidateSortBy
pattern CSBStatus = CandidateSortBy' "Status"

{-# COMPLETE
  CSBCreationTime,
  CSBFinalObjectiveMetricValue,
  CSBStatus,
  CandidateSortBy'
  #-}
