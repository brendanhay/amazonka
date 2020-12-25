{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        CandidateSortByCreationTime,
        CandidateSortByStatus,
        CandidateSortByFinalObjectiveMetricValue,
        fromCandidateSortBy
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CandidateSortBy = CandidateSortBy'
  { fromCandidateSortBy ::
      Core.Text
  }
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

pattern CandidateSortByCreationTime :: CandidateSortBy
pattern CandidateSortByCreationTime = CandidateSortBy' "CreationTime"

pattern CandidateSortByStatus :: CandidateSortBy
pattern CandidateSortByStatus = CandidateSortBy' "Status"

pattern CandidateSortByFinalObjectiveMetricValue :: CandidateSortBy
pattern CandidateSortByFinalObjectiveMetricValue = CandidateSortBy' "FinalObjectiveMetricValue"

{-# COMPLETE
  CandidateSortByCreationTime,
  CandidateSortByStatus,
  CandidateSortByFinalObjectiveMetricValue,
  CandidateSortBy'
  #-}
