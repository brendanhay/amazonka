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
-- Module      : Amazonka.SageMaker.Types.CandidateSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CandidateSortBy
  ( CandidateSortBy
      ( ..,
        CandidateSortBy_CreationTime,
        CandidateSortBy_FinalObjectiveMetricValue,
        CandidateSortBy_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CandidateSortBy = CandidateSortBy'
  { fromCandidateSortBy ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern CandidateSortBy_CreationTime :: CandidateSortBy
pattern CandidateSortBy_CreationTime = CandidateSortBy' "CreationTime"

pattern CandidateSortBy_FinalObjectiveMetricValue :: CandidateSortBy
pattern CandidateSortBy_FinalObjectiveMetricValue = CandidateSortBy' "FinalObjectiveMetricValue"

pattern CandidateSortBy_Status :: CandidateSortBy
pattern CandidateSortBy_Status = CandidateSortBy' "Status"

{-# COMPLETE
  CandidateSortBy_CreationTime,
  CandidateSortBy_FinalObjectiveMetricValue,
  CandidateSortBy_Status,
  CandidateSortBy'
  #-}
