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
-- Module      : Network.AWS.SageMaker.Types.CandidateSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CandidateSortBy
  ( CandidateSortBy
      ( ..,
        CandidateSortBy_CreationTime,
        CandidateSortBy_FinalObjectiveMetricValue,
        CandidateSortBy_Status
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CandidateSortBy = CandidateSortBy'
  { fromCandidateSortBy ::
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
