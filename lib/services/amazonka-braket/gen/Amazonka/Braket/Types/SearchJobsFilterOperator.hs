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
-- Module      : Amazonka.Braket.Types.SearchJobsFilterOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.SearchJobsFilterOperator
  ( SearchJobsFilterOperator
      ( ..,
        SearchJobsFilterOperator_BETWEEN,
        SearchJobsFilterOperator_CONTAINS,
        SearchJobsFilterOperator_EQUAL,
        SearchJobsFilterOperator_GT,
        SearchJobsFilterOperator_GTE,
        SearchJobsFilterOperator_LT,
        SearchJobsFilterOperator_LTE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SearchJobsFilterOperator = SearchJobsFilterOperator'
  { fromSearchJobsFilterOperator ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern SearchJobsFilterOperator_BETWEEN :: SearchJobsFilterOperator
pattern SearchJobsFilterOperator_BETWEEN = SearchJobsFilterOperator' "BETWEEN"

pattern SearchJobsFilterOperator_CONTAINS :: SearchJobsFilterOperator
pattern SearchJobsFilterOperator_CONTAINS = SearchJobsFilterOperator' "CONTAINS"

pattern SearchJobsFilterOperator_EQUAL :: SearchJobsFilterOperator
pattern SearchJobsFilterOperator_EQUAL = SearchJobsFilterOperator' "EQUAL"

pattern SearchJobsFilterOperator_GT :: SearchJobsFilterOperator
pattern SearchJobsFilterOperator_GT = SearchJobsFilterOperator' "GT"

pattern SearchJobsFilterOperator_GTE :: SearchJobsFilterOperator
pattern SearchJobsFilterOperator_GTE = SearchJobsFilterOperator' "GTE"

pattern SearchJobsFilterOperator_LT :: SearchJobsFilterOperator
pattern SearchJobsFilterOperator_LT = SearchJobsFilterOperator' "LT"

pattern SearchJobsFilterOperator_LTE :: SearchJobsFilterOperator
pattern SearchJobsFilterOperator_LTE = SearchJobsFilterOperator' "LTE"

{-# COMPLETE
  SearchJobsFilterOperator_BETWEEN,
  SearchJobsFilterOperator_CONTAINS,
  SearchJobsFilterOperator_EQUAL,
  SearchJobsFilterOperator_GT,
  SearchJobsFilterOperator_GTE,
  SearchJobsFilterOperator_LT,
  SearchJobsFilterOperator_LTE,
  SearchJobsFilterOperator'
  #-}
