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
-- Module      : Amazonka.Braket.Types.SearchQuantumTasksFilterOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.SearchQuantumTasksFilterOperator
  ( SearchQuantumTasksFilterOperator
      ( ..,
        SearchQuantumTasksFilterOperator_BETWEEN,
        SearchQuantumTasksFilterOperator_EQUAL,
        SearchQuantumTasksFilterOperator_GT,
        SearchQuantumTasksFilterOperator_GTE,
        SearchQuantumTasksFilterOperator_LT,
        SearchQuantumTasksFilterOperator_LTE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SearchQuantumTasksFilterOperator = SearchQuantumTasksFilterOperator'
  { fromSearchQuantumTasksFilterOperator ::
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

pattern SearchQuantumTasksFilterOperator_BETWEEN :: SearchQuantumTasksFilterOperator
pattern SearchQuantumTasksFilterOperator_BETWEEN = SearchQuantumTasksFilterOperator' "BETWEEN"

pattern SearchQuantumTasksFilterOperator_EQUAL :: SearchQuantumTasksFilterOperator
pattern SearchQuantumTasksFilterOperator_EQUAL = SearchQuantumTasksFilterOperator' "EQUAL"

pattern SearchQuantumTasksFilterOperator_GT :: SearchQuantumTasksFilterOperator
pattern SearchQuantumTasksFilterOperator_GT = SearchQuantumTasksFilterOperator' "GT"

pattern SearchQuantumTasksFilterOperator_GTE :: SearchQuantumTasksFilterOperator
pattern SearchQuantumTasksFilterOperator_GTE = SearchQuantumTasksFilterOperator' "GTE"

pattern SearchQuantumTasksFilterOperator_LT :: SearchQuantumTasksFilterOperator
pattern SearchQuantumTasksFilterOperator_LT = SearchQuantumTasksFilterOperator' "LT"

pattern SearchQuantumTasksFilterOperator_LTE :: SearchQuantumTasksFilterOperator
pattern SearchQuantumTasksFilterOperator_LTE = SearchQuantumTasksFilterOperator' "LTE"

{-# COMPLETE
  SearchQuantumTasksFilterOperator_BETWEEN,
  SearchQuantumTasksFilterOperator_EQUAL,
  SearchQuantumTasksFilterOperator_GT,
  SearchQuantumTasksFilterOperator_GTE,
  SearchQuantumTasksFilterOperator_LT,
  SearchQuantumTasksFilterOperator_LTE,
  SearchQuantumTasksFilterOperator'
  #-}
