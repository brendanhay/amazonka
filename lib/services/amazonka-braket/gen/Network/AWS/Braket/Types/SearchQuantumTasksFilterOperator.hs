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
-- Module      : Network.AWS.Braket.Types.SearchQuantumTasksFilterOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Braket.Types.SearchQuantumTasksFilterOperator
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SearchQuantumTasksFilterOperator = SearchQuantumTasksFilterOperator'
  { fromSearchQuantumTasksFilterOperator ::
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
