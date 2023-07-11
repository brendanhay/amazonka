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
-- Module      : Amazonka.Kendra.Types.ConditionOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConditionOperator
  ( ConditionOperator
      ( ..,
        ConditionOperator_BeginsWith,
        ConditionOperator_Contains,
        ConditionOperator_Equals,
        ConditionOperator_Exists,
        ConditionOperator_GreaterThan,
        ConditionOperator_GreaterThanOrEquals,
        ConditionOperator_LessThan,
        ConditionOperator_LessThanOrEquals,
        ConditionOperator_NotContains,
        ConditionOperator_NotEquals,
        ConditionOperator_NotExists
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConditionOperator = ConditionOperator'
  { fromConditionOperator ::
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

pattern ConditionOperator_BeginsWith :: ConditionOperator
pattern ConditionOperator_BeginsWith = ConditionOperator' "BeginsWith"

pattern ConditionOperator_Contains :: ConditionOperator
pattern ConditionOperator_Contains = ConditionOperator' "Contains"

pattern ConditionOperator_Equals :: ConditionOperator
pattern ConditionOperator_Equals = ConditionOperator' "Equals"

pattern ConditionOperator_Exists :: ConditionOperator
pattern ConditionOperator_Exists = ConditionOperator' "Exists"

pattern ConditionOperator_GreaterThan :: ConditionOperator
pattern ConditionOperator_GreaterThan = ConditionOperator' "GreaterThan"

pattern ConditionOperator_GreaterThanOrEquals :: ConditionOperator
pattern ConditionOperator_GreaterThanOrEquals = ConditionOperator' "GreaterThanOrEquals"

pattern ConditionOperator_LessThan :: ConditionOperator
pattern ConditionOperator_LessThan = ConditionOperator' "LessThan"

pattern ConditionOperator_LessThanOrEquals :: ConditionOperator
pattern ConditionOperator_LessThanOrEquals = ConditionOperator' "LessThanOrEquals"

pattern ConditionOperator_NotContains :: ConditionOperator
pattern ConditionOperator_NotContains = ConditionOperator' "NotContains"

pattern ConditionOperator_NotEquals :: ConditionOperator
pattern ConditionOperator_NotEquals = ConditionOperator' "NotEquals"

pattern ConditionOperator_NotExists :: ConditionOperator
pattern ConditionOperator_NotExists = ConditionOperator' "NotExists"

{-# COMPLETE
  ConditionOperator_BeginsWith,
  ConditionOperator_Contains,
  ConditionOperator_Equals,
  ConditionOperator_Exists,
  ConditionOperator_GreaterThan,
  ConditionOperator_GreaterThanOrEquals,
  ConditionOperator_LessThan,
  ConditionOperator_LessThanOrEquals,
  ConditionOperator_NotContains,
  ConditionOperator_NotEquals,
  ConditionOperator_NotExists,
  ConditionOperator'
  #-}
