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
-- Module      : Network.AWS.DeviceFarm.Types.RuleOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RuleOperator
  ( RuleOperator
      ( ..,
        RuleOperator_CONTAINS,
        RuleOperator_EQUALS,
        RuleOperator_GREATER_THAN,
        RuleOperator_GREATER_THAN_OR_EQUALS,
        RuleOperator_IN,
        RuleOperator_LESS_THAN,
        RuleOperator_LESS_THAN_OR_EQUALS,
        RuleOperator_NOT_IN
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RuleOperator = RuleOperator'
  { fromRuleOperator ::
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

pattern RuleOperator_CONTAINS :: RuleOperator
pattern RuleOperator_CONTAINS = RuleOperator' "CONTAINS"

pattern RuleOperator_EQUALS :: RuleOperator
pattern RuleOperator_EQUALS = RuleOperator' "EQUALS"

pattern RuleOperator_GREATER_THAN :: RuleOperator
pattern RuleOperator_GREATER_THAN = RuleOperator' "GREATER_THAN"

pattern RuleOperator_GREATER_THAN_OR_EQUALS :: RuleOperator
pattern RuleOperator_GREATER_THAN_OR_EQUALS = RuleOperator' "GREATER_THAN_OR_EQUALS"

pattern RuleOperator_IN :: RuleOperator
pattern RuleOperator_IN = RuleOperator' "IN"

pattern RuleOperator_LESS_THAN :: RuleOperator
pattern RuleOperator_LESS_THAN = RuleOperator' "LESS_THAN"

pattern RuleOperator_LESS_THAN_OR_EQUALS :: RuleOperator
pattern RuleOperator_LESS_THAN_OR_EQUALS = RuleOperator' "LESS_THAN_OR_EQUALS"

pattern RuleOperator_NOT_IN :: RuleOperator
pattern RuleOperator_NOT_IN = RuleOperator' "NOT_IN"

{-# COMPLETE
  RuleOperator_CONTAINS,
  RuleOperator_EQUALS,
  RuleOperator_GREATER_THAN,
  RuleOperator_GREATER_THAN_OR_EQUALS,
  RuleOperator_IN,
  RuleOperator_LESS_THAN,
  RuleOperator_LESS_THAN_OR_EQUALS,
  RuleOperator_NOT_IN,
  RuleOperator'
  #-}
