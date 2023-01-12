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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.RuleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.RuleType
  ( RuleType
      ( ..,
        RuleType_AND,
        RuleType_ATLEAST,
        RuleType_OR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An enumerated type that determines how the evaluated rules are
-- processed. RuleType can be one of the following:
--
-- ATLEAST - At least N routing controls must be set. You specify N as the
-- Threshold in the rule configuration.
--
-- AND - All routing controls must be set. This is a shortcut for \"At
-- least N,\" where N is the total number of controls in the rule.
--
-- OR - Any control must be set. This is a shortcut for \"At least N,\"
-- where N is 1.
newtype RuleType = RuleType'
  { fromRuleType ::
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

pattern RuleType_AND :: RuleType
pattern RuleType_AND = RuleType' "AND"

pattern RuleType_ATLEAST :: RuleType
pattern RuleType_ATLEAST = RuleType' "ATLEAST"

pattern RuleType_OR :: RuleType
pattern RuleType_OR = RuleType' "OR"

{-# COMPLETE
  RuleType_AND,
  RuleType_ATLEAST,
  RuleType_OR,
  RuleType'
  #-}
