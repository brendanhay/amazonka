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
-- Module      : Amazonka.Route53Resolver.Types.RuleTypeOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.RuleTypeOption
  ( RuleTypeOption
      ( ..,
        RuleTypeOption_FORWARD,
        RuleTypeOption_RECURSIVE,
        RuleTypeOption_SYSTEM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RuleTypeOption = RuleTypeOption'
  { fromRuleTypeOption ::
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

pattern RuleTypeOption_FORWARD :: RuleTypeOption
pattern RuleTypeOption_FORWARD = RuleTypeOption' "FORWARD"

pattern RuleTypeOption_RECURSIVE :: RuleTypeOption
pattern RuleTypeOption_RECURSIVE = RuleTypeOption' "RECURSIVE"

pattern RuleTypeOption_SYSTEM :: RuleTypeOption
pattern RuleTypeOption_SYSTEM = RuleTypeOption' "SYSTEM"

{-# COMPLETE
  RuleTypeOption_FORWARD,
  RuleTypeOption_RECURSIVE,
  RuleTypeOption_SYSTEM,
  RuleTypeOption'
  #-}
