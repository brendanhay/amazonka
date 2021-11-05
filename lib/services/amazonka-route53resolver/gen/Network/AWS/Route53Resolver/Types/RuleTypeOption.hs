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
-- Module      : Network.AWS.Route53Resolver.Types.RuleTypeOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Resolver.Types.RuleTypeOption
  ( RuleTypeOption
      ( ..,
        RuleTypeOption_FORWARD,
        RuleTypeOption_RECURSIVE,
        RuleTypeOption_SYSTEM
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RuleTypeOption = RuleTypeOption'
  { fromRuleTypeOption ::
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
