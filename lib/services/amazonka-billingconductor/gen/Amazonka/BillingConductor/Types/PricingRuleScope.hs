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
-- Module      : Amazonka.BillingConductor.Types.PricingRuleScope
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.PricingRuleScope
  ( PricingRuleScope
      ( ..,
        PricingRuleScope_BILLING_ENTITY,
        PricingRuleScope_GLOBAL,
        PricingRuleScope_SERVICE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PricingRuleScope = PricingRuleScope'
  { fromPricingRuleScope ::
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

pattern PricingRuleScope_BILLING_ENTITY :: PricingRuleScope
pattern PricingRuleScope_BILLING_ENTITY = PricingRuleScope' "BILLING_ENTITY"

pattern PricingRuleScope_GLOBAL :: PricingRuleScope
pattern PricingRuleScope_GLOBAL = PricingRuleScope' "GLOBAL"

pattern PricingRuleScope_SERVICE :: PricingRuleScope
pattern PricingRuleScope_SERVICE = PricingRuleScope' "SERVICE"

{-# COMPLETE
  PricingRuleScope_BILLING_ENTITY,
  PricingRuleScope_GLOBAL,
  PricingRuleScope_SERVICE,
  PricingRuleScope'
  #-}
