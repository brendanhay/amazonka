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
-- Module      : Network.AWS.CostExplorer.Types.PaymentOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.PaymentOption
  ( PaymentOption
      ( ..,
        PaymentOption_ALL_UPFRONT,
        PaymentOption_HEAVY_UTILIZATION,
        PaymentOption_LIGHT_UTILIZATION,
        PaymentOption_MEDIUM_UTILIZATION,
        PaymentOption_NO_UPFRONT,
        PaymentOption_PARTIAL_UPFRONT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PaymentOption = PaymentOption'
  { fromPaymentOption ::
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

pattern PaymentOption_ALL_UPFRONT :: PaymentOption
pattern PaymentOption_ALL_UPFRONT = PaymentOption' "ALL_UPFRONT"

pattern PaymentOption_HEAVY_UTILIZATION :: PaymentOption
pattern PaymentOption_HEAVY_UTILIZATION = PaymentOption' "HEAVY_UTILIZATION"

pattern PaymentOption_LIGHT_UTILIZATION :: PaymentOption
pattern PaymentOption_LIGHT_UTILIZATION = PaymentOption' "LIGHT_UTILIZATION"

pattern PaymentOption_MEDIUM_UTILIZATION :: PaymentOption
pattern PaymentOption_MEDIUM_UTILIZATION = PaymentOption' "MEDIUM_UTILIZATION"

pattern PaymentOption_NO_UPFRONT :: PaymentOption
pattern PaymentOption_NO_UPFRONT = PaymentOption' "NO_UPFRONT"

pattern PaymentOption_PARTIAL_UPFRONT :: PaymentOption
pattern PaymentOption_PARTIAL_UPFRONT = PaymentOption' "PARTIAL_UPFRONT"

{-# COMPLETE
  PaymentOption_ALL_UPFRONT,
  PaymentOption_HEAVY_UTILIZATION,
  PaymentOption_LIGHT_UTILIZATION,
  PaymentOption_MEDIUM_UTILIZATION,
  PaymentOption_NO_UPFRONT,
  PaymentOption_PARTIAL_UPFRONT,
  PaymentOption'
  #-}
