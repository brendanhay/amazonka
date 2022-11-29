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
-- Module      : Amazonka.IotTwinMaker.Types.PricingTier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PricingTier
  ( PricingTier
      ( ..,
        PricingTier_TIER_1,
        PricingTier_TIER_2,
        PricingTier_TIER_3,
        PricingTier_TIER_4
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PricingTier = PricingTier'
  { fromPricingTier ::
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

pattern PricingTier_TIER_1 :: PricingTier
pattern PricingTier_TIER_1 = PricingTier' "TIER_1"

pattern PricingTier_TIER_2 :: PricingTier
pattern PricingTier_TIER_2 = PricingTier' "TIER_2"

pattern PricingTier_TIER_3 :: PricingTier
pattern PricingTier_TIER_3 = PricingTier' "TIER_3"

pattern PricingTier_TIER_4 :: PricingTier
pattern PricingTier_TIER_4 = PricingTier' "TIER_4"

{-# COMPLETE
  PricingTier_TIER_1,
  PricingTier_TIER_2,
  PricingTier_TIER_3,
  PricingTier_TIER_4,
  PricingTier'
  #-}
