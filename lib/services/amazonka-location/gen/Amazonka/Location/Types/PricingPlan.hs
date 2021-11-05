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
-- Module      : Amazonka.Location.Types.PricingPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.PricingPlan
  ( PricingPlan
      ( ..,
        PricingPlan_MobileAssetManagement,
        PricingPlan_MobileAssetTracking,
        PricingPlan_RequestBasedUsage
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PricingPlan = PricingPlan'
  { fromPricingPlan ::
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

pattern PricingPlan_MobileAssetManagement :: PricingPlan
pattern PricingPlan_MobileAssetManagement = PricingPlan' "MobileAssetManagement"

pattern PricingPlan_MobileAssetTracking :: PricingPlan
pattern PricingPlan_MobileAssetTracking = PricingPlan' "MobileAssetTracking"

pattern PricingPlan_RequestBasedUsage :: PricingPlan
pattern PricingPlan_RequestBasedUsage = PricingPlan' "RequestBasedUsage"

{-# COMPLETE
  PricingPlan_MobileAssetManagement,
  PricingPlan_MobileAssetTracking,
  PricingPlan_RequestBasedUsage,
  PricingPlan'
  #-}
