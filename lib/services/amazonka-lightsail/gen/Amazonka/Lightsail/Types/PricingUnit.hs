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
-- Module      : Amazonka.Lightsail.Types.PricingUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.PricingUnit
  ( PricingUnit
      ( ..,
        PricingUnit_Bundles,
        PricingUnit_GB,
        PricingUnit_GB_Mo,
        PricingUnit_Hrs,
        PricingUnit_Queries
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PricingUnit = PricingUnit'
  { fromPricingUnit ::
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

pattern PricingUnit_Bundles :: PricingUnit
pattern PricingUnit_Bundles = PricingUnit' "Bundles"

pattern PricingUnit_GB :: PricingUnit
pattern PricingUnit_GB = PricingUnit' "GB"

pattern PricingUnit_GB_Mo :: PricingUnit
pattern PricingUnit_GB_Mo = PricingUnit' "GB-Mo"

pattern PricingUnit_Hrs :: PricingUnit
pattern PricingUnit_Hrs = PricingUnit' "Hrs"

pattern PricingUnit_Queries :: PricingUnit
pattern PricingUnit_Queries = PricingUnit' "Queries"

{-# COMPLETE
  PricingUnit_Bundles,
  PricingUnit_GB,
  PricingUnit_GB_Mo,
  PricingUnit_Hrs,
  PricingUnit_Queries,
  PricingUnit'
  #-}
