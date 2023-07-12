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
-- Module      : Amazonka.IotTwinMaker.Types.PricingMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PricingMode
  ( PricingMode
      ( ..,
        PricingMode_BASIC,
        PricingMode_STANDARD,
        PricingMode_TIERED_BUNDLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PricingMode = PricingMode'
  { fromPricingMode ::
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

pattern PricingMode_BASIC :: PricingMode
pattern PricingMode_BASIC = PricingMode' "BASIC"

pattern PricingMode_STANDARD :: PricingMode
pattern PricingMode_STANDARD = PricingMode' "STANDARD"

pattern PricingMode_TIERED_BUNDLE :: PricingMode
pattern PricingMode_TIERED_BUNDLE = PricingMode' "TIERED_BUNDLE"

{-# COMPLETE
  PricingMode_BASIC,
  PricingMode_STANDARD,
  PricingMode_TIERED_BUNDLE,
  PricingMode'
  #-}
