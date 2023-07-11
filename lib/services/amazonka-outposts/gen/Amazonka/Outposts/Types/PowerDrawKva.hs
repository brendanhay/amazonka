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
-- Module      : Amazonka.Outposts.Types.PowerDrawKva
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.PowerDrawKva
  ( PowerDrawKva
      ( ..,
        PowerDrawKva_POWER_10_KVA,
        PowerDrawKva_POWER_15_KVA,
        PowerDrawKva_POWER_5_KVA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PowerDrawKva = PowerDrawKva'
  { fromPowerDrawKva ::
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

pattern PowerDrawKva_POWER_10_KVA :: PowerDrawKva
pattern PowerDrawKva_POWER_10_KVA = PowerDrawKva' "POWER_10_KVA"

pattern PowerDrawKva_POWER_15_KVA :: PowerDrawKva
pattern PowerDrawKva_POWER_15_KVA = PowerDrawKva' "POWER_15_KVA"

pattern PowerDrawKva_POWER_5_KVA :: PowerDrawKva
pattern PowerDrawKva_POWER_5_KVA = PowerDrawKva' "POWER_5_KVA"

{-# COMPLETE
  PowerDrawKva_POWER_10_KVA,
  PowerDrawKva_POWER_15_KVA,
  PowerDrawKva_POWER_5_KVA,
  PowerDrawKva'
  #-}
