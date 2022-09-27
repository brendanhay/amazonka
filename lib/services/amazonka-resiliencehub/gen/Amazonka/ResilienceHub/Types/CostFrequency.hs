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
-- Module      : Amazonka.ResilienceHub.Types.CostFrequency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.CostFrequency
  ( CostFrequency
      ( ..,
        CostFrequency_Daily,
        CostFrequency_Hourly,
        CostFrequency_Monthly,
        CostFrequency_Yearly
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CostFrequency = CostFrequency'
  { fromCostFrequency ::
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

pattern CostFrequency_Daily :: CostFrequency
pattern CostFrequency_Daily = CostFrequency' "Daily"

pattern CostFrequency_Hourly :: CostFrequency
pattern CostFrequency_Hourly = CostFrequency' "Hourly"

pattern CostFrequency_Monthly :: CostFrequency
pattern CostFrequency_Monthly = CostFrequency' "Monthly"

pattern CostFrequency_Yearly :: CostFrequency
pattern CostFrequency_Yearly = CostFrequency' "Yearly"

{-# COMPLETE
  CostFrequency_Daily,
  CostFrequency_Hourly,
  CostFrequency_Monthly,
  CostFrequency_Yearly,
  CostFrequency'
  #-}
