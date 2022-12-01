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
-- Module      : Amazonka.EC2.Types.AllocationStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AllocationStrategy
  ( AllocationStrategy
      ( ..,
        AllocationStrategy_CapacityOptimized,
        AllocationStrategy_CapacityOptimizedPrioritized,
        AllocationStrategy_Diversified,
        AllocationStrategy_LowestPrice,
        AllocationStrategy_PriceCapacityOptimized
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AllocationStrategy = AllocationStrategy'
  { fromAllocationStrategy ::
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

pattern AllocationStrategy_CapacityOptimized :: AllocationStrategy
pattern AllocationStrategy_CapacityOptimized = AllocationStrategy' "capacityOptimized"

pattern AllocationStrategy_CapacityOptimizedPrioritized :: AllocationStrategy
pattern AllocationStrategy_CapacityOptimizedPrioritized = AllocationStrategy' "capacityOptimizedPrioritized"

pattern AllocationStrategy_Diversified :: AllocationStrategy
pattern AllocationStrategy_Diversified = AllocationStrategy' "diversified"

pattern AllocationStrategy_LowestPrice :: AllocationStrategy
pattern AllocationStrategy_LowestPrice = AllocationStrategy' "lowestPrice"

pattern AllocationStrategy_PriceCapacityOptimized :: AllocationStrategy
pattern AllocationStrategy_PriceCapacityOptimized = AllocationStrategy' "priceCapacityOptimized"

{-# COMPLETE
  AllocationStrategy_CapacityOptimized,
  AllocationStrategy_CapacityOptimizedPrioritized,
  AllocationStrategy_Diversified,
  AllocationStrategy_LowestPrice,
  AllocationStrategy_PriceCapacityOptimized,
  AllocationStrategy'
  #-}
