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
-- Module      : Network.AWS.EC2.Types.SpotAllocationStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotAllocationStrategy
  ( SpotAllocationStrategy
      ( ..,
        SpotAllocationStrategy_Capacity_optimized,
        SpotAllocationStrategy_Diversified,
        SpotAllocationStrategy_Lowest_price
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype SpotAllocationStrategy = SpotAllocationStrategy'
  { fromSpotAllocationStrategy ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern SpotAllocationStrategy_Capacity_optimized :: SpotAllocationStrategy
pattern SpotAllocationStrategy_Capacity_optimized = SpotAllocationStrategy' "capacity-optimized"

pattern SpotAllocationStrategy_Diversified :: SpotAllocationStrategy
pattern SpotAllocationStrategy_Diversified = SpotAllocationStrategy' "diversified"

pattern SpotAllocationStrategy_Lowest_price :: SpotAllocationStrategy
pattern SpotAllocationStrategy_Lowest_price = SpotAllocationStrategy' "lowest-price"

{-# COMPLETE
  SpotAllocationStrategy_Capacity_optimized,
  SpotAllocationStrategy_Diversified,
  SpotAllocationStrategy_Lowest_price,
  SpotAllocationStrategy'
  #-}
