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
-- Module      : Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
  ( FleetOnDemandAllocationStrategy
      ( ..,
        FleetOnDemandAllocationStrategy_Lowest_price,
        FleetOnDemandAllocationStrategy_Prioritized
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype FleetOnDemandAllocationStrategy = FleetOnDemandAllocationStrategy'
  { fromFleetOnDemandAllocationStrategy ::
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

pattern FleetOnDemandAllocationStrategy_Lowest_price :: FleetOnDemandAllocationStrategy
pattern FleetOnDemandAllocationStrategy_Lowest_price = FleetOnDemandAllocationStrategy' "lowest-price"

pattern FleetOnDemandAllocationStrategy_Prioritized :: FleetOnDemandAllocationStrategy
pattern FleetOnDemandAllocationStrategy_Prioritized = FleetOnDemandAllocationStrategy' "prioritized"

{-# COMPLETE
  FleetOnDemandAllocationStrategy_Lowest_price,
  FleetOnDemandAllocationStrategy_Prioritized,
  FleetOnDemandAllocationStrategy'
  #-}
