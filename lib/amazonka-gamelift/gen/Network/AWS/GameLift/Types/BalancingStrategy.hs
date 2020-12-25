{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.BalancingStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.BalancingStrategy
  ( BalancingStrategy
      ( BalancingStrategy',
        BalancingStrategySpotOnly,
        BalancingStrategySpotPreferred,
        BalancingStrategyOnDemandOnly,
        fromBalancingStrategy
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype BalancingStrategy = BalancingStrategy'
  { fromBalancingStrategy ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern BalancingStrategySpotOnly :: BalancingStrategy
pattern BalancingStrategySpotOnly = BalancingStrategy' "SPOT_ONLY"

pattern BalancingStrategySpotPreferred :: BalancingStrategy
pattern BalancingStrategySpotPreferred = BalancingStrategy' "SPOT_PREFERRED"

pattern BalancingStrategyOnDemandOnly :: BalancingStrategy
pattern BalancingStrategyOnDemandOnly = BalancingStrategy' "ON_DEMAND_ONLY"

{-# COMPLETE
  BalancingStrategySpotOnly,
  BalancingStrategySpotPreferred,
  BalancingStrategyOnDemandOnly,
  BalancingStrategy'
  #-}
