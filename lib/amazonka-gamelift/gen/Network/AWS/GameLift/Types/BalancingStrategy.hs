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
        OnDemandOnly,
        SpotOnly,
        SpotPreferred
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BalancingStrategy = BalancingStrategy' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OnDemandOnly :: BalancingStrategy
pattern OnDemandOnly = BalancingStrategy' "ON_DEMAND_ONLY"

pattern SpotOnly :: BalancingStrategy
pattern SpotOnly = BalancingStrategy' "SPOT_ONLY"

pattern SpotPreferred :: BalancingStrategy
pattern SpotPreferred = BalancingStrategy' "SPOT_PREFERRED"

{-# COMPLETE
  OnDemandOnly,
  SpotOnly,
  SpotPreferred,
  BalancingStrategy'
  #-}
