{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerState
  ( LoadBalancerState
      ( LoadBalancerState',
        LoadBalancerStateActive,
        LoadBalancerStateProvisioning,
        LoadBalancerStateActiveImpaired,
        LoadBalancerStateFailed,
        LoadBalancerStateUnknown,
        fromLoadBalancerState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LoadBalancerState = LoadBalancerState'
  { fromLoadBalancerState ::
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

pattern LoadBalancerStateActive :: LoadBalancerState
pattern LoadBalancerStateActive = LoadBalancerState' "active"

pattern LoadBalancerStateProvisioning :: LoadBalancerState
pattern LoadBalancerStateProvisioning = LoadBalancerState' "provisioning"

pattern LoadBalancerStateActiveImpaired :: LoadBalancerState
pattern LoadBalancerStateActiveImpaired = LoadBalancerState' "active_impaired"

pattern LoadBalancerStateFailed :: LoadBalancerState
pattern LoadBalancerStateFailed = LoadBalancerState' "failed"

pattern LoadBalancerStateUnknown :: LoadBalancerState
pattern LoadBalancerStateUnknown = LoadBalancerState' "unknown"

{-# COMPLETE
  LoadBalancerStateActive,
  LoadBalancerStateProvisioning,
  LoadBalancerStateActiveImpaired,
  LoadBalancerStateFailed,
  LoadBalancerStateUnknown,
  LoadBalancerState'
  #-}
