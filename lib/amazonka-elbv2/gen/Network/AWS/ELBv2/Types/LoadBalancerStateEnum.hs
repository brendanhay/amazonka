{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerStateEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerStateEnum
  ( LoadBalancerStateEnum
      ( LoadBalancerStateEnum',
        LoadBalancerStateEnumActive,
        LoadBalancerStateEnumProvisioning,
        LoadBalancerStateEnumActiveImpaired,
        LoadBalancerStateEnumFailed,
        fromLoadBalancerStateEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LoadBalancerStateEnum = LoadBalancerStateEnum'
  { fromLoadBalancerStateEnum ::
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

pattern LoadBalancerStateEnumActive :: LoadBalancerStateEnum
pattern LoadBalancerStateEnumActive = LoadBalancerStateEnum' "active"

pattern LoadBalancerStateEnumProvisioning :: LoadBalancerStateEnum
pattern LoadBalancerStateEnumProvisioning = LoadBalancerStateEnum' "provisioning"

pattern LoadBalancerStateEnumActiveImpaired :: LoadBalancerStateEnum
pattern LoadBalancerStateEnumActiveImpaired = LoadBalancerStateEnum' "active_impaired"

pattern LoadBalancerStateEnumFailed :: LoadBalancerStateEnum
pattern LoadBalancerStateEnumFailed = LoadBalancerStateEnum' "failed"

{-# COMPLETE
  LoadBalancerStateEnumActive,
  LoadBalancerStateEnumProvisioning,
  LoadBalancerStateEnumActiveImpaired,
  LoadBalancerStateEnumFailed,
  LoadBalancerStateEnum'
  #-}
