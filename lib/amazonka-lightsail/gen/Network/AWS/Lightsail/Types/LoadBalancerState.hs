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
        LBSActive,
        LBSActiveImpaired,
        LBSFailed,
        LBSProvisioning,
        LBSUnknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerState = LoadBalancerState' Lude.Text
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

pattern LBSActive :: LoadBalancerState
pattern LBSActive = LoadBalancerState' "active"

pattern LBSActiveImpaired :: LoadBalancerState
pattern LBSActiveImpaired = LoadBalancerState' "active_impaired"

pattern LBSFailed :: LoadBalancerState
pattern LBSFailed = LoadBalancerState' "failed"

pattern LBSProvisioning :: LoadBalancerState
pattern LBSProvisioning = LoadBalancerState' "provisioning"

pattern LBSUnknown :: LoadBalancerState
pattern LBSUnknown = LoadBalancerState' "unknown"

{-# COMPLETE
  LBSActive,
  LBSActiveImpaired,
  LBSFailed,
  LBSProvisioning,
  LBSUnknown,
  LoadBalancerState'
  #-}
