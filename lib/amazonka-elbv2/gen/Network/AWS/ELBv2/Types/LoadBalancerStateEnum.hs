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
        Active,
        ActiveImpaired,
        Failed,
        Provisioning
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerStateEnum = LoadBalancerStateEnum' Lude.Text
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

pattern Active :: LoadBalancerStateEnum
pattern Active = LoadBalancerStateEnum' "active"

pattern ActiveImpaired :: LoadBalancerStateEnum
pattern ActiveImpaired = LoadBalancerStateEnum' "active_impaired"

pattern Failed :: LoadBalancerStateEnum
pattern Failed = LoadBalancerStateEnum' "failed"

pattern Provisioning :: LoadBalancerStateEnum
pattern Provisioning = LoadBalancerStateEnum' "provisioning"

{-# COMPLETE
  Active,
  ActiveImpaired,
  Failed,
  Provisioning,
  LoadBalancerStateEnum'
  #-}
