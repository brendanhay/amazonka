-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
  ( LoadBalancerTypeEnum
      ( LoadBalancerTypeEnum',
        Application,
        Gateway,
        Network
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerTypeEnum = LoadBalancerTypeEnum' Lude.Text
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

pattern Application :: LoadBalancerTypeEnum
pattern Application = LoadBalancerTypeEnum' "application"

pattern Gateway :: LoadBalancerTypeEnum
pattern Gateway = LoadBalancerTypeEnum' "gateway"

pattern Network :: LoadBalancerTypeEnum
pattern Network = LoadBalancerTypeEnum' "network"

{-# COMPLETE
  Application,
  Gateway,
  Network,
  LoadBalancerTypeEnum'
  #-}
