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
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
  ( LoadBalancerTypeEnum
      ( ..,
        LoadBalancerTypeEnum_Application,
        LoadBalancerTypeEnum_Gateway,
        LoadBalancerTypeEnum_Network
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LoadBalancerTypeEnum = LoadBalancerTypeEnum'
  { fromLoadBalancerTypeEnum ::
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

pattern LoadBalancerTypeEnum_Application :: LoadBalancerTypeEnum
pattern LoadBalancerTypeEnum_Application = LoadBalancerTypeEnum' "application"

pattern LoadBalancerTypeEnum_Gateway :: LoadBalancerTypeEnum
pattern LoadBalancerTypeEnum_Gateway = LoadBalancerTypeEnum' "gateway"

pattern LoadBalancerTypeEnum_Network :: LoadBalancerTypeEnum
pattern LoadBalancerTypeEnum_Network = LoadBalancerTypeEnum' "network"

{-# COMPLETE
  LoadBalancerTypeEnum_Application,
  LoadBalancerTypeEnum_Gateway,
  LoadBalancerTypeEnum_Network,
  LoadBalancerTypeEnum'
  #-}
