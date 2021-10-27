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
-- Module      : Network.AWS.ELBV2.Types.LoadBalancerStateEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBV2.Types.LoadBalancerStateEnum
  ( LoadBalancerStateEnum
      ( ..,
        LoadBalancerStateEnum_Active,
        LoadBalancerStateEnum_Active_impaired,
        LoadBalancerStateEnum_Failed,
        LoadBalancerStateEnum_Provisioning
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LoadBalancerStateEnum = LoadBalancerStateEnum'
  { fromLoadBalancerStateEnum ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern LoadBalancerStateEnum_Active :: LoadBalancerStateEnum
pattern LoadBalancerStateEnum_Active = LoadBalancerStateEnum' "active"

pattern LoadBalancerStateEnum_Active_impaired :: LoadBalancerStateEnum
pattern LoadBalancerStateEnum_Active_impaired = LoadBalancerStateEnum' "active_impaired"

pattern LoadBalancerStateEnum_Failed :: LoadBalancerStateEnum
pattern LoadBalancerStateEnum_Failed = LoadBalancerStateEnum' "failed"

pattern LoadBalancerStateEnum_Provisioning :: LoadBalancerStateEnum
pattern LoadBalancerStateEnum_Provisioning = LoadBalancerStateEnum' "provisioning"

{-# COMPLETE
  LoadBalancerStateEnum_Active,
  LoadBalancerStateEnum_Active_impaired,
  LoadBalancerStateEnum_Failed,
  LoadBalancerStateEnum_Provisioning,
  LoadBalancerStateEnum'
  #-}
