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
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerState
  ( LoadBalancerState
      ( ..,
        LoadBalancerState_Active,
        LoadBalancerState_Active_impaired,
        LoadBalancerState_Failed,
        LoadBalancerState_Provisioning,
        LoadBalancerState_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LoadBalancerState = LoadBalancerState'
  { fromLoadBalancerState ::
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

pattern LoadBalancerState_Active :: LoadBalancerState
pattern LoadBalancerState_Active = LoadBalancerState' "active"

pattern LoadBalancerState_Active_impaired :: LoadBalancerState
pattern LoadBalancerState_Active_impaired = LoadBalancerState' "active_impaired"

pattern LoadBalancerState_Failed :: LoadBalancerState
pattern LoadBalancerState_Failed = LoadBalancerState' "failed"

pattern LoadBalancerState_Provisioning :: LoadBalancerState
pattern LoadBalancerState_Provisioning = LoadBalancerState' "provisioning"

pattern LoadBalancerState_Unknown :: LoadBalancerState
pattern LoadBalancerState_Unknown = LoadBalancerState' "unknown"

{-# COMPLETE
  LoadBalancerState_Active,
  LoadBalancerState_Active_impaired,
  LoadBalancerState_Failed,
  LoadBalancerState_Provisioning,
  LoadBalancerState_Unknown,
  LoadBalancerState'
  #-}
