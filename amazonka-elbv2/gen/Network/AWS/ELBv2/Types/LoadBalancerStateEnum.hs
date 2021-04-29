{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerStateEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerStateEnum
  ( LoadBalancerStateEnum
      ( ..,
        LoadBalancerStateEnum_Active,
        LoadBalancerStateEnum_Active_impaired,
        LoadBalancerStateEnum_Failed,
        LoadBalancerStateEnum_Provisioning
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LoadBalancerStateEnum = LoadBalancerStateEnum'
  { fromLoadBalancerStateEnum ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
