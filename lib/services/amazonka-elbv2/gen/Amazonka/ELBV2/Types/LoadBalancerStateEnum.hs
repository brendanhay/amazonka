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
-- Module      : Amazonka.ELBV2.Types.LoadBalancerStateEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.LoadBalancerStateEnum
  ( LoadBalancerStateEnum
      ( ..,
        LoadBalancerStateEnum_Active,
        LoadBalancerStateEnum_Active_impaired,
        LoadBalancerStateEnum_Failed,
        LoadBalancerStateEnum_Provisioning
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoadBalancerStateEnum = LoadBalancerStateEnum'
  { fromLoadBalancerStateEnum ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
