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
-- Module      : Amazonka.ELBV2.Types.LoadBalancerTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.LoadBalancerTypeEnum
  ( LoadBalancerTypeEnum
      ( ..,
        LoadBalancerTypeEnum_Application,
        LoadBalancerTypeEnum_Gateway,
        LoadBalancerTypeEnum_Network
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoadBalancerTypeEnum = LoadBalancerTypeEnum'
  { fromLoadBalancerTypeEnum ::
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
