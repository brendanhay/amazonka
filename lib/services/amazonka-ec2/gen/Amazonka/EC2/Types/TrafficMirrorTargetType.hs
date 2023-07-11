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
-- Module      : Amazonka.EC2.Types.TrafficMirrorTargetType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorTargetType
  ( TrafficMirrorTargetType
      ( ..,
        TrafficMirrorTargetType_Gateway_load_balancer_endpoint,
        TrafficMirrorTargetType_Network_interface,
        TrafficMirrorTargetType_Network_load_balancer
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TrafficMirrorTargetType = TrafficMirrorTargetType'
  { fromTrafficMirrorTargetType ::
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

pattern TrafficMirrorTargetType_Gateway_load_balancer_endpoint :: TrafficMirrorTargetType
pattern TrafficMirrorTargetType_Gateway_load_balancer_endpoint = TrafficMirrorTargetType' "gateway-load-balancer-endpoint"

pattern TrafficMirrorTargetType_Network_interface :: TrafficMirrorTargetType
pattern TrafficMirrorTargetType_Network_interface = TrafficMirrorTargetType' "network-interface"

pattern TrafficMirrorTargetType_Network_load_balancer :: TrafficMirrorTargetType
pattern TrafficMirrorTargetType_Network_load_balancer = TrafficMirrorTargetType' "network-load-balancer"

{-# COMPLETE
  TrafficMirrorTargetType_Gateway_load_balancer_endpoint,
  TrafficMirrorTargetType_Network_interface,
  TrafficMirrorTargetType_Network_load_balancer,
  TrafficMirrorTargetType'
  #-}
