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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorTargetType
  ( TrafficMirrorTargetType
      ( ..,
        TrafficMirrorTargetType_Network_interface,
        TrafficMirrorTargetType_Network_load_balancer
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TrafficMirrorTargetType = TrafficMirrorTargetType'
  { fromTrafficMirrorTargetType ::
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

pattern TrafficMirrorTargetType_Network_interface :: TrafficMirrorTargetType
pattern TrafficMirrorTargetType_Network_interface = TrafficMirrorTargetType' "network-interface"

pattern TrafficMirrorTargetType_Network_load_balancer :: TrafficMirrorTargetType
pattern TrafficMirrorTargetType_Network_load_balancer = TrafficMirrorTargetType' "network-load-balancer"

{-# COMPLETE
  TrafficMirrorTargetType_Network_interface,
  TrafficMirrorTargetType_Network_load_balancer,
  TrafficMirrorTargetType'
  #-}
