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
-- Module      : Amazonka.FMS.Types.TargetType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.TargetType
  ( TargetType
      ( ..,
        TargetType_CARRIER_GATEWAY,
        TargetType_EGRESS_ONLY_INTERNET_GATEWAY,
        TargetType_GATEWAY,
        TargetType_INSTANCE,
        TargetType_LOCAL_GATEWAY,
        TargetType_NAT_GATEWAY,
        TargetType_NETWORK_INTERFACE,
        TargetType_TRANSIT_GATEWAY,
        TargetType_VPC_ENDPOINT,
        TargetType_VPC_PEERING_CONNECTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetType = TargetType'
  { fromTargetType ::
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

pattern TargetType_CARRIER_GATEWAY :: TargetType
pattern TargetType_CARRIER_GATEWAY = TargetType' "CARRIER_GATEWAY"

pattern TargetType_EGRESS_ONLY_INTERNET_GATEWAY :: TargetType
pattern TargetType_EGRESS_ONLY_INTERNET_GATEWAY = TargetType' "EGRESS_ONLY_INTERNET_GATEWAY"

pattern TargetType_GATEWAY :: TargetType
pattern TargetType_GATEWAY = TargetType' "GATEWAY"

pattern TargetType_INSTANCE :: TargetType
pattern TargetType_INSTANCE = TargetType' "INSTANCE"

pattern TargetType_LOCAL_GATEWAY :: TargetType
pattern TargetType_LOCAL_GATEWAY = TargetType' "LOCAL_GATEWAY"

pattern TargetType_NAT_GATEWAY :: TargetType
pattern TargetType_NAT_GATEWAY = TargetType' "NAT_GATEWAY"

pattern TargetType_NETWORK_INTERFACE :: TargetType
pattern TargetType_NETWORK_INTERFACE = TargetType' "NETWORK_INTERFACE"

pattern TargetType_TRANSIT_GATEWAY :: TargetType
pattern TargetType_TRANSIT_GATEWAY = TargetType' "TRANSIT_GATEWAY"

pattern TargetType_VPC_ENDPOINT :: TargetType
pattern TargetType_VPC_ENDPOINT = TargetType' "VPC_ENDPOINT"

pattern TargetType_VPC_PEERING_CONNECTION :: TargetType
pattern TargetType_VPC_PEERING_CONNECTION = TargetType' "VPC_PEERING_CONNECTION"

{-# COMPLETE
  TargetType_CARRIER_GATEWAY,
  TargetType_EGRESS_ONLY_INTERNET_GATEWAY,
  TargetType_GATEWAY,
  TargetType_INSTANCE,
  TargetType_LOCAL_GATEWAY,
  TargetType_NAT_GATEWAY,
  TargetType_NETWORK_INTERFACE,
  TargetType_TRANSIT_GATEWAY,
  TargetType_VPC_ENDPOINT,
  TargetType_VPC_PEERING_CONNECTION,
  TargetType'
  #-}
