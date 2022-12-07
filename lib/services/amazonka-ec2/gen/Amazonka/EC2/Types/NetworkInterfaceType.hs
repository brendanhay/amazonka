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
-- Module      : Amazonka.EC2.Types.NetworkInterfaceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfaceType
  ( NetworkInterfaceType
      ( ..,
        NetworkInterfaceType_Api_gateway_managed,
        NetworkInterfaceType_Aws_codestar_connections_managed,
        NetworkInterfaceType_Branch,
        NetworkInterfaceType_Efa,
        NetworkInterfaceType_Gateway_load_balancer,
        NetworkInterfaceType_Gateway_load_balancer_endpoint,
        NetworkInterfaceType_Global_accelerator_managed,
        NetworkInterfaceType_Interface,
        NetworkInterfaceType_Iot_rules_managed,
        NetworkInterfaceType_Lambda,
        NetworkInterfaceType_Load_balancer,
        NetworkInterfaceType_NatGateway,
        NetworkInterfaceType_Network_load_balancer,
        NetworkInterfaceType_Quicksight,
        NetworkInterfaceType_Transit_gateway,
        NetworkInterfaceType_Trunk,
        NetworkInterfaceType_Vpc_endpoint
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype NetworkInterfaceType = NetworkInterfaceType'
  { fromNetworkInterfaceType ::
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

pattern NetworkInterfaceType_Api_gateway_managed :: NetworkInterfaceType
pattern NetworkInterfaceType_Api_gateway_managed = NetworkInterfaceType' "api_gateway_managed"

pattern NetworkInterfaceType_Aws_codestar_connections_managed :: NetworkInterfaceType
pattern NetworkInterfaceType_Aws_codestar_connections_managed = NetworkInterfaceType' "aws_codestar_connections_managed"

pattern NetworkInterfaceType_Branch :: NetworkInterfaceType
pattern NetworkInterfaceType_Branch = NetworkInterfaceType' "branch"

pattern NetworkInterfaceType_Efa :: NetworkInterfaceType
pattern NetworkInterfaceType_Efa = NetworkInterfaceType' "efa"

pattern NetworkInterfaceType_Gateway_load_balancer :: NetworkInterfaceType
pattern NetworkInterfaceType_Gateway_load_balancer = NetworkInterfaceType' "gateway_load_balancer"

pattern NetworkInterfaceType_Gateway_load_balancer_endpoint :: NetworkInterfaceType
pattern NetworkInterfaceType_Gateway_load_balancer_endpoint = NetworkInterfaceType' "gateway_load_balancer_endpoint"

pattern NetworkInterfaceType_Global_accelerator_managed :: NetworkInterfaceType
pattern NetworkInterfaceType_Global_accelerator_managed = NetworkInterfaceType' "global_accelerator_managed"

pattern NetworkInterfaceType_Interface :: NetworkInterfaceType
pattern NetworkInterfaceType_Interface = NetworkInterfaceType' "interface"

pattern NetworkInterfaceType_Iot_rules_managed :: NetworkInterfaceType
pattern NetworkInterfaceType_Iot_rules_managed = NetworkInterfaceType' "iot_rules_managed"

pattern NetworkInterfaceType_Lambda :: NetworkInterfaceType
pattern NetworkInterfaceType_Lambda = NetworkInterfaceType' "lambda"

pattern NetworkInterfaceType_Load_balancer :: NetworkInterfaceType
pattern NetworkInterfaceType_Load_balancer = NetworkInterfaceType' "load_balancer"

pattern NetworkInterfaceType_NatGateway :: NetworkInterfaceType
pattern NetworkInterfaceType_NatGateway = NetworkInterfaceType' "natGateway"

pattern NetworkInterfaceType_Network_load_balancer :: NetworkInterfaceType
pattern NetworkInterfaceType_Network_load_balancer = NetworkInterfaceType' "network_load_balancer"

pattern NetworkInterfaceType_Quicksight :: NetworkInterfaceType
pattern NetworkInterfaceType_Quicksight = NetworkInterfaceType' "quicksight"

pattern NetworkInterfaceType_Transit_gateway :: NetworkInterfaceType
pattern NetworkInterfaceType_Transit_gateway = NetworkInterfaceType' "transit_gateway"

pattern NetworkInterfaceType_Trunk :: NetworkInterfaceType
pattern NetworkInterfaceType_Trunk = NetworkInterfaceType' "trunk"

pattern NetworkInterfaceType_Vpc_endpoint :: NetworkInterfaceType
pattern NetworkInterfaceType_Vpc_endpoint = NetworkInterfaceType' "vpc_endpoint"

{-# COMPLETE
  NetworkInterfaceType_Api_gateway_managed,
  NetworkInterfaceType_Aws_codestar_connections_managed,
  NetworkInterfaceType_Branch,
  NetworkInterfaceType_Efa,
  NetworkInterfaceType_Gateway_load_balancer,
  NetworkInterfaceType_Gateway_load_balancer_endpoint,
  NetworkInterfaceType_Global_accelerator_managed,
  NetworkInterfaceType_Interface,
  NetworkInterfaceType_Iot_rules_managed,
  NetworkInterfaceType_Lambda,
  NetworkInterfaceType_Load_balancer,
  NetworkInterfaceType_NatGateway,
  NetworkInterfaceType_Network_load_balancer,
  NetworkInterfaceType_Quicksight,
  NetworkInterfaceType_Transit_gateway,
  NetworkInterfaceType_Trunk,
  NetworkInterfaceType_Vpc_endpoint,
  NetworkInterfaceType'
  #-}
