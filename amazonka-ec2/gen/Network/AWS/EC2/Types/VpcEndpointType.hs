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
-- Module      : Network.AWS.EC2.Types.VpcEndpointType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcEndpointType
  ( VpcEndpointType
      ( ..,
        VpcEndpointType_Gateway,
        VpcEndpointType_GatewayLoadBalancer,
        VpcEndpointType_Interface
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype VpcEndpointType = VpcEndpointType'
  { fromVpcEndpointType ::
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

pattern VpcEndpointType_Gateway :: VpcEndpointType
pattern VpcEndpointType_Gateway = VpcEndpointType' "Gateway"

pattern VpcEndpointType_GatewayLoadBalancer :: VpcEndpointType
pattern VpcEndpointType_GatewayLoadBalancer = VpcEndpointType' "GatewayLoadBalancer"

pattern VpcEndpointType_Interface :: VpcEndpointType
pattern VpcEndpointType_Interface = VpcEndpointType' "Interface"

{-# COMPLETE
  VpcEndpointType_Gateway,
  VpcEndpointType_GatewayLoadBalancer,
  VpcEndpointType_Interface,
  VpcEndpointType'
  #-}
