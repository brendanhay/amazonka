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
-- Module      : Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
  ( TransitGatewayAttachmentResourceType
      ( ..,
        TransitGatewayAttachmentResourceType_Connect,
        TransitGatewayAttachmentResourceType_Direct_connect_gateway,
        TransitGatewayAttachmentResourceType_Peering,
        TransitGatewayAttachmentResourceType_Tgw_peering,
        TransitGatewayAttachmentResourceType_Vpc,
        TransitGatewayAttachmentResourceType_Vpn
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayAttachmentResourceType = TransitGatewayAttachmentResourceType'
  { fromTransitGatewayAttachmentResourceType ::
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

pattern TransitGatewayAttachmentResourceType_Connect :: TransitGatewayAttachmentResourceType
pattern TransitGatewayAttachmentResourceType_Connect = TransitGatewayAttachmentResourceType' "connect"

pattern TransitGatewayAttachmentResourceType_Direct_connect_gateway :: TransitGatewayAttachmentResourceType
pattern TransitGatewayAttachmentResourceType_Direct_connect_gateway = TransitGatewayAttachmentResourceType' "direct-connect-gateway"

pattern TransitGatewayAttachmentResourceType_Peering :: TransitGatewayAttachmentResourceType
pattern TransitGatewayAttachmentResourceType_Peering = TransitGatewayAttachmentResourceType' "peering"

pattern TransitGatewayAttachmentResourceType_Tgw_peering :: TransitGatewayAttachmentResourceType
pattern TransitGatewayAttachmentResourceType_Tgw_peering = TransitGatewayAttachmentResourceType' "tgw-peering"

pattern TransitGatewayAttachmentResourceType_Vpc :: TransitGatewayAttachmentResourceType
pattern TransitGatewayAttachmentResourceType_Vpc = TransitGatewayAttachmentResourceType' "vpc"

pattern TransitGatewayAttachmentResourceType_Vpn :: TransitGatewayAttachmentResourceType
pattern TransitGatewayAttachmentResourceType_Vpn = TransitGatewayAttachmentResourceType' "vpn"

{-# COMPLETE
  TransitGatewayAttachmentResourceType_Connect,
  TransitGatewayAttachmentResourceType_Direct_connect_gateway,
  TransitGatewayAttachmentResourceType_Peering,
  TransitGatewayAttachmentResourceType_Tgw_peering,
  TransitGatewayAttachmentResourceType_Vpc,
  TransitGatewayAttachmentResourceType_Vpn,
  TransitGatewayAttachmentResourceType'
  #-}
