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
-- Module      : Amazonka.EC2.Types.FlowLogsResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FlowLogsResourceType
  ( FlowLogsResourceType
      ( ..,
        FlowLogsResourceType_NetworkInterface,
        FlowLogsResourceType_Subnet,
        FlowLogsResourceType_TransitGateway,
        FlowLogsResourceType_TransitGatewayAttachment,
        FlowLogsResourceType_VPC
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype FlowLogsResourceType = FlowLogsResourceType'
  { fromFlowLogsResourceType ::
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

pattern FlowLogsResourceType_NetworkInterface :: FlowLogsResourceType
pattern FlowLogsResourceType_NetworkInterface = FlowLogsResourceType' "NetworkInterface"

pattern FlowLogsResourceType_Subnet :: FlowLogsResourceType
pattern FlowLogsResourceType_Subnet = FlowLogsResourceType' "Subnet"

pattern FlowLogsResourceType_TransitGateway :: FlowLogsResourceType
pattern FlowLogsResourceType_TransitGateway = FlowLogsResourceType' "TransitGateway"

pattern FlowLogsResourceType_TransitGatewayAttachment :: FlowLogsResourceType
pattern FlowLogsResourceType_TransitGatewayAttachment = FlowLogsResourceType' "TransitGatewayAttachment"

pattern FlowLogsResourceType_VPC :: FlowLogsResourceType
pattern FlowLogsResourceType_VPC = FlowLogsResourceType' "VPC"

{-# COMPLETE
  FlowLogsResourceType_NetworkInterface,
  FlowLogsResourceType_Subnet,
  FlowLogsResourceType_TransitGateway,
  FlowLogsResourceType_TransitGatewayAttachment,
  FlowLogsResourceType_VPC,
  FlowLogsResourceType'
  #-}
