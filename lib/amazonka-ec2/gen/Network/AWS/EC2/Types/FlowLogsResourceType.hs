{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FlowLogsResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FlowLogsResourceType
  ( FlowLogsResourceType
    ( FlowLogsResourceType'
    , FlowLogsResourceTypeVpc
    , FlowLogsResourceTypeSubnet
    , FlowLogsResourceTypeNetworkInterface
    , fromFlowLogsResourceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype FlowLogsResourceType = FlowLogsResourceType'{fromFlowLogsResourceType
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern FlowLogsResourceTypeVpc :: FlowLogsResourceType
pattern FlowLogsResourceTypeVpc = FlowLogsResourceType' "VPC"

pattern FlowLogsResourceTypeSubnet :: FlowLogsResourceType
pattern FlowLogsResourceTypeSubnet = FlowLogsResourceType' "Subnet"

pattern FlowLogsResourceTypeNetworkInterface :: FlowLogsResourceType
pattern FlowLogsResourceTypeNetworkInterface = FlowLogsResourceType' "NetworkInterface"

{-# COMPLETE 
  FlowLogsResourceTypeVpc,

  FlowLogsResourceTypeSubnet,

  FlowLogsResourceTypeNetworkInterface,
  FlowLogsResourceType'
  #-}
