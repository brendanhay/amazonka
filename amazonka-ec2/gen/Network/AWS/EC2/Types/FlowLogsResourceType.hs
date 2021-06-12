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
-- Module      : Network.AWS.EC2.Types.FlowLogsResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FlowLogsResourceType
  ( FlowLogsResourceType
      ( ..,
        FlowLogsResourceType_NetworkInterface,
        FlowLogsResourceType_Subnet,
        FlowLogsResourceType_VPC
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype FlowLogsResourceType = FlowLogsResourceType'
  { fromFlowLogsResourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern FlowLogsResourceType_VPC :: FlowLogsResourceType
pattern FlowLogsResourceType_VPC = FlowLogsResourceType' "VPC"

{-# COMPLETE
  FlowLogsResourceType_NetworkInterface,
  FlowLogsResourceType_Subnet,
  FlowLogsResourceType_VPC,
  FlowLogsResourceType'
  #-}
