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
-- Module      : Amazonka.EC2.Types.IpamAddressHistoryResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamAddressHistoryResourceType
  ( IpamAddressHistoryResourceType
      ( ..,
        IpamAddressHistoryResourceType_Eip,
        IpamAddressHistoryResourceType_Instance,
        IpamAddressHistoryResourceType_Network_interface,
        IpamAddressHistoryResourceType_Subnet,
        IpamAddressHistoryResourceType_Vpc
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamAddressHistoryResourceType = IpamAddressHistoryResourceType'
  { fromIpamAddressHistoryResourceType ::
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

pattern IpamAddressHistoryResourceType_Eip :: IpamAddressHistoryResourceType
pattern IpamAddressHistoryResourceType_Eip = IpamAddressHistoryResourceType' "eip"

pattern IpamAddressHistoryResourceType_Instance :: IpamAddressHistoryResourceType
pattern IpamAddressHistoryResourceType_Instance = IpamAddressHistoryResourceType' "instance"

pattern IpamAddressHistoryResourceType_Network_interface :: IpamAddressHistoryResourceType
pattern IpamAddressHistoryResourceType_Network_interface = IpamAddressHistoryResourceType' "network-interface"

pattern IpamAddressHistoryResourceType_Subnet :: IpamAddressHistoryResourceType
pattern IpamAddressHistoryResourceType_Subnet = IpamAddressHistoryResourceType' "subnet"

pattern IpamAddressHistoryResourceType_Vpc :: IpamAddressHistoryResourceType
pattern IpamAddressHistoryResourceType_Vpc = IpamAddressHistoryResourceType' "vpc"

{-# COMPLETE
  IpamAddressHistoryResourceType_Eip,
  IpamAddressHistoryResourceType_Instance,
  IpamAddressHistoryResourceType_Network_interface,
  IpamAddressHistoryResourceType_Subnet,
  IpamAddressHistoryResourceType_Vpc,
  IpamAddressHistoryResourceType'
  #-}
