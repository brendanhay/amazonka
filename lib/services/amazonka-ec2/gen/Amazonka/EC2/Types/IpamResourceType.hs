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
-- Module      : Amazonka.EC2.Types.IpamResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamResourceType
  ( IpamResourceType
      ( ..,
        IpamResourceType_Eip,
        IpamResourceType_Ipv6_pool,
        IpamResourceType_Public_ipv4_pool,
        IpamResourceType_Subnet,
        IpamResourceType_Vpc
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamResourceType = IpamResourceType'
  { fromIpamResourceType ::
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

pattern IpamResourceType_Eip :: IpamResourceType
pattern IpamResourceType_Eip = IpamResourceType' "eip"

pattern IpamResourceType_Ipv6_pool :: IpamResourceType
pattern IpamResourceType_Ipv6_pool = IpamResourceType' "ipv6-pool"

pattern IpamResourceType_Public_ipv4_pool :: IpamResourceType
pattern IpamResourceType_Public_ipv4_pool = IpamResourceType' "public-ipv4-pool"

pattern IpamResourceType_Subnet :: IpamResourceType
pattern IpamResourceType_Subnet = IpamResourceType' "subnet"

pattern IpamResourceType_Vpc :: IpamResourceType
pattern IpamResourceType_Vpc = IpamResourceType' "vpc"

{-# COMPLETE
  IpamResourceType_Eip,
  IpamResourceType_Ipv6_pool,
  IpamResourceType_Public_ipv4_pool,
  IpamResourceType_Subnet,
  IpamResourceType_Vpc,
  IpamResourceType'
  #-}
