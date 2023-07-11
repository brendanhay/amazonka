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
-- Module      : Amazonka.EC2.Types.IpamPoolAllocationResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamPoolAllocationResourceType
  ( IpamPoolAllocationResourceType
      ( ..,
        IpamPoolAllocationResourceType_Custom,
        IpamPoolAllocationResourceType_Ec2_public_ipv4_pool,
        IpamPoolAllocationResourceType_Ipam_pool,
        IpamPoolAllocationResourceType_Vpc
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamPoolAllocationResourceType = IpamPoolAllocationResourceType'
  { fromIpamPoolAllocationResourceType ::
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

pattern IpamPoolAllocationResourceType_Custom :: IpamPoolAllocationResourceType
pattern IpamPoolAllocationResourceType_Custom = IpamPoolAllocationResourceType' "custom"

pattern IpamPoolAllocationResourceType_Ec2_public_ipv4_pool :: IpamPoolAllocationResourceType
pattern IpamPoolAllocationResourceType_Ec2_public_ipv4_pool = IpamPoolAllocationResourceType' "ec2-public-ipv4-pool"

pattern IpamPoolAllocationResourceType_Ipam_pool :: IpamPoolAllocationResourceType
pattern IpamPoolAllocationResourceType_Ipam_pool = IpamPoolAllocationResourceType' "ipam-pool"

pattern IpamPoolAllocationResourceType_Vpc :: IpamPoolAllocationResourceType
pattern IpamPoolAllocationResourceType_Vpc = IpamPoolAllocationResourceType' "vpc"

{-# COMPLETE
  IpamPoolAllocationResourceType_Custom,
  IpamPoolAllocationResourceType_Ec2_public_ipv4_pool,
  IpamPoolAllocationResourceType_Ipam_pool,
  IpamPoolAllocationResourceType_Vpc,
  IpamPoolAllocationResourceType'
  #-}
