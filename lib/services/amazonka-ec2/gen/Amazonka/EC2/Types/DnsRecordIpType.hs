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
-- Module      : Amazonka.EC2.Types.DnsRecordIpType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DnsRecordIpType
  ( DnsRecordIpType
      ( ..,
        DnsRecordIpType_Dualstack,
        DnsRecordIpType_Ipv4,
        DnsRecordIpType_Ipv6,
        DnsRecordIpType_Service_defined
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype DnsRecordIpType = DnsRecordIpType'
  { fromDnsRecordIpType ::
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

pattern DnsRecordIpType_Dualstack :: DnsRecordIpType
pattern DnsRecordIpType_Dualstack = DnsRecordIpType' "dualstack"

pattern DnsRecordIpType_Ipv4 :: DnsRecordIpType
pattern DnsRecordIpType_Ipv4 = DnsRecordIpType' "ipv4"

pattern DnsRecordIpType_Ipv6 :: DnsRecordIpType
pattern DnsRecordIpType_Ipv6 = DnsRecordIpType' "ipv6"

pattern DnsRecordIpType_Service_defined :: DnsRecordIpType
pattern DnsRecordIpType_Service_defined = DnsRecordIpType' "service-defined"

{-# COMPLETE
  DnsRecordIpType_Dualstack,
  DnsRecordIpType_Ipv4,
  DnsRecordIpType_Ipv6,
  DnsRecordIpType_Service_defined,
  DnsRecordIpType'
  #-}
