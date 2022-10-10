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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype DnsRecordIpType = DnsRecordIpType'
  { fromDnsRecordIpType ::
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
