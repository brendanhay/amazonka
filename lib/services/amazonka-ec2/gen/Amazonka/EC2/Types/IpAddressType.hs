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
-- Module      : Amazonka.EC2.Types.IpAddressType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpAddressType
  ( IpAddressType
      ( ..,
        IpAddressType_Dualstack,
        IpAddressType_Ipv4,
        IpAddressType_Ipv6
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpAddressType = IpAddressType'
  { fromIpAddressType ::
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

pattern IpAddressType_Dualstack :: IpAddressType
pattern IpAddressType_Dualstack = IpAddressType' "dualstack"

pattern IpAddressType_Ipv4 :: IpAddressType
pattern IpAddressType_Ipv4 = IpAddressType' "ipv4"

pattern IpAddressType_Ipv6 :: IpAddressType
pattern IpAddressType_Ipv6 = IpAddressType' "ipv6"

{-# COMPLETE
  IpAddressType_Dualstack,
  IpAddressType_Ipv4,
  IpAddressType_Ipv6,
  IpAddressType'
  #-}
