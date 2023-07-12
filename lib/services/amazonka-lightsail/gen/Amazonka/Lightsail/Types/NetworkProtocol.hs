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
-- Module      : Amazonka.Lightsail.Types.NetworkProtocol
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.NetworkProtocol
  ( NetworkProtocol
      ( ..,
        NetworkProtocol_All,
        NetworkProtocol_Icmp,
        NetworkProtocol_Tcp,
        NetworkProtocol_Udp
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NetworkProtocol = NetworkProtocol'
  { fromNetworkProtocol ::
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

pattern NetworkProtocol_All :: NetworkProtocol
pattern NetworkProtocol_All = NetworkProtocol' "all"

pattern NetworkProtocol_Icmp :: NetworkProtocol
pattern NetworkProtocol_Icmp = NetworkProtocol' "icmp"

pattern NetworkProtocol_Tcp :: NetworkProtocol
pattern NetworkProtocol_Tcp = NetworkProtocol' "tcp"

pattern NetworkProtocol_Udp :: NetworkProtocol
pattern NetworkProtocol_Udp = NetworkProtocol' "udp"

{-# COMPLETE
  NetworkProtocol_All,
  NetworkProtocol_Icmp,
  NetworkProtocol_Tcp,
  NetworkProtocol_Udp,
  NetworkProtocol'
  #-}
