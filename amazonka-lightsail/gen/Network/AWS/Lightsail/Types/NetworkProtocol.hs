{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.NetworkProtocol
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.NetworkProtocol
  ( NetworkProtocol
      ( ..,
        NetworkProtocol_All,
        NetworkProtocol_Icmp,
        NetworkProtocol_Tcp,
        NetworkProtocol_Udp
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype NetworkProtocol = NetworkProtocol'
  { fromNetworkProtocol ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
