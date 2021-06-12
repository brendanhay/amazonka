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
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceType
  ( NetworkInterfaceType
      ( ..,
        NetworkInterfaceType_Efa,
        NetworkInterfaceType_Interface,
        NetworkInterfaceType_NatGateway
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype NetworkInterfaceType = NetworkInterfaceType'
  { fromNetworkInterfaceType ::
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

pattern NetworkInterfaceType_Efa :: NetworkInterfaceType
pattern NetworkInterfaceType_Efa = NetworkInterfaceType' "efa"

pattern NetworkInterfaceType_Interface :: NetworkInterfaceType
pattern NetworkInterfaceType_Interface = NetworkInterfaceType' "interface"

pattern NetworkInterfaceType_NatGateway :: NetworkInterfaceType
pattern NetworkInterfaceType_NatGateway = NetworkInterfaceType' "natGateway"

{-# COMPLETE
  NetworkInterfaceType_Efa,
  NetworkInterfaceType_Interface,
  NetworkInterfaceType_NatGateway,
  NetworkInterfaceType'
  #-}
