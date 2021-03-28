{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
  ( NetworkInterfacePermissionStateCode
    ( NetworkInterfacePermissionStateCode'
    , NetworkInterfacePermissionStateCodePending
    , NetworkInterfacePermissionStateCodeGranted
    , NetworkInterfacePermissionStateCodeRevoking
    , NetworkInterfacePermissionStateCodeRevoked
    , fromNetworkInterfacePermissionStateCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype NetworkInterfacePermissionStateCode = NetworkInterfacePermissionStateCode'{fromNetworkInterfacePermissionStateCode
                                                                                   :: Core.Text}
                                                deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                Core.Show, Core.Generic)
                                                deriving newtype (Core.IsString, Core.Hashable,
                                                                  Core.NFData, Core.ToJSONKey,
                                                                  Core.FromJSONKey, Core.ToJSON,
                                                                  Core.FromJSON, Core.ToXML,
                                                                  Core.FromXML, Core.ToText,
                                                                  Core.FromText, Core.ToByteString,
                                                                  Core.ToQuery, Core.ToHeader)

pattern NetworkInterfacePermissionStateCodePending :: NetworkInterfacePermissionStateCode
pattern NetworkInterfacePermissionStateCodePending = NetworkInterfacePermissionStateCode' "pending"

pattern NetworkInterfacePermissionStateCodeGranted :: NetworkInterfacePermissionStateCode
pattern NetworkInterfacePermissionStateCodeGranted = NetworkInterfacePermissionStateCode' "granted"

pattern NetworkInterfacePermissionStateCodeRevoking :: NetworkInterfacePermissionStateCode
pattern NetworkInterfacePermissionStateCodeRevoking = NetworkInterfacePermissionStateCode' "revoking"

pattern NetworkInterfacePermissionStateCodeRevoked :: NetworkInterfacePermissionStateCode
pattern NetworkInterfacePermissionStateCodeRevoked = NetworkInterfacePermissionStateCode' "revoked"

{-# COMPLETE 
  NetworkInterfacePermissionStateCodePending,

  NetworkInterfacePermissionStateCodeGranted,

  NetworkInterfacePermissionStateCodeRevoking,

  NetworkInterfacePermissionStateCodeRevoked,
  NetworkInterfacePermissionStateCode'
  #-}
