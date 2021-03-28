{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ConnectionErrorCodeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.ConnectionErrorCodeType
  ( ConnectionErrorCodeType
    ( ConnectionErrorCodeType'
    , ConnectionErrorCodeTypeInvalidCredentials
    , ConnectionErrorCodeTypeClusterNotFound
    , ConnectionErrorCodeTypeNetworkErrors
    , ConnectionErrorCodeTypeInternalError
    , ConnectionErrorCodeTypeInsufficientCloudhsmHsms
    , ConnectionErrorCodeTypeUserLockedOut
    , ConnectionErrorCodeTypeUserNotFound
    , ConnectionErrorCodeTypeUserLoggedIn
    , ConnectionErrorCodeTypeSubnetNotFound
    , fromConnectionErrorCodeType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ConnectionErrorCodeType = ConnectionErrorCodeType'{fromConnectionErrorCodeType
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern ConnectionErrorCodeTypeInvalidCredentials :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeInvalidCredentials = ConnectionErrorCodeType' "INVALID_CREDENTIALS"

pattern ConnectionErrorCodeTypeClusterNotFound :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeClusterNotFound = ConnectionErrorCodeType' "CLUSTER_NOT_FOUND"

pattern ConnectionErrorCodeTypeNetworkErrors :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeNetworkErrors = ConnectionErrorCodeType' "NETWORK_ERRORS"

pattern ConnectionErrorCodeTypeInternalError :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeInternalError = ConnectionErrorCodeType' "INTERNAL_ERROR"

pattern ConnectionErrorCodeTypeInsufficientCloudhsmHsms :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeInsufficientCloudhsmHsms = ConnectionErrorCodeType' "INSUFFICIENT_CLOUDHSM_HSMS"

pattern ConnectionErrorCodeTypeUserLockedOut :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeUserLockedOut = ConnectionErrorCodeType' "USER_LOCKED_OUT"

pattern ConnectionErrorCodeTypeUserNotFound :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeUserNotFound = ConnectionErrorCodeType' "USER_NOT_FOUND"

pattern ConnectionErrorCodeTypeUserLoggedIn :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeUserLoggedIn = ConnectionErrorCodeType' "USER_LOGGED_IN"

pattern ConnectionErrorCodeTypeSubnetNotFound :: ConnectionErrorCodeType
pattern ConnectionErrorCodeTypeSubnetNotFound = ConnectionErrorCodeType' "SUBNET_NOT_FOUND"

{-# COMPLETE 
  ConnectionErrorCodeTypeInvalidCredentials,

  ConnectionErrorCodeTypeClusterNotFound,

  ConnectionErrorCodeTypeNetworkErrors,

  ConnectionErrorCodeTypeInternalError,

  ConnectionErrorCodeTypeInsufficientCloudhsmHsms,

  ConnectionErrorCodeTypeUserLockedOut,

  ConnectionErrorCodeTypeUserNotFound,

  ConnectionErrorCodeTypeUserLoggedIn,

  ConnectionErrorCodeTypeSubnetNotFound,
  ConnectionErrorCodeType'
  #-}
