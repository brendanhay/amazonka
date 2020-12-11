-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ConnectionErrorCodeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ConnectionErrorCodeType
  ( ConnectionErrorCodeType
      ( ConnectionErrorCodeType',
        ClusterNotFound,
        InsufficientCloudhsmHSMs,
        InternalError,
        InvalidCredentials,
        NetworkErrors,
        SubnetNotFound,
        UserLockedOut,
        UserLoggedIn,
        UserNotFound
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConnectionErrorCodeType = ConnectionErrorCodeType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ClusterNotFound :: ConnectionErrorCodeType
pattern ClusterNotFound = ConnectionErrorCodeType' "CLUSTER_NOT_FOUND"

pattern InsufficientCloudhsmHSMs :: ConnectionErrorCodeType
pattern InsufficientCloudhsmHSMs = ConnectionErrorCodeType' "INSUFFICIENT_CLOUDHSM_HSMS"

pattern InternalError :: ConnectionErrorCodeType
pattern InternalError = ConnectionErrorCodeType' "INTERNAL_ERROR"

pattern InvalidCredentials :: ConnectionErrorCodeType
pattern InvalidCredentials = ConnectionErrorCodeType' "INVALID_CREDENTIALS"

pattern NetworkErrors :: ConnectionErrorCodeType
pattern NetworkErrors = ConnectionErrorCodeType' "NETWORK_ERRORS"

pattern SubnetNotFound :: ConnectionErrorCodeType
pattern SubnetNotFound = ConnectionErrorCodeType' "SUBNET_NOT_FOUND"

pattern UserLockedOut :: ConnectionErrorCodeType
pattern UserLockedOut = ConnectionErrorCodeType' "USER_LOCKED_OUT"

pattern UserLoggedIn :: ConnectionErrorCodeType
pattern UserLoggedIn = ConnectionErrorCodeType' "USER_LOGGED_IN"

pattern UserNotFound :: ConnectionErrorCodeType
pattern UserNotFound = ConnectionErrorCodeType' "USER_NOT_FOUND"

{-# COMPLETE
  ClusterNotFound,
  InsufficientCloudhsmHSMs,
  InternalError,
  InvalidCredentials,
  NetworkErrors,
  SubnetNotFound,
  UserLockedOut,
  UserLoggedIn,
  UserNotFound,
  ConnectionErrorCodeType'
  #-}
