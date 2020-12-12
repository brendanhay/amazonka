{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.HandlerErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.HandlerErrorCode
  ( HandlerErrorCode
      ( HandlerErrorCode',
        AccessDenied,
        AlreadyExists,
        GeneralServiceException,
        InternalFailure,
        InvalidCredentials,
        InvalidRequest,
        NetworkFailure,
        NotFound,
        NotStabilized,
        NotUpdatable,
        ResourceConflict,
        ServiceInternalError,
        ServiceLimitExceeded,
        Throttling
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HandlerErrorCode = HandlerErrorCode' Lude.Text
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

pattern AccessDenied :: HandlerErrorCode
pattern AccessDenied = HandlerErrorCode' "AccessDenied"

pattern AlreadyExists :: HandlerErrorCode
pattern AlreadyExists = HandlerErrorCode' "AlreadyExists"

pattern GeneralServiceException :: HandlerErrorCode
pattern GeneralServiceException = HandlerErrorCode' "GeneralServiceException"

pattern InternalFailure :: HandlerErrorCode
pattern InternalFailure = HandlerErrorCode' "InternalFailure"

pattern InvalidCredentials :: HandlerErrorCode
pattern InvalidCredentials = HandlerErrorCode' "InvalidCredentials"

pattern InvalidRequest :: HandlerErrorCode
pattern InvalidRequest = HandlerErrorCode' "InvalidRequest"

pattern NetworkFailure :: HandlerErrorCode
pattern NetworkFailure = HandlerErrorCode' "NetworkFailure"

pattern NotFound :: HandlerErrorCode
pattern NotFound = HandlerErrorCode' "NotFound"

pattern NotStabilized :: HandlerErrorCode
pattern NotStabilized = HandlerErrorCode' "NotStabilized"

pattern NotUpdatable :: HandlerErrorCode
pattern NotUpdatable = HandlerErrorCode' "NotUpdatable"

pattern ResourceConflict :: HandlerErrorCode
pattern ResourceConflict = HandlerErrorCode' "ResourceConflict"

pattern ServiceInternalError :: HandlerErrorCode
pattern ServiceInternalError = HandlerErrorCode' "ServiceInternalError"

pattern ServiceLimitExceeded :: HandlerErrorCode
pattern ServiceLimitExceeded = HandlerErrorCode' "ServiceLimitExceeded"

pattern Throttling :: HandlerErrorCode
pattern Throttling = HandlerErrorCode' "Throttling"

{-# COMPLETE
  AccessDenied,
  AlreadyExists,
  GeneralServiceException,
  InternalFailure,
  InvalidCredentials,
  InvalidRequest,
  NetworkFailure,
  NotFound,
  NotStabilized,
  NotUpdatable,
  ResourceConflict,
  ServiceInternalError,
  ServiceLimitExceeded,
  Throttling,
  HandlerErrorCode'
  #-}
