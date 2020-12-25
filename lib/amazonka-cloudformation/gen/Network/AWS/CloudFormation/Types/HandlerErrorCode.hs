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
        HandlerErrorCodeNotUpdatable,
        HandlerErrorCodeInvalidRequest,
        HandlerErrorCodeAccessDenied,
        HandlerErrorCodeInvalidCredentials,
        HandlerErrorCodeAlreadyExists,
        HandlerErrorCodeNotFound,
        HandlerErrorCodeResourceConflict,
        HandlerErrorCodeThrottling,
        HandlerErrorCodeServiceLimitExceeded,
        HandlerErrorCodeNotStabilized,
        HandlerErrorCodeGeneralServiceException,
        HandlerErrorCodeServiceInternalError,
        HandlerErrorCodeNetworkFailure,
        HandlerErrorCodeInternalFailure,
        fromHandlerErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype HandlerErrorCode = HandlerErrorCode'
  { fromHandlerErrorCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern HandlerErrorCodeNotUpdatable :: HandlerErrorCode
pattern HandlerErrorCodeNotUpdatable = HandlerErrorCode' "NotUpdatable"

pattern HandlerErrorCodeInvalidRequest :: HandlerErrorCode
pattern HandlerErrorCodeInvalidRequest = HandlerErrorCode' "InvalidRequest"

pattern HandlerErrorCodeAccessDenied :: HandlerErrorCode
pattern HandlerErrorCodeAccessDenied = HandlerErrorCode' "AccessDenied"

pattern HandlerErrorCodeInvalidCredentials :: HandlerErrorCode
pattern HandlerErrorCodeInvalidCredentials = HandlerErrorCode' "InvalidCredentials"

pattern HandlerErrorCodeAlreadyExists :: HandlerErrorCode
pattern HandlerErrorCodeAlreadyExists = HandlerErrorCode' "AlreadyExists"

pattern HandlerErrorCodeNotFound :: HandlerErrorCode
pattern HandlerErrorCodeNotFound = HandlerErrorCode' "NotFound"

pattern HandlerErrorCodeResourceConflict :: HandlerErrorCode
pattern HandlerErrorCodeResourceConflict = HandlerErrorCode' "ResourceConflict"

pattern HandlerErrorCodeThrottling :: HandlerErrorCode
pattern HandlerErrorCodeThrottling = HandlerErrorCode' "Throttling"

pattern HandlerErrorCodeServiceLimitExceeded :: HandlerErrorCode
pattern HandlerErrorCodeServiceLimitExceeded = HandlerErrorCode' "ServiceLimitExceeded"

pattern HandlerErrorCodeNotStabilized :: HandlerErrorCode
pattern HandlerErrorCodeNotStabilized = HandlerErrorCode' "NotStabilized"

pattern HandlerErrorCodeGeneralServiceException :: HandlerErrorCode
pattern HandlerErrorCodeGeneralServiceException = HandlerErrorCode' "GeneralServiceException"

pattern HandlerErrorCodeServiceInternalError :: HandlerErrorCode
pattern HandlerErrorCodeServiceInternalError = HandlerErrorCode' "ServiceInternalError"

pattern HandlerErrorCodeNetworkFailure :: HandlerErrorCode
pattern HandlerErrorCodeNetworkFailure = HandlerErrorCode' "NetworkFailure"

pattern HandlerErrorCodeInternalFailure :: HandlerErrorCode
pattern HandlerErrorCodeInternalFailure = HandlerErrorCode' "InternalFailure"

{-# COMPLETE
  HandlerErrorCodeNotUpdatable,
  HandlerErrorCodeInvalidRequest,
  HandlerErrorCodeAccessDenied,
  HandlerErrorCodeInvalidCredentials,
  HandlerErrorCodeAlreadyExists,
  HandlerErrorCodeNotFound,
  HandlerErrorCodeResourceConflict,
  HandlerErrorCodeThrottling,
  HandlerErrorCodeServiceLimitExceeded,
  HandlerErrorCodeNotStabilized,
  HandlerErrorCodeGeneralServiceException,
  HandlerErrorCodeServiceInternalError,
  HandlerErrorCodeNetworkFailure,
  HandlerErrorCodeInternalFailure,
  HandlerErrorCode'
  #-}
