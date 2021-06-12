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
-- Module      : Network.AWS.CloudFormation.Types.HandlerErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.HandlerErrorCode
  ( HandlerErrorCode
      ( ..,
        HandlerErrorCode_AccessDenied,
        HandlerErrorCode_AlreadyExists,
        HandlerErrorCode_GeneralServiceException,
        HandlerErrorCode_InternalFailure,
        HandlerErrorCode_InvalidCredentials,
        HandlerErrorCode_InvalidRequest,
        HandlerErrorCode_NetworkFailure,
        HandlerErrorCode_NotFound,
        HandlerErrorCode_NotStabilized,
        HandlerErrorCode_NotUpdatable,
        HandlerErrorCode_ResourceConflict,
        HandlerErrorCode_ServiceInternalError,
        HandlerErrorCode_ServiceLimitExceeded,
        HandlerErrorCode_Throttling
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HandlerErrorCode = HandlerErrorCode'
  { fromHandlerErrorCode ::
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

pattern HandlerErrorCode_AccessDenied :: HandlerErrorCode
pattern HandlerErrorCode_AccessDenied = HandlerErrorCode' "AccessDenied"

pattern HandlerErrorCode_AlreadyExists :: HandlerErrorCode
pattern HandlerErrorCode_AlreadyExists = HandlerErrorCode' "AlreadyExists"

pattern HandlerErrorCode_GeneralServiceException :: HandlerErrorCode
pattern HandlerErrorCode_GeneralServiceException = HandlerErrorCode' "GeneralServiceException"

pattern HandlerErrorCode_InternalFailure :: HandlerErrorCode
pattern HandlerErrorCode_InternalFailure = HandlerErrorCode' "InternalFailure"

pattern HandlerErrorCode_InvalidCredentials :: HandlerErrorCode
pattern HandlerErrorCode_InvalidCredentials = HandlerErrorCode' "InvalidCredentials"

pattern HandlerErrorCode_InvalidRequest :: HandlerErrorCode
pattern HandlerErrorCode_InvalidRequest = HandlerErrorCode' "InvalidRequest"

pattern HandlerErrorCode_NetworkFailure :: HandlerErrorCode
pattern HandlerErrorCode_NetworkFailure = HandlerErrorCode' "NetworkFailure"

pattern HandlerErrorCode_NotFound :: HandlerErrorCode
pattern HandlerErrorCode_NotFound = HandlerErrorCode' "NotFound"

pattern HandlerErrorCode_NotStabilized :: HandlerErrorCode
pattern HandlerErrorCode_NotStabilized = HandlerErrorCode' "NotStabilized"

pattern HandlerErrorCode_NotUpdatable :: HandlerErrorCode
pattern HandlerErrorCode_NotUpdatable = HandlerErrorCode' "NotUpdatable"

pattern HandlerErrorCode_ResourceConflict :: HandlerErrorCode
pattern HandlerErrorCode_ResourceConflict = HandlerErrorCode' "ResourceConflict"

pattern HandlerErrorCode_ServiceInternalError :: HandlerErrorCode
pattern HandlerErrorCode_ServiceInternalError = HandlerErrorCode' "ServiceInternalError"

pattern HandlerErrorCode_ServiceLimitExceeded :: HandlerErrorCode
pattern HandlerErrorCode_ServiceLimitExceeded = HandlerErrorCode' "ServiceLimitExceeded"

pattern HandlerErrorCode_Throttling :: HandlerErrorCode
pattern HandlerErrorCode_Throttling = HandlerErrorCode' "Throttling"

{-# COMPLETE
  HandlerErrorCode_AccessDenied,
  HandlerErrorCode_AlreadyExists,
  HandlerErrorCode_GeneralServiceException,
  HandlerErrorCode_InternalFailure,
  HandlerErrorCode_InvalidCredentials,
  HandlerErrorCode_InvalidRequest,
  HandlerErrorCode_NetworkFailure,
  HandlerErrorCode_NotFound,
  HandlerErrorCode_NotStabilized,
  HandlerErrorCode_NotUpdatable,
  HandlerErrorCode_ResourceConflict,
  HandlerErrorCode_ServiceInternalError,
  HandlerErrorCode_ServiceLimitExceeded,
  HandlerErrorCode_Throttling,
  HandlerErrorCode'
  #-}
