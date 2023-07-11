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
-- Module      : Amazonka.CloudFormation.Types.HandlerErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.HandlerErrorCode
  ( HandlerErrorCode
      ( ..,
        HandlerErrorCode_AccessDenied,
        HandlerErrorCode_AlreadyExists,
        HandlerErrorCode_GeneralServiceException,
        HandlerErrorCode_HandlerInternalFailure,
        HandlerErrorCode_InternalFailure,
        HandlerErrorCode_InvalidCredentials,
        HandlerErrorCode_InvalidRequest,
        HandlerErrorCode_InvalidTypeConfiguration,
        HandlerErrorCode_NetworkFailure,
        HandlerErrorCode_NonCompliant,
        HandlerErrorCode_NotFound,
        HandlerErrorCode_NotStabilized,
        HandlerErrorCode_NotUpdatable,
        HandlerErrorCode_ResourceConflict,
        HandlerErrorCode_ServiceInternalError,
        HandlerErrorCode_ServiceLimitExceeded,
        HandlerErrorCode_Throttling,
        HandlerErrorCode_Unknown,
        HandlerErrorCode_UnsupportedTarget
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HandlerErrorCode = HandlerErrorCode'
  { fromHandlerErrorCode ::
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

pattern HandlerErrorCode_AccessDenied :: HandlerErrorCode
pattern HandlerErrorCode_AccessDenied = HandlerErrorCode' "AccessDenied"

pattern HandlerErrorCode_AlreadyExists :: HandlerErrorCode
pattern HandlerErrorCode_AlreadyExists = HandlerErrorCode' "AlreadyExists"

pattern HandlerErrorCode_GeneralServiceException :: HandlerErrorCode
pattern HandlerErrorCode_GeneralServiceException = HandlerErrorCode' "GeneralServiceException"

pattern HandlerErrorCode_HandlerInternalFailure :: HandlerErrorCode
pattern HandlerErrorCode_HandlerInternalFailure = HandlerErrorCode' "HandlerInternalFailure"

pattern HandlerErrorCode_InternalFailure :: HandlerErrorCode
pattern HandlerErrorCode_InternalFailure = HandlerErrorCode' "InternalFailure"

pattern HandlerErrorCode_InvalidCredentials :: HandlerErrorCode
pattern HandlerErrorCode_InvalidCredentials = HandlerErrorCode' "InvalidCredentials"

pattern HandlerErrorCode_InvalidRequest :: HandlerErrorCode
pattern HandlerErrorCode_InvalidRequest = HandlerErrorCode' "InvalidRequest"

pattern HandlerErrorCode_InvalidTypeConfiguration :: HandlerErrorCode
pattern HandlerErrorCode_InvalidTypeConfiguration = HandlerErrorCode' "InvalidTypeConfiguration"

pattern HandlerErrorCode_NetworkFailure :: HandlerErrorCode
pattern HandlerErrorCode_NetworkFailure = HandlerErrorCode' "NetworkFailure"

pattern HandlerErrorCode_NonCompliant :: HandlerErrorCode
pattern HandlerErrorCode_NonCompliant = HandlerErrorCode' "NonCompliant"

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

pattern HandlerErrorCode_Unknown :: HandlerErrorCode
pattern HandlerErrorCode_Unknown = HandlerErrorCode' "Unknown"

pattern HandlerErrorCode_UnsupportedTarget :: HandlerErrorCode
pattern HandlerErrorCode_UnsupportedTarget = HandlerErrorCode' "UnsupportedTarget"

{-# COMPLETE
  HandlerErrorCode_AccessDenied,
  HandlerErrorCode_AlreadyExists,
  HandlerErrorCode_GeneralServiceException,
  HandlerErrorCode_HandlerInternalFailure,
  HandlerErrorCode_InternalFailure,
  HandlerErrorCode_InvalidCredentials,
  HandlerErrorCode_InvalidRequest,
  HandlerErrorCode_InvalidTypeConfiguration,
  HandlerErrorCode_NetworkFailure,
  HandlerErrorCode_NonCompliant,
  HandlerErrorCode_NotFound,
  HandlerErrorCode_NotStabilized,
  HandlerErrorCode_NotUpdatable,
  HandlerErrorCode_ResourceConflict,
  HandlerErrorCode_ServiceInternalError,
  HandlerErrorCode_ServiceLimitExceeded,
  HandlerErrorCode_Throttling,
  HandlerErrorCode_Unknown,
  HandlerErrorCode_UnsupportedTarget,
  HandlerErrorCode'
  #-}
