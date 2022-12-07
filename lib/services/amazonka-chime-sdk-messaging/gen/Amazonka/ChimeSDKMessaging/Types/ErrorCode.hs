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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_AccessDenied,
        ErrorCode_BadRequest,
        ErrorCode_Conflict,
        ErrorCode_Forbidden,
        ErrorCode_NotFound,
        ErrorCode_PhoneNumberAssociationsExist,
        ErrorCode_PreconditionFailed,
        ErrorCode_ResourceLimitExceeded,
        ErrorCode_ServiceFailure,
        ErrorCode_ServiceUnavailable,
        ErrorCode_Throttled,
        ErrorCode_Throttling,
        ErrorCode_Unauthorized,
        ErrorCode_Unprocessable,
        ErrorCode_VoiceConnectorGroupAssociationsExist
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ErrorCode = ErrorCode'
  { fromErrorCode ::
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

pattern ErrorCode_AccessDenied :: ErrorCode
pattern ErrorCode_AccessDenied = ErrorCode' "AccessDenied"

pattern ErrorCode_BadRequest :: ErrorCode
pattern ErrorCode_BadRequest = ErrorCode' "BadRequest"

pattern ErrorCode_Conflict :: ErrorCode
pattern ErrorCode_Conflict = ErrorCode' "Conflict"

pattern ErrorCode_Forbidden :: ErrorCode
pattern ErrorCode_Forbidden = ErrorCode' "Forbidden"

pattern ErrorCode_NotFound :: ErrorCode
pattern ErrorCode_NotFound = ErrorCode' "NotFound"

pattern ErrorCode_PhoneNumberAssociationsExist :: ErrorCode
pattern ErrorCode_PhoneNumberAssociationsExist = ErrorCode' "PhoneNumberAssociationsExist"

pattern ErrorCode_PreconditionFailed :: ErrorCode
pattern ErrorCode_PreconditionFailed = ErrorCode' "PreconditionFailed"

pattern ErrorCode_ResourceLimitExceeded :: ErrorCode
pattern ErrorCode_ResourceLimitExceeded = ErrorCode' "ResourceLimitExceeded"

pattern ErrorCode_ServiceFailure :: ErrorCode
pattern ErrorCode_ServiceFailure = ErrorCode' "ServiceFailure"

pattern ErrorCode_ServiceUnavailable :: ErrorCode
pattern ErrorCode_ServiceUnavailable = ErrorCode' "ServiceUnavailable"

pattern ErrorCode_Throttled :: ErrorCode
pattern ErrorCode_Throttled = ErrorCode' "Throttled"

pattern ErrorCode_Throttling :: ErrorCode
pattern ErrorCode_Throttling = ErrorCode' "Throttling"

pattern ErrorCode_Unauthorized :: ErrorCode
pattern ErrorCode_Unauthorized = ErrorCode' "Unauthorized"

pattern ErrorCode_Unprocessable :: ErrorCode
pattern ErrorCode_Unprocessable = ErrorCode' "Unprocessable"

pattern ErrorCode_VoiceConnectorGroupAssociationsExist :: ErrorCode
pattern ErrorCode_VoiceConnectorGroupAssociationsExist = ErrorCode' "VoiceConnectorGroupAssociationsExist"

{-# COMPLETE
  ErrorCode_AccessDenied,
  ErrorCode_BadRequest,
  ErrorCode_Conflict,
  ErrorCode_Forbidden,
  ErrorCode_NotFound,
  ErrorCode_PhoneNumberAssociationsExist,
  ErrorCode_PreconditionFailed,
  ErrorCode_ResourceLimitExceeded,
  ErrorCode_ServiceFailure,
  ErrorCode_ServiceUnavailable,
  ErrorCode_Throttled,
  ErrorCode_Throttling,
  ErrorCode_Unauthorized,
  ErrorCode_Unprocessable,
  ErrorCode_VoiceConnectorGroupAssociationsExist,
  ErrorCode'
  #-}
