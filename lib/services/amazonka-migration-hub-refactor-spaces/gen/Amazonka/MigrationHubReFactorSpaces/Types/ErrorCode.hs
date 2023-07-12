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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_INVALID_RESOURCE_STATE,
        ErrorCode_NOT_AUTHORIZED,
        ErrorCode_REQUEST_LIMIT_EXCEEDED,
        ErrorCode_RESOURCE_CREATION_FAILURE,
        ErrorCode_RESOURCE_DELETION_FAILURE,
        ErrorCode_RESOURCE_IN_USE,
        ErrorCode_RESOURCE_LIMIT_EXCEEDED,
        ErrorCode_RESOURCE_NOT_FOUND,
        ErrorCode_RESOURCE_RETRIEVAL_FAILURE,
        ErrorCode_RESOURCE_UPDATE_FAILURE,
        ErrorCode_SERVICE_ENDPOINT_HEALTH_CHECK_FAILURE,
        ErrorCode_STATE_TRANSITION_FAILURE
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

pattern ErrorCode_INVALID_RESOURCE_STATE :: ErrorCode
pattern ErrorCode_INVALID_RESOURCE_STATE = ErrorCode' "INVALID_RESOURCE_STATE"

pattern ErrorCode_NOT_AUTHORIZED :: ErrorCode
pattern ErrorCode_NOT_AUTHORIZED = ErrorCode' "NOT_AUTHORIZED"

pattern ErrorCode_REQUEST_LIMIT_EXCEEDED :: ErrorCode
pattern ErrorCode_REQUEST_LIMIT_EXCEEDED = ErrorCode' "REQUEST_LIMIT_EXCEEDED"

pattern ErrorCode_RESOURCE_CREATION_FAILURE :: ErrorCode
pattern ErrorCode_RESOURCE_CREATION_FAILURE = ErrorCode' "RESOURCE_CREATION_FAILURE"

pattern ErrorCode_RESOURCE_DELETION_FAILURE :: ErrorCode
pattern ErrorCode_RESOURCE_DELETION_FAILURE = ErrorCode' "RESOURCE_DELETION_FAILURE"

pattern ErrorCode_RESOURCE_IN_USE :: ErrorCode
pattern ErrorCode_RESOURCE_IN_USE = ErrorCode' "RESOURCE_IN_USE"

pattern ErrorCode_RESOURCE_LIMIT_EXCEEDED :: ErrorCode
pattern ErrorCode_RESOURCE_LIMIT_EXCEEDED = ErrorCode' "RESOURCE_LIMIT_EXCEEDED"

pattern ErrorCode_RESOURCE_NOT_FOUND :: ErrorCode
pattern ErrorCode_RESOURCE_NOT_FOUND = ErrorCode' "RESOURCE_NOT_FOUND"

pattern ErrorCode_RESOURCE_RETRIEVAL_FAILURE :: ErrorCode
pattern ErrorCode_RESOURCE_RETRIEVAL_FAILURE = ErrorCode' "RESOURCE_RETRIEVAL_FAILURE"

pattern ErrorCode_RESOURCE_UPDATE_FAILURE :: ErrorCode
pattern ErrorCode_RESOURCE_UPDATE_FAILURE = ErrorCode' "RESOURCE_UPDATE_FAILURE"

pattern ErrorCode_SERVICE_ENDPOINT_HEALTH_CHECK_FAILURE :: ErrorCode
pattern ErrorCode_SERVICE_ENDPOINT_HEALTH_CHECK_FAILURE = ErrorCode' "SERVICE_ENDPOINT_HEALTH_CHECK_FAILURE"

pattern ErrorCode_STATE_TRANSITION_FAILURE :: ErrorCode
pattern ErrorCode_STATE_TRANSITION_FAILURE = ErrorCode' "STATE_TRANSITION_FAILURE"

{-# COMPLETE
  ErrorCode_INVALID_RESOURCE_STATE,
  ErrorCode_NOT_AUTHORIZED,
  ErrorCode_REQUEST_LIMIT_EXCEEDED,
  ErrorCode_RESOURCE_CREATION_FAILURE,
  ErrorCode_RESOURCE_DELETION_FAILURE,
  ErrorCode_RESOURCE_IN_USE,
  ErrorCode_RESOURCE_LIMIT_EXCEEDED,
  ErrorCode_RESOURCE_NOT_FOUND,
  ErrorCode_RESOURCE_RETRIEVAL_FAILURE,
  ErrorCode_RESOURCE_UPDATE_FAILURE,
  ErrorCode_SERVICE_ENDPOINT_HEALTH_CHECK_FAILURE,
  ErrorCode_STATE_TRANSITION_FAILURE,
  ErrorCode'
  #-}
