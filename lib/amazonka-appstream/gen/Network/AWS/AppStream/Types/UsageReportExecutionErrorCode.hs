{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UsageReportExecutionErrorCode where

import Network.AWS.Prelude

data UsageReportExecutionErrorCode
  = UREECAccessDenied
  | UREECInternalServiceError
  | UREECResourceNotFound
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText UsageReportExecutionErrorCode where
  parser =
    takeLowerText >>= \case
      "access_denied" -> pure UREECAccessDenied
      "internal_service_error" -> pure UREECInternalServiceError
      "resource_not_found" -> pure UREECResourceNotFound
      e ->
        fromTextError $
          "Failure parsing UsageReportExecutionErrorCode from value: '" <> e
            <> "'. Accepted values: access_denied, internal_service_error, resource_not_found"

instance ToText UsageReportExecutionErrorCode where
  toText = \case
    UREECAccessDenied -> "ACCESS_DENIED"
    UREECInternalServiceError -> "INTERNAL_SERVICE_ERROR"
    UREECResourceNotFound -> "RESOURCE_NOT_FOUND"

instance Hashable UsageReportExecutionErrorCode

instance NFData UsageReportExecutionErrorCode

instance ToByteString UsageReportExecutionErrorCode

instance ToQuery UsageReportExecutionErrorCode

instance ToHeader UsageReportExecutionErrorCode

instance FromJSON UsageReportExecutionErrorCode where
  parseJSON = parseJSONText "UsageReportExecutionErrorCode"
