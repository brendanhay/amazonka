{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause where

import Network.AWS.Prelude

data ScheduleLambdaFunctionFailedCause
  = IdAlreadyInUse
  | LambdaFunctionCreationRateExceeded
  | LambdaServiceNotAvailableInRegion
  | OpenLambdaFunctionsLimitExceeded
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

instance FromText ScheduleLambdaFunctionFailedCause where
  parser =
    takeLowerText >>= \case
      "id_already_in_use" -> pure IdAlreadyInUse
      "lambda_function_creation_rate_exceeded" -> pure LambdaFunctionCreationRateExceeded
      "lambda_service_not_available_in_region" -> pure LambdaServiceNotAvailableInRegion
      "open_lambda_functions_limit_exceeded" -> pure OpenLambdaFunctionsLimitExceeded
      e ->
        fromTextError $
          "Failure parsing ScheduleLambdaFunctionFailedCause from value: '" <> e
            <> "'. Accepted values: id_already_in_use, lambda_function_creation_rate_exceeded, lambda_service_not_available_in_region, open_lambda_functions_limit_exceeded"

instance ToText ScheduleLambdaFunctionFailedCause where
  toText = \case
    IdAlreadyInUse -> "ID_ALREADY_IN_USE"
    LambdaFunctionCreationRateExceeded -> "LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED"
    LambdaServiceNotAvailableInRegion -> "LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION"
    OpenLambdaFunctionsLimitExceeded -> "OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED"

instance Hashable ScheduleLambdaFunctionFailedCause

instance NFData ScheduleLambdaFunctionFailedCause

instance ToByteString ScheduleLambdaFunctionFailedCause

instance ToQuery ScheduleLambdaFunctionFailedCause

instance ToHeader ScheduleLambdaFunctionFailedCause

instance FromJSON ScheduleLambdaFunctionFailedCause where
  parseJSON = parseJSONText "ScheduleLambdaFunctionFailedCause"
