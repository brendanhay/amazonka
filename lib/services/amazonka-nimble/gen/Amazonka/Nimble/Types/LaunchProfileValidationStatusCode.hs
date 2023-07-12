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
-- Module      : Amazonka.Nimble.Types.LaunchProfileValidationStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileValidationStatusCode
  ( LaunchProfileValidationStatusCode
      ( ..,
        LaunchProfileValidationStatusCode_VALIDATION_FAILED_INTERNAL_SERVER_ERROR,
        LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_ACTIVE_DIRECTORY,
        LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_SECURITY_GROUP_ASSOCIATION,
        LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_SUBNET_ROUTE_TABLE_ASSOCIATION,
        LaunchProfileValidationStatusCode_VALIDATION_FAILED_SUBNET_NOT_FOUND,
        LaunchProfileValidationStatusCode_VALIDATION_FAILED_UNAUTHORIZED,
        LaunchProfileValidationStatusCode_VALIDATION_IN_PROGRESS,
        LaunchProfileValidationStatusCode_VALIDATION_NOT_STARTED,
        LaunchProfileValidationStatusCode_VALIDATION_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LaunchProfileValidationStatusCode = LaunchProfileValidationStatusCode'
  { fromLaunchProfileValidationStatusCode ::
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

pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_INTERNAL_SERVER_ERROR :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_INTERNAL_SERVER_ERROR = LaunchProfileValidationStatusCode' "VALIDATION_FAILED_INTERNAL_SERVER_ERROR"

pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_ACTIVE_DIRECTORY :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_ACTIVE_DIRECTORY = LaunchProfileValidationStatusCode' "VALIDATION_FAILED_INVALID_ACTIVE_DIRECTORY"

pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_SECURITY_GROUP_ASSOCIATION :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_SECURITY_GROUP_ASSOCIATION = LaunchProfileValidationStatusCode' "VALIDATION_FAILED_INVALID_SECURITY_GROUP_ASSOCIATION"

pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_SUBNET_ROUTE_TABLE_ASSOCIATION :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_SUBNET_ROUTE_TABLE_ASSOCIATION = LaunchProfileValidationStatusCode' "VALIDATION_FAILED_INVALID_SUBNET_ROUTE_TABLE_ASSOCIATION"

pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_SUBNET_NOT_FOUND :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_SUBNET_NOT_FOUND = LaunchProfileValidationStatusCode' "VALIDATION_FAILED_SUBNET_NOT_FOUND"

pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_UNAUTHORIZED :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_FAILED_UNAUTHORIZED = LaunchProfileValidationStatusCode' "VALIDATION_FAILED_UNAUTHORIZED"

pattern LaunchProfileValidationStatusCode_VALIDATION_IN_PROGRESS :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_IN_PROGRESS = LaunchProfileValidationStatusCode' "VALIDATION_IN_PROGRESS"

pattern LaunchProfileValidationStatusCode_VALIDATION_NOT_STARTED :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_NOT_STARTED = LaunchProfileValidationStatusCode' "VALIDATION_NOT_STARTED"

pattern LaunchProfileValidationStatusCode_VALIDATION_SUCCESS :: LaunchProfileValidationStatusCode
pattern LaunchProfileValidationStatusCode_VALIDATION_SUCCESS = LaunchProfileValidationStatusCode' "VALIDATION_SUCCESS"

{-# COMPLETE
  LaunchProfileValidationStatusCode_VALIDATION_FAILED_INTERNAL_SERVER_ERROR,
  LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_ACTIVE_DIRECTORY,
  LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_SECURITY_GROUP_ASSOCIATION,
  LaunchProfileValidationStatusCode_VALIDATION_FAILED_INVALID_SUBNET_ROUTE_TABLE_ASSOCIATION,
  LaunchProfileValidationStatusCode_VALIDATION_FAILED_SUBNET_NOT_FOUND,
  LaunchProfileValidationStatusCode_VALIDATION_FAILED_UNAUTHORIZED,
  LaunchProfileValidationStatusCode_VALIDATION_IN_PROGRESS,
  LaunchProfileValidationStatusCode_VALIDATION_NOT_STARTED,
  LaunchProfileValidationStatusCode_VALIDATION_SUCCESS,
  LaunchProfileValidationStatusCode'
  #-}
