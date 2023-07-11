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
-- Module      : Amazonka.RobOMaker.Types.SimulationJobErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationJobErrorCode
  ( SimulationJobErrorCode
      ( ..,
        SimulationJobErrorCode_BadPermissionsCloudwatchLogs,
        SimulationJobErrorCode_BadPermissionsRobotApplication,
        SimulationJobErrorCode_BadPermissionsS3Object,
        SimulationJobErrorCode_BadPermissionsS3Output,
        SimulationJobErrorCode_BadPermissionsSimulationApplication,
        SimulationJobErrorCode_BadPermissionsUserCredentials,
        SimulationJobErrorCode_BatchCanceled,
        SimulationJobErrorCode_BatchTimedOut,
        SimulationJobErrorCode_ENILimitExceeded,
        SimulationJobErrorCode_InternalServiceError,
        SimulationJobErrorCode_InvalidBundleRobotApplication,
        SimulationJobErrorCode_InvalidBundleSimulationApplication,
        SimulationJobErrorCode_InvalidInput,
        SimulationJobErrorCode_InvalidS3Resource,
        SimulationJobErrorCode_LimitExceeded,
        SimulationJobErrorCode_MismatchedEtag,
        SimulationJobErrorCode_RequestThrottled,
        SimulationJobErrorCode_ResourceNotFound,
        SimulationJobErrorCode_RobotApplicationCrash,
        SimulationJobErrorCode_RobotApplicationHealthCheckFailure,
        SimulationJobErrorCode_RobotApplicationVersionMismatchedEtag,
        SimulationJobErrorCode_SimulationApplicationCrash,
        SimulationJobErrorCode_SimulationApplicationHealthCheckFailure,
        SimulationJobErrorCode_SimulationApplicationVersionMismatchedEtag,
        SimulationJobErrorCode_SubnetIpLimitExceeded,
        SimulationJobErrorCode_ThrottlingError,
        SimulationJobErrorCode_UploadContentMismatchError,
        SimulationJobErrorCode_WrongRegionRobotApplication,
        SimulationJobErrorCode_WrongRegionS3Bucket,
        SimulationJobErrorCode_WrongRegionS3Output,
        SimulationJobErrorCode_WrongRegionSimulationApplication
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SimulationJobErrorCode = SimulationJobErrorCode'
  { fromSimulationJobErrorCode ::
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

pattern SimulationJobErrorCode_BadPermissionsCloudwatchLogs :: SimulationJobErrorCode
pattern SimulationJobErrorCode_BadPermissionsCloudwatchLogs = SimulationJobErrorCode' "BadPermissionsCloudwatchLogs"

pattern SimulationJobErrorCode_BadPermissionsRobotApplication :: SimulationJobErrorCode
pattern SimulationJobErrorCode_BadPermissionsRobotApplication = SimulationJobErrorCode' "BadPermissionsRobotApplication"

pattern SimulationJobErrorCode_BadPermissionsS3Object :: SimulationJobErrorCode
pattern SimulationJobErrorCode_BadPermissionsS3Object = SimulationJobErrorCode' "BadPermissionsS3Object"

pattern SimulationJobErrorCode_BadPermissionsS3Output :: SimulationJobErrorCode
pattern SimulationJobErrorCode_BadPermissionsS3Output = SimulationJobErrorCode' "BadPermissionsS3Output"

pattern SimulationJobErrorCode_BadPermissionsSimulationApplication :: SimulationJobErrorCode
pattern SimulationJobErrorCode_BadPermissionsSimulationApplication = SimulationJobErrorCode' "BadPermissionsSimulationApplication"

pattern SimulationJobErrorCode_BadPermissionsUserCredentials :: SimulationJobErrorCode
pattern SimulationJobErrorCode_BadPermissionsUserCredentials = SimulationJobErrorCode' "BadPermissionsUserCredentials"

pattern SimulationJobErrorCode_BatchCanceled :: SimulationJobErrorCode
pattern SimulationJobErrorCode_BatchCanceled = SimulationJobErrorCode' "BatchCanceled"

pattern SimulationJobErrorCode_BatchTimedOut :: SimulationJobErrorCode
pattern SimulationJobErrorCode_BatchTimedOut = SimulationJobErrorCode' "BatchTimedOut"

pattern SimulationJobErrorCode_ENILimitExceeded :: SimulationJobErrorCode
pattern SimulationJobErrorCode_ENILimitExceeded = SimulationJobErrorCode' "ENILimitExceeded"

pattern SimulationJobErrorCode_InternalServiceError :: SimulationJobErrorCode
pattern SimulationJobErrorCode_InternalServiceError = SimulationJobErrorCode' "InternalServiceError"

pattern SimulationJobErrorCode_InvalidBundleRobotApplication :: SimulationJobErrorCode
pattern SimulationJobErrorCode_InvalidBundleRobotApplication = SimulationJobErrorCode' "InvalidBundleRobotApplication"

pattern SimulationJobErrorCode_InvalidBundleSimulationApplication :: SimulationJobErrorCode
pattern SimulationJobErrorCode_InvalidBundleSimulationApplication = SimulationJobErrorCode' "InvalidBundleSimulationApplication"

pattern SimulationJobErrorCode_InvalidInput :: SimulationJobErrorCode
pattern SimulationJobErrorCode_InvalidInput = SimulationJobErrorCode' "InvalidInput"

pattern SimulationJobErrorCode_InvalidS3Resource :: SimulationJobErrorCode
pattern SimulationJobErrorCode_InvalidS3Resource = SimulationJobErrorCode' "InvalidS3Resource"

pattern SimulationJobErrorCode_LimitExceeded :: SimulationJobErrorCode
pattern SimulationJobErrorCode_LimitExceeded = SimulationJobErrorCode' "LimitExceeded"

pattern SimulationJobErrorCode_MismatchedEtag :: SimulationJobErrorCode
pattern SimulationJobErrorCode_MismatchedEtag = SimulationJobErrorCode' "MismatchedEtag"

pattern SimulationJobErrorCode_RequestThrottled :: SimulationJobErrorCode
pattern SimulationJobErrorCode_RequestThrottled = SimulationJobErrorCode' "RequestThrottled"

pattern SimulationJobErrorCode_ResourceNotFound :: SimulationJobErrorCode
pattern SimulationJobErrorCode_ResourceNotFound = SimulationJobErrorCode' "ResourceNotFound"

pattern SimulationJobErrorCode_RobotApplicationCrash :: SimulationJobErrorCode
pattern SimulationJobErrorCode_RobotApplicationCrash = SimulationJobErrorCode' "RobotApplicationCrash"

pattern SimulationJobErrorCode_RobotApplicationHealthCheckFailure :: SimulationJobErrorCode
pattern SimulationJobErrorCode_RobotApplicationHealthCheckFailure = SimulationJobErrorCode' "RobotApplicationHealthCheckFailure"

pattern SimulationJobErrorCode_RobotApplicationVersionMismatchedEtag :: SimulationJobErrorCode
pattern SimulationJobErrorCode_RobotApplicationVersionMismatchedEtag = SimulationJobErrorCode' "RobotApplicationVersionMismatchedEtag"

pattern SimulationJobErrorCode_SimulationApplicationCrash :: SimulationJobErrorCode
pattern SimulationJobErrorCode_SimulationApplicationCrash = SimulationJobErrorCode' "SimulationApplicationCrash"

pattern SimulationJobErrorCode_SimulationApplicationHealthCheckFailure :: SimulationJobErrorCode
pattern SimulationJobErrorCode_SimulationApplicationHealthCheckFailure = SimulationJobErrorCode' "SimulationApplicationHealthCheckFailure"

pattern SimulationJobErrorCode_SimulationApplicationVersionMismatchedEtag :: SimulationJobErrorCode
pattern SimulationJobErrorCode_SimulationApplicationVersionMismatchedEtag = SimulationJobErrorCode' "SimulationApplicationVersionMismatchedEtag"

pattern SimulationJobErrorCode_SubnetIpLimitExceeded :: SimulationJobErrorCode
pattern SimulationJobErrorCode_SubnetIpLimitExceeded = SimulationJobErrorCode' "SubnetIpLimitExceeded"

pattern SimulationJobErrorCode_ThrottlingError :: SimulationJobErrorCode
pattern SimulationJobErrorCode_ThrottlingError = SimulationJobErrorCode' "ThrottlingError"

pattern SimulationJobErrorCode_UploadContentMismatchError :: SimulationJobErrorCode
pattern SimulationJobErrorCode_UploadContentMismatchError = SimulationJobErrorCode' "UploadContentMismatchError"

pattern SimulationJobErrorCode_WrongRegionRobotApplication :: SimulationJobErrorCode
pattern SimulationJobErrorCode_WrongRegionRobotApplication = SimulationJobErrorCode' "WrongRegionRobotApplication"

pattern SimulationJobErrorCode_WrongRegionS3Bucket :: SimulationJobErrorCode
pattern SimulationJobErrorCode_WrongRegionS3Bucket = SimulationJobErrorCode' "WrongRegionS3Bucket"

pattern SimulationJobErrorCode_WrongRegionS3Output :: SimulationJobErrorCode
pattern SimulationJobErrorCode_WrongRegionS3Output = SimulationJobErrorCode' "WrongRegionS3Output"

pattern SimulationJobErrorCode_WrongRegionSimulationApplication :: SimulationJobErrorCode
pattern SimulationJobErrorCode_WrongRegionSimulationApplication = SimulationJobErrorCode' "WrongRegionSimulationApplication"

{-# COMPLETE
  SimulationJobErrorCode_BadPermissionsCloudwatchLogs,
  SimulationJobErrorCode_BadPermissionsRobotApplication,
  SimulationJobErrorCode_BadPermissionsS3Object,
  SimulationJobErrorCode_BadPermissionsS3Output,
  SimulationJobErrorCode_BadPermissionsSimulationApplication,
  SimulationJobErrorCode_BadPermissionsUserCredentials,
  SimulationJobErrorCode_BatchCanceled,
  SimulationJobErrorCode_BatchTimedOut,
  SimulationJobErrorCode_ENILimitExceeded,
  SimulationJobErrorCode_InternalServiceError,
  SimulationJobErrorCode_InvalidBundleRobotApplication,
  SimulationJobErrorCode_InvalidBundleSimulationApplication,
  SimulationJobErrorCode_InvalidInput,
  SimulationJobErrorCode_InvalidS3Resource,
  SimulationJobErrorCode_LimitExceeded,
  SimulationJobErrorCode_MismatchedEtag,
  SimulationJobErrorCode_RequestThrottled,
  SimulationJobErrorCode_ResourceNotFound,
  SimulationJobErrorCode_RobotApplicationCrash,
  SimulationJobErrorCode_RobotApplicationHealthCheckFailure,
  SimulationJobErrorCode_RobotApplicationVersionMismatchedEtag,
  SimulationJobErrorCode_SimulationApplicationCrash,
  SimulationJobErrorCode_SimulationApplicationHealthCheckFailure,
  SimulationJobErrorCode_SimulationApplicationVersionMismatchedEtag,
  SimulationJobErrorCode_SubnetIpLimitExceeded,
  SimulationJobErrorCode_ThrottlingError,
  SimulationJobErrorCode_UploadContentMismatchError,
  SimulationJobErrorCode_WrongRegionRobotApplication,
  SimulationJobErrorCode_WrongRegionS3Bucket,
  SimulationJobErrorCode_WrongRegionS3Output,
  SimulationJobErrorCode_WrongRegionSimulationApplication,
  SimulationJobErrorCode'
  #-}
