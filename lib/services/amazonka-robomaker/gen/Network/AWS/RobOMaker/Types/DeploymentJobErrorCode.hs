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
-- Module      : Amazonka.RobOMaker.Types.DeploymentJobErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.DeploymentJobErrorCode
  ( DeploymentJobErrorCode
      ( ..,
        DeploymentJobErrorCode_BadLambdaAssociated,
        DeploymentJobErrorCode_BadPermissionError,
        DeploymentJobErrorCode_DeploymentFleetDoesNotExist,
        DeploymentJobErrorCode_DownloadConditionFailed,
        DeploymentJobErrorCode_EnvironmentSetupError,
        DeploymentJobErrorCode_EtagMismatch,
        DeploymentJobErrorCode_ExtractingBundleFailure,
        DeploymentJobErrorCode_FailureThresholdBreached,
        DeploymentJobErrorCode_FleetDeploymentTimeout,
        DeploymentJobErrorCode_GreengrassDeploymentFailed,
        DeploymentJobErrorCode_GreengrassGroupVersionDoesNotExist,
        DeploymentJobErrorCode_InternalServerError,
        DeploymentJobErrorCode_InvalidGreengrassGroup,
        DeploymentJobErrorCode_LambdaDeleted,
        DeploymentJobErrorCode_MissingRobotApplicationArchitecture,
        DeploymentJobErrorCode_MissingRobotArchitecture,
        DeploymentJobErrorCode_MissingRobotDeploymentResource,
        DeploymentJobErrorCode_PostLaunchFileFailure,
        DeploymentJobErrorCode_PreLaunchFileFailure,
        DeploymentJobErrorCode_ResourceNotFound,
        DeploymentJobErrorCode_RobotAgentConnectionTimeout,
        DeploymentJobErrorCode_RobotApplicationDoesNotExist,
        DeploymentJobErrorCode_RobotDeploymentAborted,
        DeploymentJobErrorCode_RobotDeploymentNoResponse
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeploymentJobErrorCode = DeploymentJobErrorCode'
  { fromDeploymentJobErrorCode ::
      Core.Text
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

pattern DeploymentJobErrorCode_BadLambdaAssociated :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_BadLambdaAssociated = DeploymentJobErrorCode' "BadLambdaAssociated"

pattern DeploymentJobErrorCode_BadPermissionError :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_BadPermissionError = DeploymentJobErrorCode' "BadPermissionError"

pattern DeploymentJobErrorCode_DeploymentFleetDoesNotExist :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_DeploymentFleetDoesNotExist = DeploymentJobErrorCode' "DeploymentFleetDoesNotExist"

pattern DeploymentJobErrorCode_DownloadConditionFailed :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_DownloadConditionFailed = DeploymentJobErrorCode' "DownloadConditionFailed"

pattern DeploymentJobErrorCode_EnvironmentSetupError :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_EnvironmentSetupError = DeploymentJobErrorCode' "EnvironmentSetupError"

pattern DeploymentJobErrorCode_EtagMismatch :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_EtagMismatch = DeploymentJobErrorCode' "EtagMismatch"

pattern DeploymentJobErrorCode_ExtractingBundleFailure :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_ExtractingBundleFailure = DeploymentJobErrorCode' "ExtractingBundleFailure"

pattern DeploymentJobErrorCode_FailureThresholdBreached :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_FailureThresholdBreached = DeploymentJobErrorCode' "FailureThresholdBreached"

pattern DeploymentJobErrorCode_FleetDeploymentTimeout :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_FleetDeploymentTimeout = DeploymentJobErrorCode' "FleetDeploymentTimeout"

pattern DeploymentJobErrorCode_GreengrassDeploymentFailed :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_GreengrassDeploymentFailed = DeploymentJobErrorCode' "GreengrassDeploymentFailed"

pattern DeploymentJobErrorCode_GreengrassGroupVersionDoesNotExist :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_GreengrassGroupVersionDoesNotExist = DeploymentJobErrorCode' "GreengrassGroupVersionDoesNotExist"

pattern DeploymentJobErrorCode_InternalServerError :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_InternalServerError = DeploymentJobErrorCode' "InternalServerError"

pattern DeploymentJobErrorCode_InvalidGreengrassGroup :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_InvalidGreengrassGroup = DeploymentJobErrorCode' "InvalidGreengrassGroup"

pattern DeploymentJobErrorCode_LambdaDeleted :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_LambdaDeleted = DeploymentJobErrorCode' "LambdaDeleted"

pattern DeploymentJobErrorCode_MissingRobotApplicationArchitecture :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_MissingRobotApplicationArchitecture = DeploymentJobErrorCode' "MissingRobotApplicationArchitecture"

pattern DeploymentJobErrorCode_MissingRobotArchitecture :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_MissingRobotArchitecture = DeploymentJobErrorCode' "MissingRobotArchitecture"

pattern DeploymentJobErrorCode_MissingRobotDeploymentResource :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_MissingRobotDeploymentResource = DeploymentJobErrorCode' "MissingRobotDeploymentResource"

pattern DeploymentJobErrorCode_PostLaunchFileFailure :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_PostLaunchFileFailure = DeploymentJobErrorCode' "PostLaunchFileFailure"

pattern DeploymentJobErrorCode_PreLaunchFileFailure :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_PreLaunchFileFailure = DeploymentJobErrorCode' "PreLaunchFileFailure"

pattern DeploymentJobErrorCode_ResourceNotFound :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_ResourceNotFound = DeploymentJobErrorCode' "ResourceNotFound"

pattern DeploymentJobErrorCode_RobotAgentConnectionTimeout :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_RobotAgentConnectionTimeout = DeploymentJobErrorCode' "RobotAgentConnectionTimeout"

pattern DeploymentJobErrorCode_RobotApplicationDoesNotExist :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_RobotApplicationDoesNotExist = DeploymentJobErrorCode' "RobotApplicationDoesNotExist"

pattern DeploymentJobErrorCode_RobotDeploymentAborted :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_RobotDeploymentAborted = DeploymentJobErrorCode' "RobotDeploymentAborted"

pattern DeploymentJobErrorCode_RobotDeploymentNoResponse :: DeploymentJobErrorCode
pattern DeploymentJobErrorCode_RobotDeploymentNoResponse = DeploymentJobErrorCode' "RobotDeploymentNoResponse"

{-# COMPLETE
  DeploymentJobErrorCode_BadLambdaAssociated,
  DeploymentJobErrorCode_BadPermissionError,
  DeploymentJobErrorCode_DeploymentFleetDoesNotExist,
  DeploymentJobErrorCode_DownloadConditionFailed,
  DeploymentJobErrorCode_EnvironmentSetupError,
  DeploymentJobErrorCode_EtagMismatch,
  DeploymentJobErrorCode_ExtractingBundleFailure,
  DeploymentJobErrorCode_FailureThresholdBreached,
  DeploymentJobErrorCode_FleetDeploymentTimeout,
  DeploymentJobErrorCode_GreengrassDeploymentFailed,
  DeploymentJobErrorCode_GreengrassGroupVersionDoesNotExist,
  DeploymentJobErrorCode_InternalServerError,
  DeploymentJobErrorCode_InvalidGreengrassGroup,
  DeploymentJobErrorCode_LambdaDeleted,
  DeploymentJobErrorCode_MissingRobotApplicationArchitecture,
  DeploymentJobErrorCode_MissingRobotArchitecture,
  DeploymentJobErrorCode_MissingRobotDeploymentResource,
  DeploymentJobErrorCode_PostLaunchFileFailure,
  DeploymentJobErrorCode_PreLaunchFileFailure,
  DeploymentJobErrorCode_ResourceNotFound,
  DeploymentJobErrorCode_RobotAgentConnectionTimeout,
  DeploymentJobErrorCode_RobotApplicationDoesNotExist,
  DeploymentJobErrorCode_RobotDeploymentAborted,
  DeploymentJobErrorCode_RobotDeploymentNoResponse,
  DeploymentJobErrorCode'
  #-}
