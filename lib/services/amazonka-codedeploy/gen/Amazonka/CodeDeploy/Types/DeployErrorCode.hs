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
-- Module      : Amazonka.CodeDeploy.Types.DeployErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeployErrorCode
  ( DeployErrorCode
      ( ..,
        DeployErrorCode_AGENT_ISSUE,
        DeployErrorCode_ALARM_ACTIVE,
        DeployErrorCode_APPLICATION_MISSING,
        DeployErrorCode_AUTOSCALING_VALIDATION_ERROR,
        DeployErrorCode_AUTO_SCALING_CONFIGURATION,
        DeployErrorCode_AUTO_SCALING_IAM_ROLE_PERMISSIONS,
        DeployErrorCode_CLOUDFORMATION_STACK_FAILURE,
        DeployErrorCode_CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND,
        DeployErrorCode_CUSTOMER_APPLICATION_UNHEALTHY,
        DeployErrorCode_DEPLOYMENT_GROUP_MISSING,
        DeployErrorCode_ECS_UPDATE_ERROR,
        DeployErrorCode_ELASTIC_LOAD_BALANCING_INVALID,
        DeployErrorCode_ELB_INVALID_INSTANCE,
        DeployErrorCode_HEALTH_CONSTRAINTS,
        DeployErrorCode_HEALTH_CONSTRAINTS_INVALID,
        DeployErrorCode_HOOK_EXECUTION_FAILURE,
        DeployErrorCode_IAM_ROLE_MISSING,
        DeployErrorCode_IAM_ROLE_PERMISSIONS,
        DeployErrorCode_INTERNAL_ERROR,
        DeployErrorCode_INVALID_ECS_SERVICE,
        DeployErrorCode_INVALID_LAMBDA_CONFIGURATION,
        DeployErrorCode_INVALID_LAMBDA_FUNCTION,
        DeployErrorCode_INVALID_REVISION,
        DeployErrorCode_MANUAL_STOP,
        DeployErrorCode_MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION,
        DeployErrorCode_MISSING_ELB_INFORMATION,
        DeployErrorCode_MISSING_GITHUB_TOKEN,
        DeployErrorCode_NO_EC2_SUBSCRIPTION,
        DeployErrorCode_NO_INSTANCES,
        DeployErrorCode_OVER_MAX_INSTANCES,
        DeployErrorCode_RESOURCE_LIMIT_EXCEEDED,
        DeployErrorCode_REVISION_MISSING,
        DeployErrorCode_THROTTLED,
        DeployErrorCode_TIMEOUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeployErrorCode = DeployErrorCode'
  { fromDeployErrorCode ::
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

pattern DeployErrorCode_AGENT_ISSUE :: DeployErrorCode
pattern DeployErrorCode_AGENT_ISSUE = DeployErrorCode' "AGENT_ISSUE"

pattern DeployErrorCode_ALARM_ACTIVE :: DeployErrorCode
pattern DeployErrorCode_ALARM_ACTIVE = DeployErrorCode' "ALARM_ACTIVE"

pattern DeployErrorCode_APPLICATION_MISSING :: DeployErrorCode
pattern DeployErrorCode_APPLICATION_MISSING = DeployErrorCode' "APPLICATION_MISSING"

pattern DeployErrorCode_AUTOSCALING_VALIDATION_ERROR :: DeployErrorCode
pattern DeployErrorCode_AUTOSCALING_VALIDATION_ERROR = DeployErrorCode' "AUTOSCALING_VALIDATION_ERROR"

pattern DeployErrorCode_AUTO_SCALING_CONFIGURATION :: DeployErrorCode
pattern DeployErrorCode_AUTO_SCALING_CONFIGURATION = DeployErrorCode' "AUTO_SCALING_CONFIGURATION"

pattern DeployErrorCode_AUTO_SCALING_IAM_ROLE_PERMISSIONS :: DeployErrorCode
pattern DeployErrorCode_AUTO_SCALING_IAM_ROLE_PERMISSIONS = DeployErrorCode' "AUTO_SCALING_IAM_ROLE_PERMISSIONS"

pattern DeployErrorCode_CLOUDFORMATION_STACK_FAILURE :: DeployErrorCode
pattern DeployErrorCode_CLOUDFORMATION_STACK_FAILURE = DeployErrorCode' "CLOUDFORMATION_STACK_FAILURE"

pattern DeployErrorCode_CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND :: DeployErrorCode
pattern DeployErrorCode_CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND = DeployErrorCode' "CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND"

pattern DeployErrorCode_CUSTOMER_APPLICATION_UNHEALTHY :: DeployErrorCode
pattern DeployErrorCode_CUSTOMER_APPLICATION_UNHEALTHY = DeployErrorCode' "CUSTOMER_APPLICATION_UNHEALTHY"

pattern DeployErrorCode_DEPLOYMENT_GROUP_MISSING :: DeployErrorCode
pattern DeployErrorCode_DEPLOYMENT_GROUP_MISSING = DeployErrorCode' "DEPLOYMENT_GROUP_MISSING"

pattern DeployErrorCode_ECS_UPDATE_ERROR :: DeployErrorCode
pattern DeployErrorCode_ECS_UPDATE_ERROR = DeployErrorCode' "ECS_UPDATE_ERROR"

pattern DeployErrorCode_ELASTIC_LOAD_BALANCING_INVALID :: DeployErrorCode
pattern DeployErrorCode_ELASTIC_LOAD_BALANCING_INVALID = DeployErrorCode' "ELASTIC_LOAD_BALANCING_INVALID"

pattern DeployErrorCode_ELB_INVALID_INSTANCE :: DeployErrorCode
pattern DeployErrorCode_ELB_INVALID_INSTANCE = DeployErrorCode' "ELB_INVALID_INSTANCE"

pattern DeployErrorCode_HEALTH_CONSTRAINTS :: DeployErrorCode
pattern DeployErrorCode_HEALTH_CONSTRAINTS = DeployErrorCode' "HEALTH_CONSTRAINTS"

pattern DeployErrorCode_HEALTH_CONSTRAINTS_INVALID :: DeployErrorCode
pattern DeployErrorCode_HEALTH_CONSTRAINTS_INVALID = DeployErrorCode' "HEALTH_CONSTRAINTS_INVALID"

pattern DeployErrorCode_HOOK_EXECUTION_FAILURE :: DeployErrorCode
pattern DeployErrorCode_HOOK_EXECUTION_FAILURE = DeployErrorCode' "HOOK_EXECUTION_FAILURE"

pattern DeployErrorCode_IAM_ROLE_MISSING :: DeployErrorCode
pattern DeployErrorCode_IAM_ROLE_MISSING = DeployErrorCode' "IAM_ROLE_MISSING"

pattern DeployErrorCode_IAM_ROLE_PERMISSIONS :: DeployErrorCode
pattern DeployErrorCode_IAM_ROLE_PERMISSIONS = DeployErrorCode' "IAM_ROLE_PERMISSIONS"

pattern DeployErrorCode_INTERNAL_ERROR :: DeployErrorCode
pattern DeployErrorCode_INTERNAL_ERROR = DeployErrorCode' "INTERNAL_ERROR"

pattern DeployErrorCode_INVALID_ECS_SERVICE :: DeployErrorCode
pattern DeployErrorCode_INVALID_ECS_SERVICE = DeployErrorCode' "INVALID_ECS_SERVICE"

pattern DeployErrorCode_INVALID_LAMBDA_CONFIGURATION :: DeployErrorCode
pattern DeployErrorCode_INVALID_LAMBDA_CONFIGURATION = DeployErrorCode' "INVALID_LAMBDA_CONFIGURATION"

pattern DeployErrorCode_INVALID_LAMBDA_FUNCTION :: DeployErrorCode
pattern DeployErrorCode_INVALID_LAMBDA_FUNCTION = DeployErrorCode' "INVALID_LAMBDA_FUNCTION"

pattern DeployErrorCode_INVALID_REVISION :: DeployErrorCode
pattern DeployErrorCode_INVALID_REVISION = DeployErrorCode' "INVALID_REVISION"

pattern DeployErrorCode_MANUAL_STOP :: DeployErrorCode
pattern DeployErrorCode_MANUAL_STOP = DeployErrorCode' "MANUAL_STOP"

pattern DeployErrorCode_MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION :: DeployErrorCode
pattern DeployErrorCode_MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION = DeployErrorCode' "MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION"

pattern DeployErrorCode_MISSING_ELB_INFORMATION :: DeployErrorCode
pattern DeployErrorCode_MISSING_ELB_INFORMATION = DeployErrorCode' "MISSING_ELB_INFORMATION"

pattern DeployErrorCode_MISSING_GITHUB_TOKEN :: DeployErrorCode
pattern DeployErrorCode_MISSING_GITHUB_TOKEN = DeployErrorCode' "MISSING_GITHUB_TOKEN"

pattern DeployErrorCode_NO_EC2_SUBSCRIPTION :: DeployErrorCode
pattern DeployErrorCode_NO_EC2_SUBSCRIPTION = DeployErrorCode' "NO_EC2_SUBSCRIPTION"

pattern DeployErrorCode_NO_INSTANCES :: DeployErrorCode
pattern DeployErrorCode_NO_INSTANCES = DeployErrorCode' "NO_INSTANCES"

pattern DeployErrorCode_OVER_MAX_INSTANCES :: DeployErrorCode
pattern DeployErrorCode_OVER_MAX_INSTANCES = DeployErrorCode' "OVER_MAX_INSTANCES"

pattern DeployErrorCode_RESOURCE_LIMIT_EXCEEDED :: DeployErrorCode
pattern DeployErrorCode_RESOURCE_LIMIT_EXCEEDED = DeployErrorCode' "RESOURCE_LIMIT_EXCEEDED"

pattern DeployErrorCode_REVISION_MISSING :: DeployErrorCode
pattern DeployErrorCode_REVISION_MISSING = DeployErrorCode' "REVISION_MISSING"

pattern DeployErrorCode_THROTTLED :: DeployErrorCode
pattern DeployErrorCode_THROTTLED = DeployErrorCode' "THROTTLED"

pattern DeployErrorCode_TIMEOUT :: DeployErrorCode
pattern DeployErrorCode_TIMEOUT = DeployErrorCode' "TIMEOUT"

{-# COMPLETE
  DeployErrorCode_AGENT_ISSUE,
  DeployErrorCode_ALARM_ACTIVE,
  DeployErrorCode_APPLICATION_MISSING,
  DeployErrorCode_AUTOSCALING_VALIDATION_ERROR,
  DeployErrorCode_AUTO_SCALING_CONFIGURATION,
  DeployErrorCode_AUTO_SCALING_IAM_ROLE_PERMISSIONS,
  DeployErrorCode_CLOUDFORMATION_STACK_FAILURE,
  DeployErrorCode_CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND,
  DeployErrorCode_CUSTOMER_APPLICATION_UNHEALTHY,
  DeployErrorCode_DEPLOYMENT_GROUP_MISSING,
  DeployErrorCode_ECS_UPDATE_ERROR,
  DeployErrorCode_ELASTIC_LOAD_BALANCING_INVALID,
  DeployErrorCode_ELB_INVALID_INSTANCE,
  DeployErrorCode_HEALTH_CONSTRAINTS,
  DeployErrorCode_HEALTH_CONSTRAINTS_INVALID,
  DeployErrorCode_HOOK_EXECUTION_FAILURE,
  DeployErrorCode_IAM_ROLE_MISSING,
  DeployErrorCode_IAM_ROLE_PERMISSIONS,
  DeployErrorCode_INTERNAL_ERROR,
  DeployErrorCode_INVALID_ECS_SERVICE,
  DeployErrorCode_INVALID_LAMBDA_CONFIGURATION,
  DeployErrorCode_INVALID_LAMBDA_FUNCTION,
  DeployErrorCode_INVALID_REVISION,
  DeployErrorCode_MANUAL_STOP,
  DeployErrorCode_MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION,
  DeployErrorCode_MISSING_ELB_INFORMATION,
  DeployErrorCode_MISSING_GITHUB_TOKEN,
  DeployErrorCode_NO_EC2_SUBSCRIPTION,
  DeployErrorCode_NO_INSTANCES,
  DeployErrorCode_OVER_MAX_INSTANCES,
  DeployErrorCode_RESOURCE_LIMIT_EXCEEDED,
  DeployErrorCode_REVISION_MISSING,
  DeployErrorCode_THROTTLED,
  DeployErrorCode_TIMEOUT,
  DeployErrorCode'
  #-}
