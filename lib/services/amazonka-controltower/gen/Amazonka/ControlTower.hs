{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ControlTower
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- These interfaces allow you to apply the AWS library of pre-defined
-- /controls/ to your organizational units, programmatically. In this
-- context, controls are the same as AWS Control Tower guardrails.
--
-- To call these APIs, you\'ll need to know:
--
-- -   the @ControlARN@ for the control--that is, the guardrail--you are
--     targeting,
--
-- -   and the ARN associated with the target organizational unit (OU).
--
-- __To get the @ControlARN@ for your AWS Control Tower guardrail:__
--
-- The @ControlARN@ contains the control name which is specified in each
-- guardrail. For a list of control names for /Strongly recommended/ and
-- /Elective/ guardrails, see
-- <https://docs.aws.amazon.com/controltower/latest/userguide/control-identifiers.html.html Resource identifiers for APIs and guardrails>
-- in the
-- <https://docs.aws.amazon.com/controltower/latest/userguide/automating-tasks.html Automating tasks section>
-- of the AWS Control Tower User Guide. Remember that /Mandatory/
-- guardrails cannot be added or removed.
--
-- __ARN format:__ @arn:aws:controltower:{REGION}::control\/{CONTROL_NAME}@
--
-- __Example:__
--
-- @arn:aws:controltower:us-west-2::control\/AWS-GR_AUTOSCALING_LAUNCH_CONFIG_PUBLIC_IP_DISABLED@
--
-- __To get the ARN for an OU:__
--
-- In the AWS Organizations console, you can find the ARN for the OU on the
-- __Organizational unit details__ page associated with that OU.
--
-- __OU ARN format:__
--
-- @arn:${Partition}:organizations::${MasterAccountId}:ou\/o-${OrganizationId}\/ou-${OrganizationalUnitId}@
--
-- __Details and examples__
--
-- -   <https://docs.aws.amazon.com/controltower/latest/userguide/control-identifiers.html List of resource identifiers for APIs and guardrails>
--
-- -   <https://docs.aws.amazon.com/controltower/latest/userguide/guardrail-api-examples-short.html Guardrail API examples (CLI)>
--
-- -   <https://docs.aws.amazon.com/controltower/latest/userguide/enable-controls.html Enable controls with AWS CloudFormation>
--
-- -   <https://docs.aws.amazon.com/controltower/latest/userguide/creating-resources-with-cloudformation.html Creating AWS Control Tower resources with AWS CloudFormation>
--
-- To view the open source resource repository on GitHub, see
-- <https://github.com/aws-cloudformation/aws-cloudformation-resource-providers-controltower aws-cloudformation\/aws-cloudformation-resource-providers-controltower>
--
-- __Recording API Requests__
--
-- AWS Control Tower supports AWS CloudTrail, a service that records AWS
-- API calls for your AWS account and delivers log files to an Amazon S3
-- bucket. By using information collected by CloudTrail, you can determine
-- which requests the AWS Control Tower service received, who made the
-- request and when, and so on. For more about AWS Control Tower and its
-- support for CloudTrail, see
-- <https://docs.aws.amazon.com/controltower/latest/userguide/logging-using-cloudtrail.html Logging AWS Control Tower Actions with AWS CloudTrail>
-- in the AWS Control Tower User Guide. To learn more about CloudTrail,
-- including how to turn it on and find your log files, see the AWS
-- CloudTrail User Guide.
module Amazonka.ControlTower
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DisableControl
    DisableControl (DisableControl'),
    newDisableControl,
    DisableControlResponse (DisableControlResponse'),
    newDisableControlResponse,

    -- ** EnableControl
    EnableControl (EnableControl'),
    newEnableControl,
    EnableControlResponse (EnableControlResponse'),
    newEnableControlResponse,

    -- ** GetControlOperation
    GetControlOperation (GetControlOperation'),
    newGetControlOperation,
    GetControlOperationResponse (GetControlOperationResponse'),
    newGetControlOperationResponse,

    -- ** ListEnabledControls (Paginated)
    ListEnabledControls (ListEnabledControls'),
    newListEnabledControls,
    ListEnabledControlsResponse (ListEnabledControlsResponse'),
    newListEnabledControlsResponse,

    -- * Types

    -- ** ControlOperationStatus
    ControlOperationStatus (..),

    -- ** ControlOperationType
    ControlOperationType (..),

    -- ** ControlOperation
    ControlOperation (ControlOperation'),
    newControlOperation,

    -- ** EnabledControlSummary
    EnabledControlSummary (EnabledControlSummary'),
    newEnabledControlSummary,
  )
where

import Amazonka.ControlTower.DisableControl
import Amazonka.ControlTower.EnableControl
import Amazonka.ControlTower.GetControlOperation
import Amazonka.ControlTower.Lens
import Amazonka.ControlTower.ListEnabledControls
import Amazonka.ControlTower.Types
import Amazonka.ControlTower.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ControlTower'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
