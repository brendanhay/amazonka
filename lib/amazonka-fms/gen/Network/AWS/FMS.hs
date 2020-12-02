{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Firewall Manager__
--
-- This is the /AWS Firewall Manager API Reference/ . This guide is for developers who need detailed information about the AWS Firewall Manager API actions, data types, and errors. For detailed information about AWS Firewall Manager features, see the <http://docs.aws.amazon.com/waf/latest/developerguide/fms-chapter.html AWS Firewall Manager Developer Guide> .
--
module Network.AWS.FMS
    (
    -- * Service Configuration
      fms

    -- * Errors
    -- $errors

    -- ** InternalErrorException
    , _InternalErrorException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListPolicies
    , module Network.AWS.FMS.ListPolicies

    -- ** GetComplianceDetail
    , module Network.AWS.FMS.GetComplianceDetail

    -- ** GetNotificationChannel
    , module Network.AWS.FMS.GetNotificationChannel

    -- ** GetAdminAccount
    , module Network.AWS.FMS.GetAdminAccount

    -- ** ListComplianceStatus
    , module Network.AWS.FMS.ListComplianceStatus

    -- ** PutPolicy
    , module Network.AWS.FMS.PutPolicy

    -- ** DeletePolicy
    , module Network.AWS.FMS.DeletePolicy

    -- ** DisassociateAdminAccount
    , module Network.AWS.FMS.DisassociateAdminAccount

    -- ** PutNotificationChannel
    , module Network.AWS.FMS.PutNotificationChannel

    -- ** DeleteNotificationChannel
    , module Network.AWS.FMS.DeleteNotificationChannel

    -- ** AssociateAdminAccount
    , module Network.AWS.FMS.AssociateAdminAccount

    -- ** GetPolicy
    , module Network.AWS.FMS.GetPolicy

    -- * Types

    -- ** PolicyComplianceStatusType
    , PolicyComplianceStatusType (..)

    -- ** SecurityServiceType
    , SecurityServiceType (..)

    -- ** ViolationReason
    , ViolationReason (..)

    -- ** ComplianceViolator
    , ComplianceViolator
    , complianceViolator
    , cvResourceId
    , cvResourceType
    , cvViolationReason

    -- ** EvaluationResult
    , EvaluationResult
    , evaluationResult
    , erViolatorCount
    , erComplianceStatus
    , erEvaluationLimitExceeded

    -- ** Policy
    , Policy
    , policy
    , pPolicyId
    , pResourceTags
    , pPolicyUpdateToken
    , pPolicyName
    , pSecurityServicePolicyData
    , pResourceType
    , pExcludeResourceTags
    , pRemediationEnabled

    -- ** PolicyComplianceDetail
    , PolicyComplianceDetail
    , policyComplianceDetail
    , pcdExpiredAt
    , pcdPolicyId
    , pcdViolators
    , pcdEvaluationLimitExceeded
    , pcdPolicyOwner
    , pcdMemberAccount

    -- ** PolicyComplianceStatus
    , PolicyComplianceStatus
    , policyComplianceStatus
    , pcsEvaluationResults
    , pcsLastUpdated
    , pcsPolicyName
    , pcsPolicyId
    , pcsPolicyOwner
    , pcsMemberAccount

    -- ** PolicySummary
    , PolicySummary
    , policySummary
    , psPolicyName
    , psRemediationEnabled
    , psResourceType
    , psPolicyId
    , psPolicyARN
    , psSecurityServiceType

    -- ** ResourceTag
    , ResourceTag
    , resourceTag
    , rtValue
    , rtKey

    -- ** SecurityServicePolicyData
    , SecurityServicePolicyData
    , securityServicePolicyData
    , sspdManagedServiceData
    , sspdType
    ) where

import Network.AWS.FMS.AssociateAdminAccount
import Network.AWS.FMS.DeleteNotificationChannel
import Network.AWS.FMS.DeletePolicy
import Network.AWS.FMS.DisassociateAdminAccount
import Network.AWS.FMS.GetAdminAccount
import Network.AWS.FMS.GetComplianceDetail
import Network.AWS.FMS.GetNotificationChannel
import Network.AWS.FMS.GetPolicy
import Network.AWS.FMS.ListComplianceStatus
import Network.AWS.FMS.ListPolicies
import Network.AWS.FMS.PutNotificationChannel
import Network.AWS.FMS.PutPolicy
import Network.AWS.FMS.Types
import Network.AWS.FMS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'FMS'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
