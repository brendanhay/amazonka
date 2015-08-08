{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pending
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/actions-on-domain-registrations.html AWS API Reference>
module Network.AWS.Route53Domains
    (
    -- * Service Description
      Route53Domains

    -- * Error Matchers
    -- $errors
    , _InvalidInput
    , _OperationLimitExceeded
    , _DomainLimitExceeded
    , _UnsupportedTLD
    , _TLDRulesViolation
    , _DuplicateRequest

    -- * Operations
    -- $operations

    -- ** ListOperations (Paginated)
    , module Network.AWS.Route53Domains.ListOperations
    -- $pager

    -- ** GetDomainDetail
    , module Network.AWS.Route53Domains.GetDomainDetail

    -- ** UpdateDomainContactPrivacy
    , module Network.AWS.Route53Domains.UpdateDomainContactPrivacy

    -- ** GetOperationDetail
    , module Network.AWS.Route53Domains.GetOperationDetail

    -- ** EnableDomainAutoRenew
    , module Network.AWS.Route53Domains.EnableDomainAutoRenew

    -- ** DisableDomainAutoRenew
    , module Network.AWS.Route53Domains.DisableDomainAutoRenew

    -- ** UpdateDomainContact
    , module Network.AWS.Route53Domains.UpdateDomainContact

    -- ** EnableDomainTransferLock
    , module Network.AWS.Route53Domains.EnableDomainTransferLock

    -- ** RegisterDomain
    , module Network.AWS.Route53Domains.RegisterDomain

    -- ** DisableDomainTransferLock
    , module Network.AWS.Route53Domains.DisableDomainTransferLock

    -- ** CheckDomainAvailability
    , module Network.AWS.Route53Domains.CheckDomainAvailability

    -- ** ListTagsForDomain
    , module Network.AWS.Route53Domains.ListTagsForDomain

    -- ** UpdateDomainNameservers
    , module Network.AWS.Route53Domains.UpdateDomainNameservers

    -- ** RetrieveDomainAuthCode
    , module Network.AWS.Route53Domains.RetrieveDomainAuthCode

    -- ** TransferDomain
    , module Network.AWS.Route53Domains.TransferDomain

    -- ** DeleteTagsForDomain
    , module Network.AWS.Route53Domains.DeleteTagsForDomain

    -- ** UpdateTagsForDomain
    , module Network.AWS.Route53Domains.UpdateTagsForDomain

    -- ** ListDomains (Paginated)
    , module Network.AWS.Route53Domains.ListDomains
    -- $pager

    -- * Types

    -- ** ContactType
    , ContactType (..)

    -- ** CountryCode
    , CountryCode (..)

    -- ** DomainAvailability
    , DomainAvailability (..)

    -- ** ExtraParamName
    , ExtraParamName (..)

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** OperationType
    , OperationType (..)

    -- ** ContactDetail
    , ContactDetail
    , contactDetail
    , cdOrganizationName
    , cdEmail
    , cdFax
    , cdState
    , cdLastName
    , cdExtraParams
    , cdZipCode
    , cdAddressLine1
    , cdCity
    , cdPhoneNumber
    , cdAddressLine2
    , cdFirstName
    , cdCountryCode
    , cdContactType

    -- ** DomainSummary
    , DomainSummary
    , domainSummary
    , dsExpiry
    , dsTransferLock
    , dsAutoRenew
    , dsDomainName

    -- ** ExtraParam
    , ExtraParam
    , extraParam
    , epName
    , epValue

    -- ** Nameserver
    , Nameserver
    , nameserver
    , nGlueIPs
    , nName

    -- ** OperationSummary
    , OperationSummary
    , operationSummary
    , osOperationId
    , osStatus
    , osType
    , osSubmittedDate

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import           Network.AWS.Route53Domains.CheckDomainAvailability
import           Network.AWS.Route53Domains.DeleteTagsForDomain
import           Network.AWS.Route53Domains.DisableDomainAutoRenew
import           Network.AWS.Route53Domains.DisableDomainTransferLock
import           Network.AWS.Route53Domains.EnableDomainAutoRenew
import           Network.AWS.Route53Domains.EnableDomainTransferLock
import           Network.AWS.Route53Domains.GetDomainDetail
import           Network.AWS.Route53Domains.GetOperationDetail
import           Network.AWS.Route53Domains.ListDomains
import           Network.AWS.Route53Domains.ListOperations
import           Network.AWS.Route53Domains.ListTagsForDomain
import           Network.AWS.Route53Domains.RegisterDomain
import           Network.AWS.Route53Domains.RetrieveDomainAuthCode
import           Network.AWS.Route53Domains.TransferDomain
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.UpdateDomainContact
import           Network.AWS.Route53Domains.UpdateDomainContactPrivacy
import           Network.AWS.Route53Domains.UpdateDomainNameservers
import           Network.AWS.Route53Domains.UpdateTagsForDomain
import           Network.AWS.Route53Domains.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Route53Domains'.
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

{- $pager
This operation can return paginated results.
-}
