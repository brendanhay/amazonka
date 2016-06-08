{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Certificate Manager
--
-- Welcome to the AWS Certificate Manager (ACM) Command Reference. This guide provides descriptions, syntax, and usage examples for each ACM command. You can use AWS Certificate Manager to request ACM Certificates for your AWS-based websites and applications. For general information about using ACM and for more information about using the console, see the <http://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager User Guide>. For more information about using the ACM API, see the <http://docs.aws.amazon.com/acm/latest/APIReference/Welcome.html AWS Certificate Manager API Reference>.
module Network.AWS.CertificateManager
    (
    -- * Service Configuration
      certificateManager

    -- * Errors
    -- $errors

    -- ** InvalidTagException
    , _InvalidTagException

    -- ** InvalidDomainValidationOptionsException
    , _InvalidDomainValidationOptionsException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** RequestInProgressException
    , _RequestInProgressException

    -- ** InvalidARNException
    , _InvalidARNException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidStateException
    , _InvalidStateException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ResendValidationEmail
    , module Network.AWS.CertificateManager.ResendValidationEmail

    -- ** ListTagsForCertificate
    , module Network.AWS.CertificateManager.ListTagsForCertificate

    -- ** GetCertificate
    , module Network.AWS.CertificateManager.GetCertificate

    -- ** AddTagsToCertificate
    , module Network.AWS.CertificateManager.AddTagsToCertificate

    -- ** RequestCertificate
    , module Network.AWS.CertificateManager.RequestCertificate

    -- ** ListCertificates
    , module Network.AWS.CertificateManager.ListCertificates

    -- ** DeleteCertificate
    , module Network.AWS.CertificateManager.DeleteCertificate

    -- ** RemoveTagsFromCertificate
    , module Network.AWS.CertificateManager.RemoveTagsFromCertificate

    -- ** DescribeCertificate
    , module Network.AWS.CertificateManager.DescribeCertificate

    -- * Types

    -- ** CertificateStatus
    , CertificateStatus (..)

    -- ** KeyAlgorithm
    , KeyAlgorithm (..)

    -- ** RevocationReason
    , RevocationReason (..)

    -- ** CertificateDetail
    , CertificateDetail
    , certificateDetail
    , cdSubject
    , cdStatus
    , cdSubjectAlternativeNames
    , cdInUseBy
    , cdCreatedAt
    , cdCertificateARN
    , cdSerial
    , cdRevokedAt
    , cdNotBefore
    , cdRevocationReason
    , cdDomainName
    , cdKeyAlgorithm
    , cdIssuedAt
    , cdSignatureAlgorithm
    , cdDomainValidationOptions
    , cdIssuer
    , cdNotAfter

    -- ** CertificateSummary
    , CertificateSummary
    , certificateSummary
    , csCertificateARN
    , csDomainName

    -- ** DomainValidation
    , DomainValidation
    , domainValidation
    , dvValidationEmails
    , dvValidationDomain
    , dvDomainName

    -- ** DomainValidationOption
    , DomainValidationOption
    , domainValidationOption
    , dvoDomainName
    , dvoValidationDomain

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import           Network.AWS.CertificateManager.AddTagsToCertificate
import           Network.AWS.CertificateManager.DeleteCertificate
import           Network.AWS.CertificateManager.DescribeCertificate
import           Network.AWS.CertificateManager.GetCertificate
import           Network.AWS.CertificateManager.ListCertificates
import           Network.AWS.CertificateManager.ListTagsForCertificate
import           Network.AWS.CertificateManager.RemoveTagsFromCertificate
import           Network.AWS.CertificateManager.RequestCertificate
import           Network.AWS.CertificateManager.ResendValidationEmail
import           Network.AWS.CertificateManager.Types
import           Network.AWS.CertificateManager.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CertificateManager'.
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
