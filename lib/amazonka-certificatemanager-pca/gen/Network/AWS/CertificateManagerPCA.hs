{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use the ACM PCA API to create a private certificate authority (CA). You must first call the 'CreateCertificateAuthority' function. If successful, the function returns an Amazon Resource Name (ARN) for your private CA. Use this ARN as input to the 'GetCertificateAuthorityCsr' function to retrieve the certificate signing request (CSR) for your private CA certificate. Sign the CSR using the root or an intermediate CA in your on-premises PKI hierarchy, and call the 'ImportCertificateAuthorityCertificate' to import your signed private CA certificate into ACM PCA.
--
--
-- Use your private CA to issue and revoke certificates. These are private certificates that identify and secure client computers, servers, applications, services, devices, and users over SSLS/TLS connections within your organization. Call the 'IssueCertificate' function to issue a certificate. Call the 'RevokeCertificate' function to revoke a certificate.
--
-- Your private CA can optionally create a certificate revocation list (CRL) to track the certificates you revoke. To create a CRL, you must specify a 'RevocationConfiguration' object when you call the 'CreateCertificateAuthority' function. ACM PCA writes the CRL to an S3 bucket that you specify. You must specify a bucket policy that grants ACM PCA write permission.
--
-- You can also call the 'CreateCertificateAuthorityAuditReport' to create an optional audit report that lists every time the CA private key is used. The private key is used for signing when the __IssueCertificate__ or __RevokeCertificate__ function is called.
--
module Network.AWS.CertificateManagerPCA
    (
    -- * Service Configuration
      certificateManagerPCA

    -- * Errors
    -- $errors

    -- ** InvalidTagException
    , _InvalidTagException

    -- ** MalformedCSRException
    , _MalformedCSRException

    -- ** RequestAlreadyProcessedException
    , _RequestAlreadyProcessedException

    -- ** MalformedCertificateException
    , _MalformedCertificateException

    -- ** RequestFailedException
    , _RequestFailedException

    -- ** CertificateMismatchException
    , _CertificateMismatchException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** InvalidArgsException
    , _InvalidArgsException

    -- ** RequestInProgressException
    , _RequestInProgressException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InvalidARNException
    , _InvalidARNException

    -- ** InvalidPolicyException
    , _InvalidPolicyException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidStateException
    , _InvalidStateException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ImportCertificateAuthorityCertificate
    , module Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate

    -- ** DescribeCertificateAuthorityAuditReport
    , module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport

    -- ** RevokeCertificate
    , module Network.AWS.CertificateManagerPCA.RevokeCertificate

    -- ** UpdateCertificateAuthority
    , module Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority

    -- ** DeleteCertificateAuthority
    , module Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority

    -- ** GetCertificateAuthorityCSR
    , module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR

    -- ** CreateCertificateAuthority
    , module Network.AWS.CertificateManagerPCA.CreateCertificateAuthority

    -- ** ListCertificateAuthorities
    , module Network.AWS.CertificateManagerPCA.ListCertificateAuthorities

    -- ** GetCertificate
    , module Network.AWS.CertificateManagerPCA.GetCertificate

    -- ** TagCertificateAuthority
    , module Network.AWS.CertificateManagerPCA.TagCertificateAuthority

    -- ** DescribeCertificateAuthority
    , module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority

    -- ** IssueCertificate
    , module Network.AWS.CertificateManagerPCA.IssueCertificate

    -- ** GetCertificateAuthorityCertificate
    , module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate

    -- ** UntagCertificateAuthority
    , module Network.AWS.CertificateManagerPCA.UntagCertificateAuthority

    -- ** CreateCertificateAuthorityAuditReport
    , module Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport

    -- ** ListTags
    , module Network.AWS.CertificateManagerPCA.ListTags

    -- * Types

    -- ** AuditReportResponseFormat
    , AuditReportResponseFormat (..)

    -- ** AuditReportStatus
    , AuditReportStatus (..)

    -- ** CertificateAuthorityStatus
    , CertificateAuthorityStatus (..)

    -- ** CertificateAuthorityType
    , CertificateAuthorityType (..)

    -- ** FailureReason
    , FailureReason (..)

    -- ** KeyAlgorithm
    , KeyAlgorithm (..)

    -- ** RevocationReason
    , RevocationReason (..)

    -- ** SigningAlgorithm
    , SigningAlgorithm (..)

    -- ** ValidityPeriodType
    , ValidityPeriodType (..)

    -- ** ASN1Subject
    , ASN1Subject
    , asn1Subject
    , asGivenName
    , asState
    , asCommonName
    , asOrganizationalUnit
    , asCountry
    , asGenerationQualifier
    , asLocality
    , asPseudonym
    , asInitials
    , asTitle
    , asOrganization
    , asSerialNumber
    , asSurname
    , asDistinguishedNameQualifier

    -- ** CertificateAuthority
    , CertificateAuthority
    , certificateAuthority
    , caStatus
    , caFailureReason
    , caCertificateAuthorityConfiguration
    , caARN
    , caCreatedAt
    , caSerial
    , caNotBefore
    , caType
    , caRevocationConfiguration
    , caLastStateChangeAt
    , caNotAfter

    -- ** CertificateAuthorityConfiguration
    , CertificateAuthorityConfiguration
    , certificateAuthorityConfiguration
    , cacKeyAlgorithm
    , cacSigningAlgorithm
    , cacSubject

    -- ** CrlConfiguration
    , CrlConfiguration
    , crlConfiguration
    , ccCustomCname
    , ccExpirationInDays
    , ccS3BucketName
    , ccEnabled

    -- ** RevocationConfiguration
    , RevocationConfiguration
    , revocationConfiguration
    , rcCrlConfiguration

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** Validity
    , Validity
    , validity
    , vValue
    , vType
    ) where

import Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
import Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.GetCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR
import Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
import Network.AWS.CertificateManagerPCA.IssueCertificate
import Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
import Network.AWS.CertificateManagerPCA.ListTags
import Network.AWS.CertificateManagerPCA.RevokeCertificate
import Network.AWS.CertificateManagerPCA.TagCertificateAuthority
import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
import Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
import Network.AWS.CertificateManagerPCA.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CertificateManagerPCA'.
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
