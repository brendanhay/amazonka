{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.PaymentCryptography
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-09-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- You use the Amazon Web Services Payment Cryptography Control Plane to
-- manage the encryption keys you use for payment-related cryptographic
-- operations. You can create, import, export, share, manage, and delete
-- keys. You can also manage Identity and Access Management (IAM) policies
-- for keys. For more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/security-iam.html Identity and access management>
-- in the /Amazon Web Services Payment Cryptography User Guide./
--
-- To use encryption keys for payment-related transaction processing and
-- associated cryptographic operations, you use the
-- <https://docs.aws.amazon.com/payment-cryptography/latest/DataAPIReference/Welcome.html Amazon Web Services Payment Cryptography Data Plane>.
-- You can encrypt, decrypt, generate, verify, and translate
-- payment-related cryptographic operations.
--
-- All Amazon Web Services Payment Cryptography API calls must be signed
-- and transmitted using Transport Layer Security (TLS). We recommend you
-- always use the latest supported TLS version for logging API requests.
--
-- Amazon Web Services Payment Cryptography supports CloudTrail, a service
-- that logs Amazon Web Services API calls and related events for your
-- Amazon Web Services account and delivers them to an Amazon S3 bucket
-- that you specify. By using the information collected by CloudTrail, you
-- can determine what requests were made to Amazon Web Services Payment
-- Cryptography, who made the request, when it was made, and so on. If you
-- don\'t conﬁgure a trail, you can still view the most recent events in
-- the CloudTrail console. For more information, see the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/ CloudTrail User Guide>.
module Amazonka.PaymentCryptography
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

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** CreateKey
    CreateKey (CreateKey'),
    newCreateKey,
    CreateKeyResponse (CreateKeyResponse'),
    newCreateKeyResponse,

    -- ** DeleteAlias
    DeleteAlias (DeleteAlias'),
    newDeleteAlias,
    DeleteAliasResponse (DeleteAliasResponse'),
    newDeleteAliasResponse,

    -- ** DeleteKey
    DeleteKey (DeleteKey'),
    newDeleteKey,
    DeleteKeyResponse (DeleteKeyResponse'),
    newDeleteKeyResponse,

    -- ** ExportKey
    ExportKey (ExportKey'),
    newExportKey,
    ExportKeyResponse (ExportKeyResponse'),
    newExportKeyResponse,

    -- ** GetAlias
    GetAlias (GetAlias'),
    newGetAlias,
    GetAliasResponse (GetAliasResponse'),
    newGetAliasResponse,

    -- ** GetKey
    GetKey (GetKey'),
    newGetKey,
    GetKeyResponse (GetKeyResponse'),
    newGetKeyResponse,

    -- ** GetParametersForExport
    GetParametersForExport (GetParametersForExport'),
    newGetParametersForExport,
    GetParametersForExportResponse (GetParametersForExportResponse'),
    newGetParametersForExportResponse,

    -- ** GetParametersForImport
    GetParametersForImport (GetParametersForImport'),
    newGetParametersForImport,
    GetParametersForImportResponse (GetParametersForImportResponse'),
    newGetParametersForImportResponse,

    -- ** GetPublicKeyCertificate
    GetPublicKeyCertificate (GetPublicKeyCertificate'),
    newGetPublicKeyCertificate,
    GetPublicKeyCertificateResponse (GetPublicKeyCertificateResponse'),
    newGetPublicKeyCertificateResponse,

    -- ** ImportKey
    ImportKey (ImportKey'),
    newImportKey,
    ImportKeyResponse (ImportKeyResponse'),
    newImportKeyResponse,

    -- ** ListAliases (Paginated)
    ListAliases (ListAliases'),
    newListAliases,
    ListAliasesResponse (ListAliasesResponse'),
    newListAliasesResponse,

    -- ** ListKeys (Paginated)
    ListKeys (ListKeys'),
    newListKeys,
    ListKeysResponse (ListKeysResponse'),
    newListKeysResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RestoreKey
    RestoreKey (RestoreKey'),
    newRestoreKey,
    RestoreKeyResponse (RestoreKeyResponse'),
    newRestoreKeyResponse,

    -- ** StartKeyUsage
    StartKeyUsage (StartKeyUsage'),
    newStartKeyUsage,
    StartKeyUsageResponse (StartKeyUsageResponse'),
    newStartKeyUsageResponse,

    -- ** StopKeyUsage
    StopKeyUsage (StopKeyUsage'),
    newStopKeyUsage,
    StopKeyUsageResponse (StopKeyUsageResponse'),
    newStopKeyUsageResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAlias
    UpdateAlias (UpdateAlias'),
    newUpdateAlias,
    UpdateAliasResponse (UpdateAliasResponse'),
    newUpdateAliasResponse,

    -- * Types

    -- ** KeyAlgorithm
    KeyAlgorithm (..),

    -- ** KeyCheckValueAlgorithm
    KeyCheckValueAlgorithm (..),

    -- ** KeyClass
    KeyClass (..),

    -- ** KeyMaterialType
    KeyMaterialType (..),

    -- ** KeyOrigin
    KeyOrigin (..),

    -- ** KeyState
    KeyState (..),

    -- ** KeyUsage
    KeyUsage (..),

    -- ** Tr34KeyBlockFormat
    Tr34KeyBlockFormat (..),

    -- ** WrappedKeyMaterialFormat
    WrappedKeyMaterialFormat (..),

    -- ** Alias
    Alias (Alias'),
    newAlias,

    -- ** ExportKeyMaterial
    ExportKeyMaterial (ExportKeyMaterial'),
    newExportKeyMaterial,

    -- ** ExportTr31KeyBlock
    ExportTr31KeyBlock (ExportTr31KeyBlock'),
    newExportTr31KeyBlock,

    -- ** ExportTr34KeyBlock
    ExportTr34KeyBlock (ExportTr34KeyBlock'),
    newExportTr34KeyBlock,

    -- ** ImportKeyMaterial
    ImportKeyMaterial (ImportKeyMaterial'),
    newImportKeyMaterial,

    -- ** ImportTr31KeyBlock
    ImportTr31KeyBlock (ImportTr31KeyBlock'),
    newImportTr31KeyBlock,

    -- ** ImportTr34KeyBlock
    ImportTr34KeyBlock (ImportTr34KeyBlock'),
    newImportTr34KeyBlock,

    -- ** Key
    Key (Key'),
    newKey,

    -- ** KeyAttributes
    KeyAttributes (KeyAttributes'),
    newKeyAttributes,

    -- ** KeyModesOfUse
    KeyModesOfUse (KeyModesOfUse'),
    newKeyModesOfUse,

    -- ** KeySummary
    KeySummary (KeySummary'),
    newKeySummary,

    -- ** RootCertificatePublicKey
    RootCertificatePublicKey (RootCertificatePublicKey'),
    newRootCertificatePublicKey,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TrustedCertificatePublicKey
    TrustedCertificatePublicKey (TrustedCertificatePublicKey'),
    newTrustedCertificatePublicKey,

    -- ** WrappedKey
    WrappedKey (WrappedKey'),
    newWrappedKey,
  )
where

import Amazonka.PaymentCryptography.CreateAlias
import Amazonka.PaymentCryptography.CreateKey
import Amazonka.PaymentCryptography.DeleteAlias
import Amazonka.PaymentCryptography.DeleteKey
import Amazonka.PaymentCryptography.ExportKey
import Amazonka.PaymentCryptography.GetAlias
import Amazonka.PaymentCryptography.GetKey
import Amazonka.PaymentCryptography.GetParametersForExport
import Amazonka.PaymentCryptography.GetParametersForImport
import Amazonka.PaymentCryptography.GetPublicKeyCertificate
import Amazonka.PaymentCryptography.ImportKey
import Amazonka.PaymentCryptography.Lens
import Amazonka.PaymentCryptography.ListAliases
import Amazonka.PaymentCryptography.ListKeys
import Amazonka.PaymentCryptography.ListTagsForResource
import Amazonka.PaymentCryptography.RestoreKey
import Amazonka.PaymentCryptography.StartKeyUsage
import Amazonka.PaymentCryptography.StopKeyUsage
import Amazonka.PaymentCryptography.TagResource
import Amazonka.PaymentCryptography.Types
import Amazonka.PaymentCryptography.UntagResource
import Amazonka.PaymentCryptography.UpdateAlias
import Amazonka.PaymentCryptography.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'PaymentCryptography'.

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
