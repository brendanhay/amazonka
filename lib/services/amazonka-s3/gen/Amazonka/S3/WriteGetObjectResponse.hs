{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3.WriteGetObjectResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Passes transformed objects to a @GetObject@ operation when using Object
-- Lambda access points. For information about Object Lambda access points,
-- see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/transforming-objects.html Transforming objects with Object Lambda access points>
-- in the /Amazon S3 User Guide/.
--
-- This operation supports metadata that can be returned by
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>,
-- in addition to @RequestRoute@, @RequestToken@, @StatusCode@,
-- @ErrorCode@, and @ErrorMessage@. The @GetObject@ response metadata is
-- supported so that the @WriteGetObjectResponse@ caller, typically an
-- Lambda function, can provide the same metadata when it internally
-- invokes @GetObject@. When @WriteGetObjectResponse@ is called by a
-- customer-owned Lambda function, the metadata returned to the end user
-- @GetObject@ call might differ from what Amazon S3 would normally return.
--
-- You can include any number of metadata headers. When including a
-- metadata header, it should be prefaced with @x-amz-meta@. For example,
-- @x-amz-meta-my-custom-header: MyCustomValue@. The primary use case for
-- this is to forward @GetObject@ metadata.
--
-- Amazon Web Services provides some prebuilt Lambda functions that you can
-- use with S3 Object Lambda to detect and redact personally identifiable
-- information (PII) and decompress S3 objects. These Lambda functions are
-- available in the Amazon Web Services Serverless Application Repository,
-- and can be selected through the Amazon Web Services Management Console
-- when you create your Object Lambda access point.
--
-- Example 1: PII Access Control - This Lambda function uses Amazon
-- Comprehend, a natural language processing (NLP) service using machine
-- learning to find insights and relationships in text. It automatically
-- detects personally identifiable information (PII) such as names,
-- addresses, dates, credit card numbers, and social security numbers from
-- documents in your Amazon S3 bucket.
--
-- Example 2: PII Redaction - This Lambda function uses Amazon Comprehend,
-- a natural language processing (NLP) service using machine learning to
-- find insights and relationships in text. It automatically redacts
-- personally identifiable information (PII) such as names, addresses,
-- dates, credit card numbers, and social security numbers from documents
-- in your Amazon S3 bucket.
--
-- Example 3: Decompression - The Lambda function
-- S3ObjectLambdaDecompression, is equipped to decompress objects stored in
-- S3 in one of six compressed file formats including bzip2, gzip, snappy,
-- zlib, zstandard and ZIP.
--
-- For information on how to view and use these functions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/olap-examples.html Using Amazon Web Services built Lambda functions>
-- in the /Amazon S3 User Guide/.
module Amazonka.S3.WriteGetObjectResponse
  ( -- * Creating a Request
    WriteGetObjectResponse (..),
    newWriteGetObjectResponse,

    -- * Request Lenses
    writeGetObjectResponse_requestCharged,
    writeGetObjectResponse_partsCount,
    writeGetObjectResponse_eTag,
    writeGetObjectResponse_versionId,
    writeGetObjectResponse_contentLength,
    writeGetObjectResponse_objectLockMode,
    writeGetObjectResponse_expires,
    writeGetObjectResponse_restore,
    writeGetObjectResponse_expiration,
    writeGetObjectResponse_deleteMarker,
    writeGetObjectResponse_sSECustomerAlgorithm,
    writeGetObjectResponse_tagCount,
    writeGetObjectResponse_missingMeta,
    writeGetObjectResponse_bucketKeyEnabled,
    writeGetObjectResponse_acceptRanges,
    writeGetObjectResponse_storageClass,
    writeGetObjectResponse_sSECustomerKeyMD5,
    writeGetObjectResponse_sSEKMSKeyId,
    writeGetObjectResponse_contentEncoding,
    writeGetObjectResponse_errorCode,
    writeGetObjectResponse_objectLockRetainUntilDate,
    writeGetObjectResponse_metadata,
    writeGetObjectResponse_replicationStatus,
    writeGetObjectResponse_cacheControl,
    writeGetObjectResponse_contentLanguage,
    writeGetObjectResponse_errorMessage,
    writeGetObjectResponse_lastModified,
    writeGetObjectResponse_objectLockLegalHoldStatus,
    writeGetObjectResponse_contentDisposition,
    writeGetObjectResponse_contentRange,
    writeGetObjectResponse_serverSideEncryption,
    writeGetObjectResponse_contentType,
    writeGetObjectResponse_statusCode,
    writeGetObjectResponse_requestRoute,
    writeGetObjectResponse_requestToken,
    writeGetObjectResponse_body,

    -- * Destructuring the Response
    WriteGetObjectResponseResponse (..),
    newWriteGetObjectResponseResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newWriteGetObjectResponse' smart constructor.
data WriteGetObjectResponse = WriteGetObjectResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The count of parts this object has.
    partsCount :: Prelude.Maybe Prelude.Int,
    -- | An opaque identifier assigned by a web server to a specific version of a
    -- resource found at a URL.
    eTag :: Prelude.Maybe ETag,
    -- | An ID used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The size of the content body in bytes.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | Indicates whether an object stored in Amazon S3 has Object Lock enabled.
    -- For more information about S3 Object Lock, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-lock.html Object Lock>.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Prelude.Maybe Core.ISO8601,
    -- | Provides information about object restoration operation and expiration
    -- time of the restored object copy.
    restore :: Prelude.Maybe Prelude.Text,
    -- | If object stored in Amazon S3 expiration is configured (see PUT Bucket
    -- lifecycle) it includes expiry-date and rule-id key-value pairs providing
    -- object expiration information. The value of the rule-id is URL encoded.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether an object stored in Amazon S3 is (@true@) or is not
    -- (@false@) a delete marker.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | Encryption algorithm used if server-side encryption with a
    -- customer-provided encryption key was specified for object stored in
    -- Amazon S3.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The number of tags, if any, on the object.
    tagCount :: Prelude.Maybe Prelude.Int,
    -- | Set to the number of metadata entries not returned in @x-amz-meta@
    -- headers. This can happen if you create metadata using an API like SOAP
    -- that supports more flexible metadata than the REST API. For example,
    -- using SOAP, you can create metadata whose values are not legal HTTP
    -- headers.
    missingMeta :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the object stored in Amazon S3 uses an S3 bucket key
    -- for server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that a range of bytes was specified.
    acceptRanges :: Prelude.Maybe Prelude.Text,
    -- | The class of storage used to store object in Amazon S3.
    storageClass :: Prelude.Maybe StorageClass,
    -- | 128-bit MD5 digest of customer-provided encryption key used in Amazon S3
    -- to encrypt data stored in S3. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/ServerSideEncryptionCustomerKeys.html Protecting data using server-side encryption with customer-provided encryption keys (SSE-C)>.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the ID of the Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) symmetric customer managed key that
    -- was used for stored in Amazon S3 object.
    sSEKMSKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | A string that uniquely identifies an error condition. Returned in the
    -- \<Code> tag of the error XML response for a corresponding @GetObject@
    -- call. Cannot be used with a successful @StatusCode@ header or when the
    -- transformed object is provided in the body. All error codes from S3 are
    -- sentence-cased. Regex value is \"^[A-Z][a-zA-Z]+$\".
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The date and time when Object Lock is configured to expire.
    objectLockRetainUntilDate :: Prelude.Maybe Core.ISO8601,
    -- | A map of metadata to store with the object in S3.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Indicates if request involves bucket that is either a source or
    -- destination in a Replication rule. For more information about S3
    -- Replication, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/replication.html Replication>.
    replicationStatus :: Prelude.Maybe ReplicationStatus,
    -- | Specifies caching behavior along the request\/reply chain.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
    -- | Contains a generic description of the error condition. Returned in the
    -- \<Message> tag of the error XML response for a corresponding @GetObject@
    -- call. Cannot be used with a successful @StatusCode@ header or when the
    -- transformed object is provided in body.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the object was last modified.
    lastModified :: Prelude.Maybe Core.ISO8601,
    -- | Indicates whether an object stored in Amazon S3 has an active legal
    -- hold.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | Specifies presentational information for the object.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The portion of the object returned in the response.
    contentRange :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption algorithm used when storing requested object
    -- in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The integer status code for an HTTP response of a corresponding
    -- @GetObject@ request.
    --
    -- __Status Codes__
    --
    -- -   /200 - OK/
    --
    -- -   /206 - Partial Content/
    --
    -- -   /304 - Not Modified/
    --
    -- -   /400 - Bad Request/
    --
    -- -   /401 - Unauthorized/
    --
    -- -   /403 - Forbidden/
    --
    -- -   /404 - Not Found/
    --
    -- -   /405 - Method Not Allowed/
    --
    -- -   /409 - Conflict/
    --
    -- -   /411 - Length Required/
    --
    -- -   /412 - Precondition Failed/
    --
    -- -   /416 - Range Not Satisfiable/
    --
    -- -   /500 - Internal Server Error/
    --
    -- -   /503 - Service Unavailable/
    statusCode :: Prelude.Maybe Prelude.Int,
    -- | Route prefix to the HTTP URL generated.
    requestRoute :: Prelude.Text,
    -- | A single use encrypted token that maps @WriteGetObjectResponse@ to the
    -- end user @GetObject@ request.
    requestToken :: Prelude.Text,
    -- | The object data.
    body :: Core.RequestBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteGetObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'writeGetObjectResponse_requestCharged' - Undocumented member.
--
-- 'partsCount', 'writeGetObjectResponse_partsCount' - The count of parts this object has.
--
-- 'eTag', 'writeGetObjectResponse_eTag' - An opaque identifier assigned by a web server to a specific version of a
-- resource found at a URL.
--
-- 'versionId', 'writeGetObjectResponse_versionId' - An ID used to reference a specific version of the object.
--
-- 'contentLength', 'writeGetObjectResponse_contentLength' - The size of the content body in bytes.
--
-- 'objectLockMode', 'writeGetObjectResponse_objectLockMode' - Indicates whether an object stored in Amazon S3 has Object Lock enabled.
-- For more information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-lock.html Object Lock>.
--
-- 'expires', 'writeGetObjectResponse_expires' - The date and time at which the object is no longer cacheable.
--
-- 'restore', 'writeGetObjectResponse_restore' - Provides information about object restoration operation and expiration
-- time of the restored object copy.
--
-- 'expiration', 'writeGetObjectResponse_expiration' - If object stored in Amazon S3 expiration is configured (see PUT Bucket
-- lifecycle) it includes expiry-date and rule-id key-value pairs providing
-- object expiration information. The value of the rule-id is URL encoded.
--
-- 'deleteMarker', 'writeGetObjectResponse_deleteMarker' - Specifies whether an object stored in Amazon S3 is (@true@) or is not
-- (@false@) a delete marker.
--
-- 'sSECustomerAlgorithm', 'writeGetObjectResponse_sSECustomerAlgorithm' - Encryption algorithm used if server-side encryption with a
-- customer-provided encryption key was specified for object stored in
-- Amazon S3.
--
-- 'tagCount', 'writeGetObjectResponse_tagCount' - The number of tags, if any, on the object.
--
-- 'missingMeta', 'writeGetObjectResponse_missingMeta' - Set to the number of metadata entries not returned in @x-amz-meta@
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
--
-- 'bucketKeyEnabled', 'writeGetObjectResponse_bucketKeyEnabled' - Indicates whether the object stored in Amazon S3 uses an S3 bucket key
-- for server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'acceptRanges', 'writeGetObjectResponse_acceptRanges' - Indicates that a range of bytes was specified.
--
-- 'storageClass', 'writeGetObjectResponse_storageClass' - The class of storage used to store object in Amazon S3.
--
-- 'sSECustomerKeyMD5', 'writeGetObjectResponse_sSECustomerKeyMD5' - 128-bit MD5 digest of customer-provided encryption key used in Amazon S3
-- to encrypt data stored in S3. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/ServerSideEncryptionCustomerKeys.html Protecting data using server-side encryption with customer-provided encryption keys (SSE-C)>.
--
-- 'sSEKMSKeyId', 'writeGetObjectResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for stored in Amazon S3 object.
--
-- 'contentEncoding', 'writeGetObjectResponse_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'errorCode', 'writeGetObjectResponse_errorCode' - A string that uniquely identifies an error condition. Returned in the
-- \<Code> tag of the error XML response for a corresponding @GetObject@
-- call. Cannot be used with a successful @StatusCode@ header or when the
-- transformed object is provided in the body. All error codes from S3 are
-- sentence-cased. Regex value is \"^[A-Z][a-zA-Z]+$\".
--
-- 'objectLockRetainUntilDate', 'writeGetObjectResponse_objectLockRetainUntilDate' - The date and time when Object Lock is configured to expire.
--
-- 'metadata', 'writeGetObjectResponse_metadata' - A map of metadata to store with the object in S3.
--
-- 'replicationStatus', 'writeGetObjectResponse_replicationStatus' - Indicates if request involves bucket that is either a source or
-- destination in a Replication rule. For more information about S3
-- Replication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/replication.html Replication>.
--
-- 'cacheControl', 'writeGetObjectResponse_cacheControl' - Specifies caching behavior along the request\/reply chain.
--
-- 'contentLanguage', 'writeGetObjectResponse_contentLanguage' - The language the content is in.
--
-- 'errorMessage', 'writeGetObjectResponse_errorMessage' - Contains a generic description of the error condition. Returned in the
-- \<Message> tag of the error XML response for a corresponding @GetObject@
-- call. Cannot be used with a successful @StatusCode@ header or when the
-- transformed object is provided in body.
--
-- 'lastModified', 'writeGetObjectResponse_lastModified' - The date and time that the object was last modified.
--
-- 'objectLockLegalHoldStatus', 'writeGetObjectResponse_objectLockLegalHoldStatus' - Indicates whether an object stored in Amazon S3 has an active legal
-- hold.
--
-- 'contentDisposition', 'writeGetObjectResponse_contentDisposition' - Specifies presentational information for the object.
--
-- 'contentRange', 'writeGetObjectResponse_contentRange' - The portion of the object returned in the response.
--
-- 'serverSideEncryption', 'writeGetObjectResponse_serverSideEncryption' - The server-side encryption algorithm used when storing requested object
-- in Amazon S3 (for example, AES256, aws:kms).
--
-- 'contentType', 'writeGetObjectResponse_contentType' - A standard MIME type describing the format of the object data.
--
-- 'statusCode', 'writeGetObjectResponse_statusCode' - The integer status code for an HTTP response of a corresponding
-- @GetObject@ request.
--
-- __Status Codes__
--
-- -   /200 - OK/
--
-- -   /206 - Partial Content/
--
-- -   /304 - Not Modified/
--
-- -   /400 - Bad Request/
--
-- -   /401 - Unauthorized/
--
-- -   /403 - Forbidden/
--
-- -   /404 - Not Found/
--
-- -   /405 - Method Not Allowed/
--
-- -   /409 - Conflict/
--
-- -   /411 - Length Required/
--
-- -   /412 - Precondition Failed/
--
-- -   /416 - Range Not Satisfiable/
--
-- -   /500 - Internal Server Error/
--
-- -   /503 - Service Unavailable/
--
-- 'requestRoute', 'writeGetObjectResponse_requestRoute' - Route prefix to the HTTP URL generated.
--
-- 'requestToken', 'writeGetObjectResponse_requestToken' - A single use encrypted token that maps @WriteGetObjectResponse@ to the
-- end user @GetObject@ request.
--
-- 'body', 'writeGetObjectResponse_body' - The object data.
newWriteGetObjectResponse ::
  -- | 'requestRoute'
  Prelude.Text ->
  -- | 'requestToken'
  Prelude.Text ->
  -- | 'body'
  Core.RequestBody ->
  WriteGetObjectResponse
newWriteGetObjectResponse
  pRequestRoute_
  pRequestToken_
  pBody_ =
    WriteGetObjectResponse'
      { requestCharged =
          Prelude.Nothing,
        partsCount = Prelude.Nothing,
        eTag = Prelude.Nothing,
        versionId = Prelude.Nothing,
        contentLength = Prelude.Nothing,
        objectLockMode = Prelude.Nothing,
        expires = Prelude.Nothing,
        restore = Prelude.Nothing,
        expiration = Prelude.Nothing,
        deleteMarker = Prelude.Nothing,
        sSECustomerAlgorithm = Prelude.Nothing,
        tagCount = Prelude.Nothing,
        missingMeta = Prelude.Nothing,
        bucketKeyEnabled = Prelude.Nothing,
        acceptRanges = Prelude.Nothing,
        storageClass = Prelude.Nothing,
        sSECustomerKeyMD5 = Prelude.Nothing,
        sSEKMSKeyId = Prelude.Nothing,
        contentEncoding = Prelude.Nothing,
        errorCode = Prelude.Nothing,
        objectLockRetainUntilDate = Prelude.Nothing,
        metadata = Prelude.mempty,
        replicationStatus = Prelude.Nothing,
        cacheControl = Prelude.Nothing,
        contentLanguage = Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        lastModified = Prelude.Nothing,
        objectLockLegalHoldStatus = Prelude.Nothing,
        contentDisposition = Prelude.Nothing,
        contentRange = Prelude.Nothing,
        serverSideEncryption = Prelude.Nothing,
        contentType = Prelude.Nothing,
        statusCode = Prelude.Nothing,
        requestRoute = pRequestRoute_,
        requestToken = pRequestToken_,
        body = pBody_
      }

-- | Undocumented member.
writeGetObjectResponse_requestCharged :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe RequestCharged)
writeGetObjectResponse_requestCharged = Lens.lens (\WriteGetObjectResponse' {requestCharged} -> requestCharged) (\s@WriteGetObjectResponse' {} a -> s {requestCharged = a} :: WriteGetObjectResponse)

-- | The count of parts this object has.
writeGetObjectResponse_partsCount :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Int)
writeGetObjectResponse_partsCount = Lens.lens (\WriteGetObjectResponse' {partsCount} -> partsCount) (\s@WriteGetObjectResponse' {} a -> s {partsCount = a} :: WriteGetObjectResponse)

-- | An opaque identifier assigned by a web server to a specific version of a
-- resource found at a URL.
writeGetObjectResponse_eTag :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe ETag)
writeGetObjectResponse_eTag = Lens.lens (\WriteGetObjectResponse' {eTag} -> eTag) (\s@WriteGetObjectResponse' {} a -> s {eTag = a} :: WriteGetObjectResponse)

-- | An ID used to reference a specific version of the object.
writeGetObjectResponse_versionId :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe ObjectVersionId)
writeGetObjectResponse_versionId = Lens.lens (\WriteGetObjectResponse' {versionId} -> versionId) (\s@WriteGetObjectResponse' {} a -> s {versionId = a} :: WriteGetObjectResponse)

-- | The size of the content body in bytes.
writeGetObjectResponse_contentLength :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Integer)
writeGetObjectResponse_contentLength = Lens.lens (\WriteGetObjectResponse' {contentLength} -> contentLength) (\s@WriteGetObjectResponse' {} a -> s {contentLength = a} :: WriteGetObjectResponse)

-- | Indicates whether an object stored in Amazon S3 has Object Lock enabled.
-- For more information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-lock.html Object Lock>.
writeGetObjectResponse_objectLockMode :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe ObjectLockMode)
writeGetObjectResponse_objectLockMode = Lens.lens (\WriteGetObjectResponse' {objectLockMode} -> objectLockMode) (\s@WriteGetObjectResponse' {} a -> s {objectLockMode = a} :: WriteGetObjectResponse)

-- | The date and time at which the object is no longer cacheable.
writeGetObjectResponse_expires :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.UTCTime)
writeGetObjectResponse_expires = Lens.lens (\WriteGetObjectResponse' {expires} -> expires) (\s@WriteGetObjectResponse' {} a -> s {expires = a} :: WriteGetObjectResponse) Prelude.. Lens.mapping Core._Time

-- | Provides information about object restoration operation and expiration
-- time of the restored object copy.
writeGetObjectResponse_restore :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_restore = Lens.lens (\WriteGetObjectResponse' {restore} -> restore) (\s@WriteGetObjectResponse' {} a -> s {restore = a} :: WriteGetObjectResponse)

-- | If object stored in Amazon S3 expiration is configured (see PUT Bucket
-- lifecycle) it includes expiry-date and rule-id key-value pairs providing
-- object expiration information. The value of the rule-id is URL encoded.
writeGetObjectResponse_expiration :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_expiration = Lens.lens (\WriteGetObjectResponse' {expiration} -> expiration) (\s@WriteGetObjectResponse' {} a -> s {expiration = a} :: WriteGetObjectResponse)

-- | Specifies whether an object stored in Amazon S3 is (@true@) or is not
-- (@false@) a delete marker.
writeGetObjectResponse_deleteMarker :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Bool)
writeGetObjectResponse_deleteMarker = Lens.lens (\WriteGetObjectResponse' {deleteMarker} -> deleteMarker) (\s@WriteGetObjectResponse' {} a -> s {deleteMarker = a} :: WriteGetObjectResponse)

-- | Encryption algorithm used if server-side encryption with a
-- customer-provided encryption key was specified for object stored in
-- Amazon S3.
writeGetObjectResponse_sSECustomerAlgorithm :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_sSECustomerAlgorithm = Lens.lens (\WriteGetObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@WriteGetObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: WriteGetObjectResponse)

-- | The number of tags, if any, on the object.
writeGetObjectResponse_tagCount :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Int)
writeGetObjectResponse_tagCount = Lens.lens (\WriteGetObjectResponse' {tagCount} -> tagCount) (\s@WriteGetObjectResponse' {} a -> s {tagCount = a} :: WriteGetObjectResponse)

-- | Set to the number of metadata entries not returned in @x-amz-meta@
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
writeGetObjectResponse_missingMeta :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Int)
writeGetObjectResponse_missingMeta = Lens.lens (\WriteGetObjectResponse' {missingMeta} -> missingMeta) (\s@WriteGetObjectResponse' {} a -> s {missingMeta = a} :: WriteGetObjectResponse)

-- | Indicates whether the object stored in Amazon S3 uses an S3 bucket key
-- for server-side encryption with Amazon Web Services KMS (SSE-KMS).
writeGetObjectResponse_bucketKeyEnabled :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Bool)
writeGetObjectResponse_bucketKeyEnabled = Lens.lens (\WriteGetObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@WriteGetObjectResponse' {} a -> s {bucketKeyEnabled = a} :: WriteGetObjectResponse)

-- | Indicates that a range of bytes was specified.
writeGetObjectResponse_acceptRanges :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_acceptRanges = Lens.lens (\WriteGetObjectResponse' {acceptRanges} -> acceptRanges) (\s@WriteGetObjectResponse' {} a -> s {acceptRanges = a} :: WriteGetObjectResponse)

-- | The class of storage used to store object in Amazon S3.
writeGetObjectResponse_storageClass :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe StorageClass)
writeGetObjectResponse_storageClass = Lens.lens (\WriteGetObjectResponse' {storageClass} -> storageClass) (\s@WriteGetObjectResponse' {} a -> s {storageClass = a} :: WriteGetObjectResponse)

-- | 128-bit MD5 digest of customer-provided encryption key used in Amazon S3
-- to encrypt data stored in S3. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/ServerSideEncryptionCustomerKeys.html Protecting data using server-side encryption with customer-provided encryption keys (SSE-C)>.
writeGetObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_sSECustomerKeyMD5 = Lens.lens (\WriteGetObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@WriteGetObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: WriteGetObjectResponse)

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for stored in Amazon S3 object.
writeGetObjectResponse_sSEKMSKeyId :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_sSEKMSKeyId = Lens.lens (\WriteGetObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@WriteGetObjectResponse' {} a -> s {sSEKMSKeyId = a} :: WriteGetObjectResponse) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
writeGetObjectResponse_contentEncoding :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_contentEncoding = Lens.lens (\WriteGetObjectResponse' {contentEncoding} -> contentEncoding) (\s@WriteGetObjectResponse' {} a -> s {contentEncoding = a} :: WriteGetObjectResponse)

-- | A string that uniquely identifies an error condition. Returned in the
-- \<Code> tag of the error XML response for a corresponding @GetObject@
-- call. Cannot be used with a successful @StatusCode@ header or when the
-- transformed object is provided in the body. All error codes from S3 are
-- sentence-cased. Regex value is \"^[A-Z][a-zA-Z]+$\".
writeGetObjectResponse_errorCode :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_errorCode = Lens.lens (\WriteGetObjectResponse' {errorCode} -> errorCode) (\s@WriteGetObjectResponse' {} a -> s {errorCode = a} :: WriteGetObjectResponse)

-- | The date and time when Object Lock is configured to expire.
writeGetObjectResponse_objectLockRetainUntilDate :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.UTCTime)
writeGetObjectResponse_objectLockRetainUntilDate = Lens.lens (\WriteGetObjectResponse' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@WriteGetObjectResponse' {} a -> s {objectLockRetainUntilDate = a} :: WriteGetObjectResponse) Prelude.. Lens.mapping Core._Time

-- | A map of metadata to store with the object in S3.
writeGetObjectResponse_metadata :: Lens.Lens' WriteGetObjectResponse (Prelude.HashMap Prelude.Text Prelude.Text)
writeGetObjectResponse_metadata = Lens.lens (\WriteGetObjectResponse' {metadata} -> metadata) (\s@WriteGetObjectResponse' {} a -> s {metadata = a} :: WriteGetObjectResponse) Prelude.. Lens.coerced

-- | Indicates if request involves bucket that is either a source or
-- destination in a Replication rule. For more information about S3
-- Replication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/replication.html Replication>.
writeGetObjectResponse_replicationStatus :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe ReplicationStatus)
writeGetObjectResponse_replicationStatus = Lens.lens (\WriteGetObjectResponse' {replicationStatus} -> replicationStatus) (\s@WriteGetObjectResponse' {} a -> s {replicationStatus = a} :: WriteGetObjectResponse)

-- | Specifies caching behavior along the request\/reply chain.
writeGetObjectResponse_cacheControl :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_cacheControl = Lens.lens (\WriteGetObjectResponse' {cacheControl} -> cacheControl) (\s@WriteGetObjectResponse' {} a -> s {cacheControl = a} :: WriteGetObjectResponse)

-- | The language the content is in.
writeGetObjectResponse_contentLanguage :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_contentLanguage = Lens.lens (\WriteGetObjectResponse' {contentLanguage} -> contentLanguage) (\s@WriteGetObjectResponse' {} a -> s {contentLanguage = a} :: WriteGetObjectResponse)

-- | Contains a generic description of the error condition. Returned in the
-- \<Message> tag of the error XML response for a corresponding @GetObject@
-- call. Cannot be used with a successful @StatusCode@ header or when the
-- transformed object is provided in body.
writeGetObjectResponse_errorMessage :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_errorMessage = Lens.lens (\WriteGetObjectResponse' {errorMessage} -> errorMessage) (\s@WriteGetObjectResponse' {} a -> s {errorMessage = a} :: WriteGetObjectResponse)

-- | The date and time that the object was last modified.
writeGetObjectResponse_lastModified :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.UTCTime)
writeGetObjectResponse_lastModified = Lens.lens (\WriteGetObjectResponse' {lastModified} -> lastModified) (\s@WriteGetObjectResponse' {} a -> s {lastModified = a} :: WriteGetObjectResponse) Prelude.. Lens.mapping Core._Time

-- | Indicates whether an object stored in Amazon S3 has an active legal
-- hold.
writeGetObjectResponse_objectLockLegalHoldStatus :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe ObjectLockLegalHoldStatus)
writeGetObjectResponse_objectLockLegalHoldStatus = Lens.lens (\WriteGetObjectResponse' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@WriteGetObjectResponse' {} a -> s {objectLockLegalHoldStatus = a} :: WriteGetObjectResponse)

-- | Specifies presentational information for the object.
writeGetObjectResponse_contentDisposition :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_contentDisposition = Lens.lens (\WriteGetObjectResponse' {contentDisposition} -> contentDisposition) (\s@WriteGetObjectResponse' {} a -> s {contentDisposition = a} :: WriteGetObjectResponse)

-- | The portion of the object returned in the response.
writeGetObjectResponse_contentRange :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_contentRange = Lens.lens (\WriteGetObjectResponse' {contentRange} -> contentRange) (\s@WriteGetObjectResponse' {} a -> s {contentRange = a} :: WriteGetObjectResponse)

-- | The server-side encryption algorithm used when storing requested object
-- in Amazon S3 (for example, AES256, aws:kms).
writeGetObjectResponse_serverSideEncryption :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe ServerSideEncryption)
writeGetObjectResponse_serverSideEncryption = Lens.lens (\WriteGetObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@WriteGetObjectResponse' {} a -> s {serverSideEncryption = a} :: WriteGetObjectResponse)

-- | A standard MIME type describing the format of the object data.
writeGetObjectResponse_contentType :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Text)
writeGetObjectResponse_contentType = Lens.lens (\WriteGetObjectResponse' {contentType} -> contentType) (\s@WriteGetObjectResponse' {} a -> s {contentType = a} :: WriteGetObjectResponse)

-- | The integer status code for an HTTP response of a corresponding
-- @GetObject@ request.
--
-- __Status Codes__
--
-- -   /200 - OK/
--
-- -   /206 - Partial Content/
--
-- -   /304 - Not Modified/
--
-- -   /400 - Bad Request/
--
-- -   /401 - Unauthorized/
--
-- -   /403 - Forbidden/
--
-- -   /404 - Not Found/
--
-- -   /405 - Method Not Allowed/
--
-- -   /409 - Conflict/
--
-- -   /411 - Length Required/
--
-- -   /412 - Precondition Failed/
--
-- -   /416 - Range Not Satisfiable/
--
-- -   /500 - Internal Server Error/
--
-- -   /503 - Service Unavailable/
writeGetObjectResponse_statusCode :: Lens.Lens' WriteGetObjectResponse (Prelude.Maybe Prelude.Int)
writeGetObjectResponse_statusCode = Lens.lens (\WriteGetObjectResponse' {statusCode} -> statusCode) (\s@WriteGetObjectResponse' {} a -> s {statusCode = a} :: WriteGetObjectResponse)

-- | Route prefix to the HTTP URL generated.
writeGetObjectResponse_requestRoute :: Lens.Lens' WriteGetObjectResponse Prelude.Text
writeGetObjectResponse_requestRoute = Lens.lens (\WriteGetObjectResponse' {requestRoute} -> requestRoute) (\s@WriteGetObjectResponse' {} a -> s {requestRoute = a} :: WriteGetObjectResponse)

-- | A single use encrypted token that maps @WriteGetObjectResponse@ to the
-- end user @GetObject@ request.
writeGetObjectResponse_requestToken :: Lens.Lens' WriteGetObjectResponse Prelude.Text
writeGetObjectResponse_requestToken = Lens.lens (\WriteGetObjectResponse' {requestToken} -> requestToken) (\s@WriteGetObjectResponse' {} a -> s {requestToken = a} :: WriteGetObjectResponse)

-- | The object data.
writeGetObjectResponse_body :: Lens.Lens' WriteGetObjectResponse Core.RequestBody
writeGetObjectResponse_body = Lens.lens (\WriteGetObjectResponse' {body} -> body) (\s@WriteGetObjectResponse' {} a -> s {body = a} :: WriteGetObjectResponse)

instance Core.AWSRequest WriteGetObjectResponse where
  type
    AWSResponse WriteGetObjectResponse =
      WriteGetObjectResponseResponse
  request =
    Request.s3vhost
      Prelude.. Request.postBody defaultService
  response =
    Response.receiveNull
      WriteGetObjectResponseResponse'

instance Core.ToBody WriteGetObjectResponse where
  toBody WriteGetObjectResponse' {..} = Core.toBody body

instance Core.ToHeaders WriteGetObjectResponse where
  toHeaders WriteGetObjectResponse' {..} =
    Prelude.mconcat
      [ "x-amz-fwd-header-x-amz-request-charged"
          Core.=# requestCharged,
        "x-amz-fwd-header-x-amz-mp-parts-count"
          Core.=# partsCount,
        "x-amz-fwd-header-ETag" Core.=# eTag,
        "x-amz-fwd-header-x-amz-version-id"
          Core.=# versionId,
        "Content-Length" Core.=# contentLength,
        "x-amz-fwd-header-x-amz-object-lock-mode"
          Core.=# objectLockMode,
        "x-amz-fwd-header-Expires" Core.=# expires,
        "x-amz-fwd-header-x-amz-restore" Core.=# restore,
        "x-amz-fwd-header-x-amz-expiration"
          Core.=# expiration,
        "x-amz-fwd-header-x-amz-delete-marker"
          Core.=# deleteMarker,
        "x-amz-fwd-header-x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "x-amz-fwd-header-x-amz-tagging-count"
          Core.=# tagCount,
        "x-amz-fwd-header-x-amz-missing-meta"
          Core.=# missingMeta,
        "x-amz-fwd-header-x-amz-server-side-encryption-bucket-key-enabled"
          Core.=# bucketKeyEnabled,
        "x-amz-fwd-header-accept-ranges"
          Core.=# acceptRanges,
        "x-amz-fwd-header-x-amz-storage-class"
          Core.=# storageClass,
        "x-amz-fwd-header-x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "x-amz-fwd-header-x-amz-server-side-encryption-aws-kms-key-id"
          Core.=# sSEKMSKeyId,
        "x-amz-fwd-header-Content-Encoding"
          Core.=# contentEncoding,
        "x-amz-fwd-error-code" Core.=# errorCode,
        "x-amz-fwd-header-x-amz-object-lock-retain-until-date"
          Core.=# objectLockRetainUntilDate,
        "x-amz-meta-" Core.=# metadata,
        "x-amz-fwd-header-x-amz-replication-status"
          Core.=# replicationStatus,
        "x-amz-fwd-header-Cache-Control"
          Core.=# cacheControl,
        "x-amz-fwd-header-Content-Language"
          Core.=# contentLanguage,
        "x-amz-fwd-error-message" Core.=# errorMessage,
        "x-amz-fwd-header-Last-Modified"
          Core.=# lastModified,
        "x-amz-fwd-header-x-amz-object-lock-legal-hold"
          Core.=# objectLockLegalHoldStatus,
        "x-amz-fwd-header-Content-Disposition"
          Core.=# contentDisposition,
        "x-amz-fwd-header-Content-Range"
          Core.=# contentRange,
        "x-amz-fwd-header-x-amz-server-side-encryption"
          Core.=# serverSideEncryption,
        "x-amz-fwd-header-Content-Type" Core.=# contentType,
        "x-amz-fwd-status" Core.=# statusCode,
        "x-amz-request-route" Core.=# requestRoute,
        "x-amz-request-token" Core.=# requestToken
      ]

instance Core.ToPath WriteGetObjectResponse where
  toPath = Prelude.const "/WriteGetObjectResponse"

instance Core.ToQuery WriteGetObjectResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newWriteGetObjectResponseResponse' smart constructor.
data WriteGetObjectResponseResponse = WriteGetObjectResponseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteGetObjectResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newWriteGetObjectResponseResponse ::
  WriteGetObjectResponseResponse
newWriteGetObjectResponseResponse =
  WriteGetObjectResponseResponse'

instance
  Prelude.NFData
    WriteGetObjectResponseResponse
  where
  rnf _ = ()
