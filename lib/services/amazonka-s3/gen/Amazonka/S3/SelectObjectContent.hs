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
-- Module      : Amazonka.S3.SelectObjectContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action filters the contents of an Amazon S3 object based on a
-- simple structured query language (SQL) statement. In the request, along
-- with the SQL expression, you must also specify a data serialization
-- format (JSON, CSV, or Apache Parquet) of the object. Amazon S3 uses this
-- format to parse object data into records, and returns only records that
-- match the specified SQL expression. You must also specify the data
-- serialization format for the response.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- For more information about Amazon S3 Select, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/selecting-content-from-objects.html Selecting Content from Objects>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-glacier-select-sql-reference-select.html SELECT Command>
-- in the /Amazon S3 User Guide/.
--
-- For more information about using SQL with Amazon S3 Select, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-glacier-select-sql-reference.html SQL Reference for Amazon S3 Select and S3 Glacier Select>
-- in the /Amazon S3 User Guide/.
--
-- __Permissions__
--
-- You must have @s3:GetObject@ permission for this operation. Amazon S3
-- Select does not support anonymous access. For more information about
-- permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>
-- in the /Amazon S3 User Guide/.
--
-- /Object Data Formats/
--
-- You can use Amazon S3 Select to query objects that have the following
-- format properties:
--
-- -   /CSV, JSON, and Parquet/ - Objects must be in CSV, JSON, or Parquet
--     format.
--
-- -   /UTF-8/ - UTF-8 is the only encoding type Amazon S3 Select supports.
--
-- -   /GZIP or BZIP2/ - CSV and JSON files can be compressed using GZIP or
--     BZIP2. GZIP and BZIP2 are the only compression formats that Amazon
--     S3 Select supports for CSV and JSON files. Amazon S3 Select supports
--     columnar compression for Parquet using GZIP or Snappy. Amazon S3
--     Select does not support whole-object compression for Parquet
--     objects.
--
-- -   /Server-side encryption/ - Amazon S3 Select supports querying
--     objects that are protected with server-side encryption.
--
--     For objects that are encrypted with customer-provided encryption
--     keys (SSE-C), you must use HTTPS, and you must use the headers that
--     are documented in the
--     <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>.
--     For more information about SSE-C, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys)>
--     in the /Amazon S3 User Guide/.
--
--     For objects that are encrypted with Amazon S3 managed encryption
--     keys (SSE-S3) and Amazon Web Services KMS keys (SSE-KMS),
--     server-side encryption is handled transparently, so you don\'t need
--     to specify anything. For more information about server-side
--     encryption, including SSE-S3 and SSE-KMS, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption>
--     in the /Amazon S3 User Guide/.
--
-- __Working with the Response Body__
--
-- Given the response size is unknown, Amazon S3 Select streams the
-- response as a series of messages and includes a @Transfer-Encoding@
-- header with @chunked@ as its value in the response. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTSelectObjectAppendix.html Appendix: SelectObjectContent Response>.
--
-- __GetObject Support__
--
-- The @SelectObjectContent@ action does not support the following
-- @GetObject@ functionality. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>.
--
-- -   @Range@: Although you can specify a scan range for an Amazon S3
--     Select request (see
--     <https://docs.aws.amazon.com/AmazonS3/latest/API/API_SelectObjectContent.html#AmazonS3-SelectObjectContent-request-ScanRange SelectObjectContentRequest - ScanRange>
--     in the request parameters), you cannot specify the range of bytes of
--     an object to return.
--
-- -   GLACIER, DEEP_ARCHIVE and REDUCED_REDUNDANCY storage classes: You
--     cannot specify the GLACIER, DEEP_ARCHIVE, or @REDUCED_REDUNDANCY@
--     storage classes. For more information, about storage classes see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#storage-class-intro Storage Classes>
--     in the /Amazon S3 User Guide/.
--
-- __Special Errors__
--
-- For a list of special errors for this operation, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#SelectObjectContentErrorCodeList List of SELECT Object Content Error Codes>
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>
module Amazonka.S3.SelectObjectContent
  ( -- * Creating a Request
    SelectObjectContent (..),
    newSelectObjectContent,

    -- * Request Lenses
    selectObjectContent_expectedBucketOwner,
    selectObjectContent_requestProgress,
    selectObjectContent_scanRange,
    selectObjectContent_sSECustomerAlgorithm,
    selectObjectContent_sSECustomerKeyMD5,
    selectObjectContent_sSECustomerKey,
    selectObjectContent_bucket,
    selectObjectContent_key,
    selectObjectContent_expression,
    selectObjectContent_expressionType,
    selectObjectContent_inputSerialization,
    selectObjectContent_outputSerialization,

    -- * Destructuring the Response
    SelectObjectContentResponse (..),
    newSelectObjectContentResponse,

    -- * Response Lenses
    selectObjectContentResponse_payload,
    selectObjectContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | Request to filter the contents of an Amazon S3 object based on a simple
-- Structured Query Language (SQL) statement. In the request, along with
-- the SQL expression, you must specify a data serialization format (JSON
-- or CSV) of the object. Amazon S3 uses this to parse object data into
-- records. It returns only records that match the specified SQL
-- expression. You must also specify the data serialization format for the
-- response. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectSELECTContent.html S3Select API Documentation>.
--
-- /See:/ 'newSelectObjectContent' smart constructor.
data SelectObjectContent = SelectObjectContent'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies if periodic request progress information should be enabled.
    requestProgress :: Prelude.Maybe RequestProgress,
    -- | Specifies the byte range of the object to get the records from. A record
    -- is processed when its first byte is contained by the range. This
    -- parameter is optional, but when specified, it must not be empty. See RFC
    -- 2616, Section 14.35.1 about how to specify the start and end of the
    -- range.
    --
    -- @ScanRange@may be used in the following ways:
    --
    -- -   @\<scanrange>\<start>50\<\/start>\<end>100\<\/end>\<\/scanrange>@ -
    --     process only the records starting between the bytes 50 and 100
    --     (inclusive, counting from zero)
    --
    -- -   @\<scanrange>\<start>50\<\/start>\<\/scanrange>@ - process only the
    --     records starting after the byte 50
    --
    -- -   @\<scanrange>\<end>50\<\/end>\<\/scanrange>@ - process only the
    --     records within the last 50 bytes of the file.
    scanRange :: Prelude.Maybe ScanRange,
    -- | The server-side encryption (SSE) algorithm used to encrypt the object.
    -- This parameter is needed only when the object was created using a
    -- checksum algorithm. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
    -- in the /Amazon S3 User Guide/.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The MD5 server-side encryption (SSE) customer managed key. This
    -- parameter is needed only when the object was created using a checksum
    -- algorithm. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
    -- in the /Amazon S3 User Guide/.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption (SSE) customer managed key. This parameter is
    -- needed only when the object was created using a checksum algorithm. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
    -- in the /Amazon S3 User Guide/.
    sSECustomerKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The S3 bucket.
    bucket :: BucketName,
    -- | The object key.
    key :: ObjectKey,
    -- | The expression that is used to query the object.
    expression :: Prelude.Text,
    -- | The type of the provided expression (for example, SQL).
    expressionType :: ExpressionType,
    -- | Describes the format of the data in the object that is being queried.
    inputSerialization :: InputSerialization,
    -- | Describes the format of the data that you want Amazon S3 to return in
    -- response.
    outputSerialization :: OutputSerialization
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectObjectContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'selectObjectContent_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestProgress', 'selectObjectContent_requestProgress' - Specifies if periodic request progress information should be enabled.
--
-- 'scanRange', 'selectObjectContent_scanRange' - Specifies the byte range of the object to get the records from. A record
-- is processed when its first byte is contained by the range. This
-- parameter is optional, but when specified, it must not be empty. See RFC
-- 2616, Section 14.35.1 about how to specify the start and end of the
-- range.
--
-- @ScanRange@may be used in the following ways:
--
-- -   @\<scanrange>\<start>50\<\/start>\<end>100\<\/end>\<\/scanrange>@ -
--     process only the records starting between the bytes 50 and 100
--     (inclusive, counting from zero)
--
-- -   @\<scanrange>\<start>50\<\/start>\<\/scanrange>@ - process only the
--     records starting after the byte 50
--
-- -   @\<scanrange>\<end>50\<\/end>\<\/scanrange>@ - process only the
--     records within the last 50 bytes of the file.
--
-- 'sSECustomerAlgorithm', 'selectObjectContent_sSECustomerAlgorithm' - The server-side encryption (SSE) algorithm used to encrypt the object.
-- This parameter is needed only when the object was created using a
-- checksum algorithm. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
-- in the /Amazon S3 User Guide/.
--
-- 'sSECustomerKeyMD5', 'selectObjectContent_sSECustomerKeyMD5' - The MD5 server-side encryption (SSE) customer managed key. This
-- parameter is needed only when the object was created using a checksum
-- algorithm. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
-- in the /Amazon S3 User Guide/.
--
-- 'sSECustomerKey', 'selectObjectContent_sSECustomerKey' - The server-side encryption (SSE) customer managed key. This parameter is
-- needed only when the object was created using a checksum algorithm. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
-- in the /Amazon S3 User Guide/.
--
-- 'bucket', 'selectObjectContent_bucket' - The S3 bucket.
--
-- 'key', 'selectObjectContent_key' - The object key.
--
-- 'expression', 'selectObjectContent_expression' - The expression that is used to query the object.
--
-- 'expressionType', 'selectObjectContent_expressionType' - The type of the provided expression (for example, SQL).
--
-- 'inputSerialization', 'selectObjectContent_inputSerialization' - Describes the format of the data in the object that is being queried.
--
-- 'outputSerialization', 'selectObjectContent_outputSerialization' - Describes the format of the data that you want Amazon S3 to return in
-- response.
newSelectObjectContent ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'expression'
  Prelude.Text ->
  -- | 'expressionType'
  ExpressionType ->
  -- | 'inputSerialization'
  InputSerialization ->
  -- | 'outputSerialization'
  OutputSerialization ->
  SelectObjectContent
newSelectObjectContent
  pBucket_
  pKey_
  pExpression_
  pExpressionType_
  pInputSerialization_
  pOutputSerialization_ =
    SelectObjectContent'
      { expectedBucketOwner =
          Prelude.Nothing,
        requestProgress = Prelude.Nothing,
        scanRange = Prelude.Nothing,
        sSECustomerAlgorithm = Prelude.Nothing,
        sSECustomerKeyMD5 = Prelude.Nothing,
        sSECustomerKey = Prelude.Nothing,
        bucket = pBucket_,
        key = pKey_,
        expression = pExpression_,
        expressionType = pExpressionType_,
        inputSerialization = pInputSerialization_,
        outputSerialization = pOutputSerialization_
      }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
selectObjectContent_expectedBucketOwner :: Lens.Lens' SelectObjectContent (Prelude.Maybe Prelude.Text)
selectObjectContent_expectedBucketOwner = Lens.lens (\SelectObjectContent' {expectedBucketOwner} -> expectedBucketOwner) (\s@SelectObjectContent' {} a -> s {expectedBucketOwner = a} :: SelectObjectContent)

-- | Specifies if periodic request progress information should be enabled.
selectObjectContent_requestProgress :: Lens.Lens' SelectObjectContent (Prelude.Maybe RequestProgress)
selectObjectContent_requestProgress = Lens.lens (\SelectObjectContent' {requestProgress} -> requestProgress) (\s@SelectObjectContent' {} a -> s {requestProgress = a} :: SelectObjectContent)

-- | Specifies the byte range of the object to get the records from. A record
-- is processed when its first byte is contained by the range. This
-- parameter is optional, but when specified, it must not be empty. See RFC
-- 2616, Section 14.35.1 about how to specify the start and end of the
-- range.
--
-- @ScanRange@may be used in the following ways:
--
-- -   @\<scanrange>\<start>50\<\/start>\<end>100\<\/end>\<\/scanrange>@ -
--     process only the records starting between the bytes 50 and 100
--     (inclusive, counting from zero)
--
-- -   @\<scanrange>\<start>50\<\/start>\<\/scanrange>@ - process only the
--     records starting after the byte 50
--
-- -   @\<scanrange>\<end>50\<\/end>\<\/scanrange>@ - process only the
--     records within the last 50 bytes of the file.
selectObjectContent_scanRange :: Lens.Lens' SelectObjectContent (Prelude.Maybe ScanRange)
selectObjectContent_scanRange = Lens.lens (\SelectObjectContent' {scanRange} -> scanRange) (\s@SelectObjectContent' {} a -> s {scanRange = a} :: SelectObjectContent)

-- | The server-side encryption (SSE) algorithm used to encrypt the object.
-- This parameter is needed only when the object was created using a
-- checksum algorithm. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
-- in the /Amazon S3 User Guide/.
selectObjectContent_sSECustomerAlgorithm :: Lens.Lens' SelectObjectContent (Prelude.Maybe Prelude.Text)
selectObjectContent_sSECustomerAlgorithm = Lens.lens (\SelectObjectContent' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@SelectObjectContent' {} a -> s {sSECustomerAlgorithm = a} :: SelectObjectContent)

-- | The MD5 server-side encryption (SSE) customer managed key. This
-- parameter is needed only when the object was created using a checksum
-- algorithm. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
-- in the /Amazon S3 User Guide/.
selectObjectContent_sSECustomerKeyMD5 :: Lens.Lens' SelectObjectContent (Prelude.Maybe Prelude.Text)
selectObjectContent_sSECustomerKeyMD5 = Lens.lens (\SelectObjectContent' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@SelectObjectContent' {} a -> s {sSECustomerKeyMD5 = a} :: SelectObjectContent)

-- | The server-side encryption (SSE) customer managed key. This parameter is
-- needed only when the object was created using a checksum algorithm. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Protecting data using SSE-C keys>
-- in the /Amazon S3 User Guide/.
selectObjectContent_sSECustomerKey :: Lens.Lens' SelectObjectContent (Prelude.Maybe Prelude.Text)
selectObjectContent_sSECustomerKey = Lens.lens (\SelectObjectContent' {sSECustomerKey} -> sSECustomerKey) (\s@SelectObjectContent' {} a -> s {sSECustomerKey = a} :: SelectObjectContent) Prelude.. Lens.mapping Core._Sensitive

-- | The S3 bucket.
selectObjectContent_bucket :: Lens.Lens' SelectObjectContent BucketName
selectObjectContent_bucket = Lens.lens (\SelectObjectContent' {bucket} -> bucket) (\s@SelectObjectContent' {} a -> s {bucket = a} :: SelectObjectContent)

-- | The object key.
selectObjectContent_key :: Lens.Lens' SelectObjectContent ObjectKey
selectObjectContent_key = Lens.lens (\SelectObjectContent' {key} -> key) (\s@SelectObjectContent' {} a -> s {key = a} :: SelectObjectContent)

-- | The expression that is used to query the object.
selectObjectContent_expression :: Lens.Lens' SelectObjectContent Prelude.Text
selectObjectContent_expression = Lens.lens (\SelectObjectContent' {expression} -> expression) (\s@SelectObjectContent' {} a -> s {expression = a} :: SelectObjectContent)

-- | The type of the provided expression (for example, SQL).
selectObjectContent_expressionType :: Lens.Lens' SelectObjectContent ExpressionType
selectObjectContent_expressionType = Lens.lens (\SelectObjectContent' {expressionType} -> expressionType) (\s@SelectObjectContent' {} a -> s {expressionType = a} :: SelectObjectContent)

-- | Describes the format of the data in the object that is being queried.
selectObjectContent_inputSerialization :: Lens.Lens' SelectObjectContent InputSerialization
selectObjectContent_inputSerialization = Lens.lens (\SelectObjectContent' {inputSerialization} -> inputSerialization) (\s@SelectObjectContent' {} a -> s {inputSerialization = a} :: SelectObjectContent)

-- | Describes the format of the data that you want Amazon S3 to return in
-- response.
selectObjectContent_outputSerialization :: Lens.Lens' SelectObjectContent OutputSerialization
selectObjectContent_outputSerialization = Lens.lens (\SelectObjectContent' {outputSerialization} -> outputSerialization) (\s@SelectObjectContent' {} a -> s {outputSerialization = a} :: SelectObjectContent)

instance Core.AWSRequest SelectObjectContent where
  type
    AWSResponse SelectObjectContent =
      SelectObjectContentResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          SelectObjectContentResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SelectObjectContent where
  hashWithSalt _salt SelectObjectContent' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestProgress
      `Prelude.hashWithSalt` scanRange
      `Prelude.hashWithSalt` sSECustomerAlgorithm
      `Prelude.hashWithSalt` sSECustomerKeyMD5
      `Prelude.hashWithSalt` sSECustomerKey
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` expressionType
      `Prelude.hashWithSalt` inputSerialization
      `Prelude.hashWithSalt` outputSerialization

instance Prelude.NFData SelectObjectContent where
  rnf SelectObjectContent' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestProgress
      `Prelude.seq` Prelude.rnf scanRange
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf sSECustomerKey
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf expressionType
      `Prelude.seq` Prelude.rnf inputSerialization
      `Prelude.seq` Prelude.rnf outputSerialization

instance Core.ToElement SelectObjectContent where
  toElement =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}SelectObjectContentRequest"

instance Core.ToHeaders SelectObjectContent where
  toHeaders SelectObjectContent' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "x-amz-server-side-encryption-customer-key"
          Core.=# sSECustomerKey
      ]

instance Core.ToPath SelectObjectContent where
  toPath SelectObjectContent' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery SelectObjectContent where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["select&select-type=2"])

instance Core.ToXML SelectObjectContent where
  toXML SelectObjectContent' {..} =
    Prelude.mconcat
      [ "RequestProgress" Core.@= requestProgress,
        "ScanRange" Core.@= scanRange,
        "Expression" Core.@= expression,
        "ExpressionType" Core.@= expressionType,
        "InputSerialization" Core.@= inputSerialization,
        "OutputSerialization" Core.@= outputSerialization
      ]

-- | /See:/ 'newSelectObjectContentResponse' smart constructor.
data SelectObjectContentResponse = SelectObjectContentResponse'
  { -- | The array of results.
    payload :: Prelude.Maybe SelectObjectContentEventStream,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectObjectContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'selectObjectContentResponse_payload' - The array of results.
--
-- 'httpStatus', 'selectObjectContentResponse_httpStatus' - The response's http status code.
newSelectObjectContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SelectObjectContentResponse
newSelectObjectContentResponse pHttpStatus_ =
  SelectObjectContentResponse'
    { payload =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The array of results.
selectObjectContentResponse_payload :: Lens.Lens' SelectObjectContentResponse (Prelude.Maybe SelectObjectContentEventStream)
selectObjectContentResponse_payload = Lens.lens (\SelectObjectContentResponse' {payload} -> payload) (\s@SelectObjectContentResponse' {} a -> s {payload = a} :: SelectObjectContentResponse)

-- | The response's http status code.
selectObjectContentResponse_httpStatus :: Lens.Lens' SelectObjectContentResponse Prelude.Int
selectObjectContentResponse_httpStatus = Lens.lens (\SelectObjectContentResponse' {httpStatus} -> httpStatus) (\s@SelectObjectContentResponse' {} a -> s {httpStatus = a} :: SelectObjectContentResponse)

instance Prelude.NFData SelectObjectContentResponse where
  rnf SelectObjectContentResponse' {..} =
    Prelude.rnf payload
      `Prelude.seq` Prelude.rnf httpStatus
