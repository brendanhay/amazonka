{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.SelectObjectContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation filters the contents of an Amazon S3 object based on a simple structured query language (SQL) statement. In the request, along with the SQL expression, you must also specify a data serialization format (JSON, CSV, or Apache Parquet) of the object. Amazon S3 uses this format to parse object data into records, and returns only records that match the specified SQL expression. You must also specify the data serialization format for the response.
--
-- This action is not supported by Amazon S3 on Outposts.
-- For more information about Amazon S3 Select, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/selecting-content-from-objects.html Selecting Content from Objects> in the /Amazon Simple Storage Service Developer Guide/ .
-- For more information about using SQL with Amazon S3 Select, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-glacier-select-sql-reference.html SQL Reference for Amazon S3 Select and S3 Glacier Select> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- __Permissions__
-- You must have @s3:GetObject@ permission for this operation. Amazon S3 Select does not support anonymous access. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Object Data Formats/
-- You can use Amazon S3 Select to query objects that have the following format properties:
--
--     * /CSV, JSON, and Parquet/ - Objects must be in CSV, JSON, or Parquet format.
--
--
--     * /UTF-8/ - UTF-8 is the only encoding type Amazon S3 Select supports.
--
--
--     * /GZIP or BZIP2/ - CSV and JSON files can be compressed using GZIP or BZIP2. GZIP and BZIP2 are the only compression formats that Amazon S3 Select supports for CSV and JSON files. Amazon S3 Select supports columnar compression for Parquet using GZIP or Snappy. Amazon S3 Select does not support whole-object compression for Parquet objects.
--
--
--     * /Server-side encryption/ - Amazon S3 Select supports querying objects that are protected with server-side encryption.
-- For objects that are encrypted with customer-provided encryption keys (SSE-C), you must use HTTPS, and you must use the headers that are documented in the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject> . For more information about SSE-C, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys)> in the /Amazon Simple Storage Service Developer Guide/ .
-- For objects that are encrypted with Amazon S3 managed encryption keys (SSE-S3) and customer master keys (CMKs) stored in AWS Key Management Service (SSE-KMS), server-side encryption is handled transparently, so you don't need to specify anything. For more information about server-side encryption, including SSE-S3 and SSE-KMS, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
-- __Working with the Response Body__
-- Given the response size is unknown, Amazon S3 Select streams the response as a series of messages and includes a @Transfer-Encoding@ header with @chunked@ as its value in the response. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTSelectObjectAppendix.html Appendix: SelectObjectContent Response> .
--
-- __GetObject Support__
-- The @SelectObjectContent@ operation does not support the following @GetObject@ functionality. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject> .
--
--     * @Range@ : Although you can specify a scan range for an Amazon S3 Select request (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_SelectObjectContent.html#AmazonS3-SelectObjectContent-request-ScanRange SelectObjectContentRequest - ScanRange> in the request parameters), you cannot specify the range of bytes of an object to return.
--
--
--     * GLACIER, DEEP_ARCHIVE and REDUCED_REDUNDANCY storage classes: You cannot specify the GLACIER, DEEP_ARCHIVE, or @REDUCED_REDUNDANCY@ storage classes. For more information, about storage classes see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#storage-class-intro Storage Classes> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- __Special Errors__
-- For a list of special errors for this operation, see <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#SelectObjectContentErrorCodeList List of SELECT Object Content Error Codes>
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>
module Network.AWS.S3.SelectObjectContent
  ( -- * Creating a request
    SelectObjectContent (..),
    mkSelectObjectContent,

    -- ** Request lenses
    socBucket,
    socKey,
    socExpression,
    socExpressionType,
    socInputSerialization,
    socOutputSerialization,
    socExpectedBucketOwner,
    socRequestProgress,
    socSSECustomerAlgorithm,
    socSSECustomerKey,
    socSSECustomerKeyMD5,
    socScanRange,

    -- * Destructuring the response
    SelectObjectContentResponse (..),
    mkSelectObjectContentResponse,

    -- ** Response lenses
    socrrsPayload,
    socrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | Request to filter the contents of an Amazon S3 object based on a simple Structured Query Language (SQL) statement. In the request, along with the SQL expression, you must specify a data serialization format (JSON or CSV) of the object. Amazon S3 uses this to parse object data into records. It returns only records that match the specified SQL expression. You must also specify the data serialization format for the response. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectSELECTContent.html S3Select API Documentation> .
--
-- /See:/ 'mkSelectObjectContent' smart constructor.
data SelectObjectContent = SelectObjectContent'
  { -- | The S3 bucket.
    bucket :: Types.BucketName,
    -- | The object key.
    key :: Types.Key,
    -- | The expression that is used to query the object.
    expression :: Types.Expression,
    -- | The type of the provided expression (for example, SQL).
    expressionType :: Types.ExpressionType,
    -- | Describes the format of the data in the object that is being queried.
    inputSerialization :: Types.InputSerialization,
    -- | Describes the format of the data that you want Amazon S3 to return in response.
    outputSerialization :: Types.OutputSerialization,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    -- | Specifies if periodic request progress information should be enabled.
    requestProgress :: Core.Maybe Types.RequestProgress,
    -- | The SSE Algorithm used to encrypt the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
    sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm,
    -- | The SSE Customer Key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
    sSECustomerKey :: Core.Maybe Types.SSECustomerKey,
    -- | The SSE Customer Key MD5. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
    sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5,
    -- | Specifies the byte range of the object to get the records from. A record is processed when its first byte is contained by the range. This parameter is optional, but when specified, it must not be empty. See RFC 2616, Section 14.35.1 about how to specify the start and end of the range.
    --
    -- @ScanRange@ may be used in the following ways:
    --
    --     * @<scanrange><start>50</start><end>100</end></scanrange>@ - process only the records starting between the bytes 50 and 100 (inclusive, counting from zero)
    --
    --
    --     * @<scanrange><start>50</start></scanrange>@ - process only the records starting after the byte 50
    --
    --
    --     * @<scanrange><end>50</end></scanrange>@ - process only the records within the last 50 bytes of the file.
    scanRange :: Core.Maybe Types.ScanRange
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectObjectContent' value with any optional fields omitted.
mkSelectObjectContent ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.Key ->
  -- | 'expression'
  Types.Expression ->
  -- | 'expressionType'
  Types.ExpressionType ->
  -- | 'inputSerialization'
  Types.InputSerialization ->
  -- | 'outputSerialization'
  Types.OutputSerialization ->
  SelectObjectContent
mkSelectObjectContent
  bucket
  key
  expression
  expressionType
  inputSerialization
  outputSerialization =
    SelectObjectContent'
      { bucket,
        key,
        expression,
        expressionType,
        inputSerialization,
        outputSerialization,
        expectedBucketOwner = Core.Nothing,
        requestProgress = Core.Nothing,
        sSECustomerAlgorithm = Core.Nothing,
        sSECustomerKey = Core.Nothing,
        sSECustomerKeyMD5 = Core.Nothing,
        scanRange = Core.Nothing
      }

-- | The S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socBucket :: Lens.Lens' SelectObjectContent Types.BucketName
socBucket = Lens.field @"bucket"
{-# DEPRECATED socBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socKey :: Lens.Lens' SelectObjectContent Types.Key
socKey = Lens.field @"key"
{-# DEPRECATED socKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The expression that is used to query the object.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socExpression :: Lens.Lens' SelectObjectContent Types.Expression
socExpression = Lens.field @"expression"
{-# DEPRECATED socExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The type of the provided expression (for example, SQL).
--
-- /Note:/ Consider using 'expressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socExpressionType :: Lens.Lens' SelectObjectContent Types.ExpressionType
socExpressionType = Lens.field @"expressionType"
{-# DEPRECATED socExpressionType "Use generic-lens or generic-optics with 'expressionType' instead." #-}

-- | Describes the format of the data in the object that is being queried.
--
-- /Note:/ Consider using 'inputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socInputSerialization :: Lens.Lens' SelectObjectContent Types.InputSerialization
socInputSerialization = Lens.field @"inputSerialization"
{-# DEPRECATED socInputSerialization "Use generic-lens or generic-optics with 'inputSerialization' instead." #-}

-- | Describes the format of the data that you want Amazon S3 to return in response.
--
-- /Note:/ Consider using 'outputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socOutputSerialization :: Lens.Lens' SelectObjectContent Types.OutputSerialization
socOutputSerialization = Lens.field @"outputSerialization"
{-# DEPRECATED socOutputSerialization "Use generic-lens or generic-optics with 'outputSerialization' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socExpectedBucketOwner :: Lens.Lens' SelectObjectContent (Core.Maybe Types.ExpectedBucketOwner)
socExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED socExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Specifies if periodic request progress information should be enabled.
--
-- /Note:/ Consider using 'requestProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socRequestProgress :: Lens.Lens' SelectObjectContent (Core.Maybe Types.RequestProgress)
socRequestProgress = Lens.field @"requestProgress"
{-# DEPRECATED socRequestProgress "Use generic-lens or generic-optics with 'requestProgress' instead." #-}

-- | The SSE Algorithm used to encrypt the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socSSECustomerAlgorithm :: Lens.Lens' SelectObjectContent (Core.Maybe Types.SSECustomerAlgorithm)
socSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED socSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | The SSE Customer Key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socSSECustomerKey :: Lens.Lens' SelectObjectContent (Core.Maybe Types.SSECustomerKey)
socSSECustomerKey = Lens.field @"sSECustomerKey"
{-# DEPRECATED socSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | The SSE Customer Key MD5. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socSSECustomerKeyMD5 :: Lens.Lens' SelectObjectContent (Core.Maybe Types.SSECustomerKeyMD5)
socSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED socSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Specifies the byte range of the object to get the records from. A record is processed when its first byte is contained by the range. This parameter is optional, but when specified, it must not be empty. See RFC 2616, Section 14.35.1 about how to specify the start and end of the range.
--
-- @ScanRange@ may be used in the following ways:
--
--     * @<scanrange><start>50</start><end>100</end></scanrange>@ - process only the records starting between the bytes 50 and 100 (inclusive, counting from zero)
--
--
--     * @<scanrange><start>50</start></scanrange>@ - process only the records starting after the byte 50
--
--
--     * @<scanrange><end>50</end></scanrange>@ - process only the records within the last 50 bytes of the file.
--
--
--
-- /Note:/ Consider using 'scanRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socScanRange :: Lens.Lens' SelectObjectContent (Core.Maybe Types.ScanRange)
socScanRange = Lens.field @"scanRange"
{-# DEPRECATED socScanRange "Use generic-lens or generic-optics with 'scanRange' instead." #-}

instance Core.ToXML SelectObjectContent where
  toXML SelectObjectContent {..} =
    Core.toXMLNode "Expression" expression
      Core.<> Core.toXMLNode "ExpressionType" expressionType
      Core.<> Core.toXMLNode "InputSerialization" inputSerialization
      Core.<> Core.toXMLNode "OutputSerialization" outputSerialization
      Core.<> Core.toXMLNode "RequestProgress" Core.<$> requestProgress
      Core.<> Core.toXMLNode "ScanRange" Core.<$> scanRange
  toXMLDocument =
    Core.mkXMLElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}SelectObjectContentRequest"

instance Core.AWSRequest SelectObjectContent where
  type Rs SelectObjectContent = SelectObjectContentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery = Core.pure ("select&select-type=2", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption-customer-algorithm"
                        sSECustomerAlgorithm
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption-customer-key"
                        sSECustomerKey
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption-customer-key-MD5"
                        sSECustomerKeyMD5
                    ),
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          SelectObjectContentResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSelectObjectContentResponse' smart constructor.
data SelectObjectContentResponse = SelectObjectContentResponse'
  { -- | The array of results.
    payload :: Core.Maybe Types.SelectObjectContentEventStream,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectObjectContentResponse' value with any optional fields omitted.
mkSelectObjectContentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SelectObjectContentResponse
mkSelectObjectContentResponse responseStatus =
  SelectObjectContentResponse'
    { payload = Core.Nothing,
      responseStatus
    }

-- | The array of results.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socrrsPayload :: Lens.Lens' SelectObjectContentResponse (Core.Maybe Types.SelectObjectContentEventStream)
socrrsPayload = Lens.field @"payload"
{-# DEPRECATED socrrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socrrsResponseStatus :: Lens.Lens' SelectObjectContentResponse Core.Int
socrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED socrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
