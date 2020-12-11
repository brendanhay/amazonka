{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    socSSECustomerAlgorithm,
    socSSECustomerKey,
    socRequestProgress,
    socSSECustomerKeyMD5,
    socScanRange,
    socExpectedBucketOwner,
    socBucket,
    socKey,
    socExpression,
    socExpressionType,
    socInputSerialization,
    socOutputSerialization,

    -- * Destructuring the response
    SelectObjectContentResponse (..),
    mkSelectObjectContentResponse,

    -- ** Response lenses
    socrsPayload,
    socrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | Request to filter the contents of an Amazon S3 object based on a simple Structured Query Language (SQL) statement. In the request, along with the SQL expression, you must specify a data serialization format (JSON or CSV) of the object. Amazon S3 uses this to parse object data into records. It returns only records that match the specified SQL expression. You must also specify the data serialization format for the response. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectSELECTContent.html S3Select API Documentation> .
--
-- /See:/ 'mkSelectObjectContent' smart constructor.
data SelectObjectContent = SelectObjectContent'
  { sSECustomerAlgorithm ::
      Lude.Maybe Lude.Text,
    sSECustomerKey ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    requestProgress :: Lude.Maybe RequestProgress,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    scanRange :: Lude.Maybe ScanRange,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    key :: ObjectKey,
    expression :: Lude.Text,
    expressionType :: ExpressionType,
    inputSerialization :: InputSerialization,
    outputSerialization :: OutputSerialization
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelectObjectContent' with the minimum fields required to make a request.
--
-- * 'bucket' - The S3 bucket.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'expression' - The expression that is used to query the object.
-- * 'expressionType' - The type of the provided expression (for example, SQL).
-- * 'inputSerialization' - Describes the format of the data in the object that is being queried.
-- * 'key' - The object key.
-- * 'outputSerialization' - Describes the format of the data that you want Amazon S3 to return in response.
-- * 'requestProgress' - Specifies if periodic request progress information should be enabled.
-- * 'sSECustomerAlgorithm' - The SSE Algorithm used to encrypt the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
-- * 'sSECustomerKey' - The SSE Customer Key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
-- * 'sSECustomerKeyMD5' - The SSE Customer Key MD5. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
-- * 'scanRange' - Specifies the byte range of the object to get the records from. A record is processed when its first byte is contained by the range. This parameter is optional, but when specified, it must not be empty. See RFC 2616, Section 14.35.1 about how to specify the start and end of the range.
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
mkSelectObjectContent ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'expression'
  Lude.Text ->
  -- | 'expressionType'
  ExpressionType ->
  -- | 'inputSerialization'
  InputSerialization ->
  -- | 'outputSerialization'
  OutputSerialization ->
  SelectObjectContent
mkSelectObjectContent
  pBucket_
  pKey_
  pExpression_
  pExpressionType_
  pInputSerialization_
  pOutputSerialization_ =
    SelectObjectContent'
      { sSECustomerAlgorithm = Lude.Nothing,
        sSECustomerKey = Lude.Nothing,
        requestProgress = Lude.Nothing,
        sSECustomerKeyMD5 = Lude.Nothing,
        scanRange = Lude.Nothing,
        expectedBucketOwner = Lude.Nothing,
        bucket = pBucket_,
        key = pKey_,
        expression = pExpression_,
        expressionType = pExpressionType_,
        inputSerialization = pInputSerialization_,
        outputSerialization = pOutputSerialization_
      }

-- | The SSE Algorithm used to encrypt the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socSSECustomerAlgorithm :: Lens.Lens' SelectObjectContent (Lude.Maybe Lude.Text)
socSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: SelectObjectContent -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: SelectObjectContent)
{-# DEPRECATED socSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | The SSE Customer Key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socSSECustomerKey :: Lens.Lens' SelectObjectContent (Lude.Maybe (Lude.Sensitive Lude.Text))
socSSECustomerKey = Lens.lens (sSECustomerKey :: SelectObjectContent -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSECustomerKey = a} :: SelectObjectContent)
{-# DEPRECATED socSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Specifies if periodic request progress information should be enabled.
--
-- /Note:/ Consider using 'requestProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socRequestProgress :: Lens.Lens' SelectObjectContent (Lude.Maybe RequestProgress)
socRequestProgress = Lens.lens (requestProgress :: SelectObjectContent -> Lude.Maybe RequestProgress) (\s a -> s {requestProgress = a} :: SelectObjectContent)
{-# DEPRECATED socRequestProgress "Use generic-lens or generic-optics with 'requestProgress' instead." #-}

-- | The SSE Customer Key MD5. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socSSECustomerKeyMD5 :: Lens.Lens' SelectObjectContent (Lude.Maybe Lude.Text)
socSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: SelectObjectContent -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: SelectObjectContent)
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
socScanRange :: Lens.Lens' SelectObjectContent (Lude.Maybe ScanRange)
socScanRange = Lens.lens (scanRange :: SelectObjectContent -> Lude.Maybe ScanRange) (\s a -> s {scanRange = a} :: SelectObjectContent)
{-# DEPRECATED socScanRange "Use generic-lens or generic-optics with 'scanRange' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socExpectedBucketOwner :: Lens.Lens' SelectObjectContent (Lude.Maybe Lude.Text)
socExpectedBucketOwner = Lens.lens (expectedBucketOwner :: SelectObjectContent -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: SelectObjectContent)
{-# DEPRECATED socExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socBucket :: Lens.Lens' SelectObjectContent BucketName
socBucket = Lens.lens (bucket :: SelectObjectContent -> BucketName) (\s a -> s {bucket = a} :: SelectObjectContent)
{-# DEPRECATED socBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socKey :: Lens.Lens' SelectObjectContent ObjectKey
socKey = Lens.lens (key :: SelectObjectContent -> ObjectKey) (\s a -> s {key = a} :: SelectObjectContent)
{-# DEPRECATED socKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The expression that is used to query the object.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socExpression :: Lens.Lens' SelectObjectContent Lude.Text
socExpression = Lens.lens (expression :: SelectObjectContent -> Lude.Text) (\s a -> s {expression = a} :: SelectObjectContent)
{-# DEPRECATED socExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The type of the provided expression (for example, SQL).
--
-- /Note:/ Consider using 'expressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socExpressionType :: Lens.Lens' SelectObjectContent ExpressionType
socExpressionType = Lens.lens (expressionType :: SelectObjectContent -> ExpressionType) (\s a -> s {expressionType = a} :: SelectObjectContent)
{-# DEPRECATED socExpressionType "Use generic-lens or generic-optics with 'expressionType' instead." #-}

-- | Describes the format of the data in the object that is being queried.
--
-- /Note:/ Consider using 'inputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socInputSerialization :: Lens.Lens' SelectObjectContent InputSerialization
socInputSerialization = Lens.lens (inputSerialization :: SelectObjectContent -> InputSerialization) (\s a -> s {inputSerialization = a} :: SelectObjectContent)
{-# DEPRECATED socInputSerialization "Use generic-lens or generic-optics with 'inputSerialization' instead." #-}

-- | Describes the format of the data that you want Amazon S3 to return in response.
--
-- /Note:/ Consider using 'outputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socOutputSerialization :: Lens.Lens' SelectObjectContent OutputSerialization
socOutputSerialization = Lens.lens (outputSerialization :: SelectObjectContent -> OutputSerialization) (\s a -> s {outputSerialization = a} :: SelectObjectContent)
{-# DEPRECATED socOutputSerialization "Use generic-lens or generic-optics with 'outputSerialization' instead." #-}

instance Lude.AWSRequest SelectObjectContent where
  type Rs SelectObjectContent = SelectObjectContentResponse
  request = Req.postXML s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          SelectObjectContentResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement SelectObjectContent where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}SelectObjectContentRequest"

instance Lude.ToHeaders SelectObjectContent where
  toHeaders SelectObjectContent' {..} =
    Lude.mconcat
      [ "x-amz-server-side-encryption-customer-algorithm"
          Lude.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" Lude.=# sSECustomerKey,
        "x-amz-server-side-encryption-customer-key-MD5"
          Lude.=# sSECustomerKeyMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath SelectObjectContent where
  toPath SelectObjectContent' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery SelectObjectContent where
  toQuery = Lude.const (Lude.mconcat ["select&select-type=2"])

instance Lude.ToXML SelectObjectContent where
  toXML SelectObjectContent' {..} =
    Lude.mconcat
      [ "RequestProgress" Lude.@= requestProgress,
        "ScanRange" Lude.@= scanRange,
        "Expression" Lude.@= expression,
        "ExpressionType" Lude.@= expressionType,
        "InputSerialization" Lude.@= inputSerialization,
        "OutputSerialization" Lude.@= outputSerialization
      ]

-- | /See:/ 'mkSelectObjectContentResponse' smart constructor.
data SelectObjectContentResponse = SelectObjectContentResponse'
  { payload ::
      Lude.Maybe
        SelectObjectContentEventStream,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelectObjectContentResponse' with the minimum fields required to make a request.
--
-- * 'payload' - The array of results.
-- * 'responseStatus' - The response status code.
mkSelectObjectContentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SelectObjectContentResponse
mkSelectObjectContentResponse pResponseStatus_ =
  SelectObjectContentResponse'
    { payload = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The array of results.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socrsPayload :: Lens.Lens' SelectObjectContentResponse (Lude.Maybe SelectObjectContentEventStream)
socrsPayload = Lens.lens (payload :: SelectObjectContentResponse -> Lude.Maybe SelectObjectContentEventStream) (\s a -> s {payload = a} :: SelectObjectContentResponse)
{-# DEPRECATED socrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socrsResponseStatus :: Lens.Lens' SelectObjectContentResponse Lude.Int
socrsResponseStatus = Lens.lens (responseStatus :: SelectObjectContentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SelectObjectContentResponse)
{-# DEPRECATED socrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
