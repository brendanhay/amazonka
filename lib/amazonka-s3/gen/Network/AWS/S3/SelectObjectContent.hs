{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- For more information about Amazon S3 Select, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/selecting-content-from-objects.html Selecting Content from Objects> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For more information about using SQL with Amazon S3 Select, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-glacier-select-sql-reference.html SQL Reference for Amazon S3 Select and S3 Glacier Select> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- __Permissions__
--
-- You must have @s3:GetObject@ permission for this operation. Amazon S3 Select does not support anonymous access. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /Object Data Formats/
--
-- You can use Amazon S3 Select to query objects that have the following format properties:
--
--     * /CSV, JSON, and Parquet/ - Objects must be in CSV, JSON, or Parquet format.
--
--     * /UTF-8/ - UTF-8 is the only encoding type Amazon S3 Select supports.
--
--     * /GZIP or BZIP2/ - CSV and JSON files can be compressed using GZIP or BZIP2. GZIP and BZIP2 are the only compression formats that Amazon S3 Select supports for CSV and JSON files. Amazon S3 Select supports columnar compression for Parquet using GZIP or Snappy. Amazon S3 Select does not support whole-object compression for Parquet objects.
--
--     * /Server-side encryption/ - Amazon S3 Select supports querying objects that are protected with server-side encryption.
--
-- For objects that are encrypted with customer-provided encryption keys (SSE-C), you must use HTTPS, and you must use the headers that are documented in the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject> . For more information about SSE-C, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys)> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For objects that are encrypted with Amazon S3 managed encryption keys (SSE-S3) and customer master keys (CMKs) stored in AWS Key Management Service (SSE-KMS), server-side encryption is handled transparently, so you don't need to specify anything. For more information about server-side encryption, including SSE-S3 and SSE-KMS, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- __Working with the Response Body__
--
-- Given the response size is unknown, Amazon S3 Select streams the response as a series of messages and includes a @Transfer-Encoding@ header with @chunked@ as its value in the response. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTSelectObjectAppendix.html Appendix: SelectObjectContent Response> .
--
--
--
-- __GetObject Support__
--
-- The @SelectObjectContent@ operation does not support the following @GetObject@ functionality. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject> .
--
--     * @Range@ : Although you can specify a scan range for an Amazon S3 Select request (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_SelectObjectContent.html#AmazonS3-SelectObjectContent-request-ScanRange SelectObjectContentRequest - ScanRange> in the request parameters), you cannot specify the range of bytes of an object to return.
--
--     * GLACIER, DEEP_ARCHIVE and REDUCED_REDUNDANCY storage classes: You cannot specify the GLACIER, DEEP_ARCHIVE, or @REDUCED_REDUNDANCY@ storage classes. For more information, about storage classes see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#storage-class-intro Storage Classes> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
--
--
-- __Special Errors__
--
-- For a list of special errors for this operation, see <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#SelectObjectContentErrorCodeList List of SELECT Object Content Error Codes>
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>
module Network.AWS.S3.SelectObjectContent
  ( -- * Creating a Request
    selectObjectContent,
    SelectObjectContent,

    -- * Request Lenses
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

    -- * Destructuring the Response
    selectObjectContentResponse,
    SelectObjectContentResponse,

    -- * Response Lenses
    socrsPayload,
    socrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | Request to filter the contents of an Amazon S3 object based on a simple Structured Query Language (SQL) statement. In the request, along with the SQL expression, you must specify a data serialization format (JSON or CSV) of the object. Amazon S3 uses this to parse object data into records. It returns only records that match the specified SQL expression. You must also specify the data serialization format for the response. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectSELECTContent.html S3Select API Documentation> .
--
--
--
-- /See:/ 'selectObjectContent' smart constructor.
data SelectObjectContent = SelectObjectContent'
  { _socSSECustomerAlgorithm ::
      !(Maybe Text),
    _socSSECustomerKey :: !(Maybe (Sensitive Text)),
    _socRequestProgress :: !(Maybe RequestProgress),
    _socSSECustomerKeyMD5 :: !(Maybe Text),
    _socScanRange :: !(Maybe ScanRange),
    _socExpectedBucketOwner :: !(Maybe Text),
    _socBucket :: !BucketName,
    _socKey :: !ObjectKey,
    _socExpression :: !Text,
    _socExpressionType :: !ExpressionType,
    _socInputSerialization :: !InputSerialization,
    _socOutputSerialization :: !OutputSerialization
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelectObjectContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socSSECustomerAlgorithm' - The SSE Algorithm used to encrypt the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- * 'socSSECustomerKey' - The SSE Customer Key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- * 'socRequestProgress' - Specifies if periodic request progress information should be enabled.
--
-- * 'socSSECustomerKeyMD5' - The SSE Customer Key MD5. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
--
-- * 'socScanRange' - Specifies the byte range of the object to get the records from. A record is processed when its first byte is contained by the range. This parameter is optional, but when specified, it must not be empty. See RFC 2616, Section 14.35.1 about how to specify the start and end of the range. @ScanRange@ may be used in the following ways:     * @<scanrange><start>50</start><end>100</end></scanrange>@ - process only the records starting between the bytes 50 and 100 (inclusive, counting from zero)     * @<scanrange><start>50</start></scanrange>@ - process only the records starting after the byte 50     * @<scanrange><end>50</end></scanrange>@ - process only the records within the last 50 bytes of the file.
--
-- * 'socExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'socBucket' - The S3 bucket.
--
-- * 'socKey' - The object key.
--
-- * 'socExpression' - The expression that is used to query the object.
--
-- * 'socExpressionType' - The type of the provided expression (for example, SQL).
--
-- * 'socInputSerialization' - Describes the format of the data in the object that is being queried.
--
-- * 'socOutputSerialization' - Describes the format of the data that you want Amazon S3 to return in response.
selectObjectContent ::
  -- | 'socBucket'
  BucketName ->
  -- | 'socKey'
  ObjectKey ->
  -- | 'socExpression'
  Text ->
  -- | 'socExpressionType'
  ExpressionType ->
  -- | 'socInputSerialization'
  InputSerialization ->
  -- | 'socOutputSerialization'
  OutputSerialization ->
  SelectObjectContent
selectObjectContent
  pBucket_
  pKey_
  pExpression_
  pExpressionType_
  pInputSerialization_
  pOutputSerialization_ =
    SelectObjectContent'
      { _socSSECustomerAlgorithm = Nothing,
        _socSSECustomerKey = Nothing,
        _socRequestProgress = Nothing,
        _socSSECustomerKeyMD5 = Nothing,
        _socScanRange = Nothing,
        _socExpectedBucketOwner = Nothing,
        _socBucket = pBucket_,
        _socKey = pKey_,
        _socExpression = pExpression_,
        _socExpressionType = pExpressionType_,
        _socInputSerialization = pInputSerialization_,
        _socOutputSerialization = pOutputSerialization_
      }

-- | The SSE Algorithm used to encrypt the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
socSSECustomerAlgorithm :: Lens' SelectObjectContent (Maybe Text)
socSSECustomerAlgorithm = lens _socSSECustomerAlgorithm (\s a -> s {_socSSECustomerAlgorithm = a})

-- | The SSE Customer Key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
socSSECustomerKey :: Lens' SelectObjectContent (Maybe Text)
socSSECustomerKey = lens _socSSECustomerKey (\s a -> s {_socSSECustomerKey = a}) . mapping _Sensitive

-- | Specifies if periodic request progress information should be enabled.
socRequestProgress :: Lens' SelectObjectContent (Maybe RequestProgress)
socRequestProgress = lens _socRequestProgress (\s a -> s {_socRequestProgress = a})

-- | The SSE Customer Key MD5. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys> .
socSSECustomerKeyMD5 :: Lens' SelectObjectContent (Maybe Text)
socSSECustomerKeyMD5 = lens _socSSECustomerKeyMD5 (\s a -> s {_socSSECustomerKeyMD5 = a})

-- | Specifies the byte range of the object to get the records from. A record is processed when its first byte is contained by the range. This parameter is optional, but when specified, it must not be empty. See RFC 2616, Section 14.35.1 about how to specify the start and end of the range. @ScanRange@ may be used in the following ways:     * @<scanrange><start>50</start><end>100</end></scanrange>@ - process only the records starting between the bytes 50 and 100 (inclusive, counting from zero)     * @<scanrange><start>50</start></scanrange>@ - process only the records starting after the byte 50     * @<scanrange><end>50</end></scanrange>@ - process only the records within the last 50 bytes of the file.
socScanRange :: Lens' SelectObjectContent (Maybe ScanRange)
socScanRange = lens _socScanRange (\s a -> s {_socScanRange = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
socExpectedBucketOwner :: Lens' SelectObjectContent (Maybe Text)
socExpectedBucketOwner = lens _socExpectedBucketOwner (\s a -> s {_socExpectedBucketOwner = a})

-- | The S3 bucket.
socBucket :: Lens' SelectObjectContent BucketName
socBucket = lens _socBucket (\s a -> s {_socBucket = a})

-- | The object key.
socKey :: Lens' SelectObjectContent ObjectKey
socKey = lens _socKey (\s a -> s {_socKey = a})

-- | The expression that is used to query the object.
socExpression :: Lens' SelectObjectContent Text
socExpression = lens _socExpression (\s a -> s {_socExpression = a})

-- | The type of the provided expression (for example, SQL).
socExpressionType :: Lens' SelectObjectContent ExpressionType
socExpressionType = lens _socExpressionType (\s a -> s {_socExpressionType = a})

-- | Describes the format of the data in the object that is being queried.
socInputSerialization :: Lens' SelectObjectContent InputSerialization
socInputSerialization = lens _socInputSerialization (\s a -> s {_socInputSerialization = a})

-- | Describes the format of the data that you want Amazon S3 to return in response.
socOutputSerialization :: Lens' SelectObjectContent OutputSerialization
socOutputSerialization = lens _socOutputSerialization (\s a -> s {_socOutputSerialization = a})

instance AWSRequest SelectObjectContent where
  type Rs SelectObjectContent = SelectObjectContentResponse
  request = postXML s3
  response =
    receiveXML
      ( \s h x ->
          SelectObjectContentResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable SelectObjectContent

instance NFData SelectObjectContent

instance ToElement SelectObjectContent where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}SelectObjectContentRequest"

instance ToHeaders SelectObjectContent where
  toHeaders SelectObjectContent' {..} =
    mconcat
      [ "x-amz-server-side-encryption-customer-algorithm"
          =# _socSSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" =# _socSSECustomerKey,
        "x-amz-server-side-encryption-customer-key-MD5"
          =# _socSSECustomerKeyMD5,
        "x-amz-expected-bucket-owner" =# _socExpectedBucketOwner
      ]

instance ToPath SelectObjectContent where
  toPath SelectObjectContent' {..} =
    mconcat ["/", toBS _socBucket, "/", toBS _socKey]

instance ToQuery SelectObjectContent where
  toQuery = const (mconcat ["select&select-type=2"])

instance ToXML SelectObjectContent where
  toXML SelectObjectContent' {..} =
    mconcat
      [ "RequestProgress" @= _socRequestProgress,
        "ScanRange" @= _socScanRange,
        "Expression" @= _socExpression,
        "ExpressionType" @= _socExpressionType,
        "InputSerialization" @= _socInputSerialization,
        "OutputSerialization" @= _socOutputSerialization
      ]

-- | /See:/ 'selectObjectContentResponse' smart constructor.
data SelectObjectContentResponse = SelectObjectContentResponse'
  { _socrsPayload ::
      !( Maybe
           SelectObjectContentEventStream
       ),
    _socrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelectObjectContentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socrsPayload' - The array of results.
--
-- * 'socrsResponseStatus' - -- | The response status code.
selectObjectContentResponse ::
  -- | 'socrsResponseStatus'
  Int ->
  SelectObjectContentResponse
selectObjectContentResponse pResponseStatus_ =
  SelectObjectContentResponse'
    { _socrsPayload = Nothing,
      _socrsResponseStatus = pResponseStatus_
    }

-- | The array of results.
socrsPayload :: Lens' SelectObjectContentResponse (Maybe SelectObjectContentEventStream)
socrsPayload = lens _socrsPayload (\s a -> s {_socrsPayload = a})

-- | -- | The response status code.
socrsResponseStatus :: Lens' SelectObjectContentResponse Int
socrsResponseStatus = lens _socrsResponseStatus (\s a -> s {_socrsResponseStatus = a})

instance NFData SelectObjectContentResponse
