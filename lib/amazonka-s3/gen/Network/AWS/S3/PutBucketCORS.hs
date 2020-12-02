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
-- Module      : Network.AWS.S3.PutBucketCORS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the @cors@ configuration for your bucket. If the configuration exists, Amazon S3 replaces it.
--
--
-- To use this operation, you must be allowed to perform the @s3:PutBucketCORS@ action. By default, the bucket owner has this permission and can grant it to others.
--
-- You set this configuration on a bucket so that the bucket can service cross-origin requests. For example, you might want to enable a request whose origin is @http://www.example.com@ to access your Amazon S3 bucket at @my.example.bucket.com@ by using the browser's @XMLHttpRequest@ capability.
--
-- To enable cross-origin resource sharing (CORS) on a bucket, you add the @cors@ subresource to the bucket. The @cors@ subresource is an XML document in which you configure rules that identify origins and the HTTP methods that can be executed on your bucket. The document is limited to 64 KB in size.
--
-- When Amazon S3 receives a cross-origin request (or a pre-flight OPTIONS request) against a bucket, it evaluates the @cors@ configuration on the bucket and uses the first @CORSRule@ rule that matches the incoming browser request to enable a cross-origin request. For a rule to match, the following conditions must be met:
--
--     * The request's @Origin@ header must match @AllowedOrigin@ elements.
--
--     * The request method (for example, GET, PUT, HEAD, and so on) or the @Access-Control-Request-Method@ header in case of a pre-flight @OPTIONS@ request must be one of the @AllowedMethod@ elements.
--
--     * Every header specified in the @Access-Control-Request-Headers@ request header of a pre-flight request must match an @AllowedHeader@ element.
--
--
--
-- For more information about CORS, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketCors.html GetBucketCors>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html DeleteBucketCors>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html RESTOPTIONSobject>
module Network.AWS.S3.PutBucketCORS
  ( -- * Creating a Request
    putBucketCORS,
    PutBucketCORS,

    -- * Request Lenses
    pbcContentMD5,
    pbcExpectedBucketOwner,
    pbcBucket,
    pbcCORSConfiguration,

    -- * Destructuring the Response
    putBucketCORSResponse,
    PutBucketCORSResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketCORS' smart constructor.
data PutBucketCORS = PutBucketCORS'
  { _pbcContentMD5 ::
      !(Maybe Text),
    _pbcExpectedBucketOwner :: !(Maybe Text),
    _pbcBucket :: !BucketName,
    _pbcCORSConfiguration :: !CORSConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketCORS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbcContentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbcExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbcBucket' - Specifies the bucket impacted by the @cors@ configuration.
--
-- * 'pbcCORSConfiguration' - Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
putBucketCORS ::
  -- | 'pbcBucket'
  BucketName ->
  -- | 'pbcCORSConfiguration'
  CORSConfiguration ->
  PutBucketCORS
putBucketCORS pBucket_ pCORSConfiguration_ =
  PutBucketCORS'
    { _pbcContentMD5 = Nothing,
      _pbcExpectedBucketOwner = Nothing,
      _pbcBucket = pBucket_,
      _pbcCORSConfiguration = pCORSConfiguration_
    }

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbcContentMD5 :: Lens' PutBucketCORS (Maybe Text)
pbcContentMD5 = lens _pbcContentMD5 (\s a -> s {_pbcContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbcExpectedBucketOwner :: Lens' PutBucketCORS (Maybe Text)
pbcExpectedBucketOwner = lens _pbcExpectedBucketOwner (\s a -> s {_pbcExpectedBucketOwner = a})

-- | Specifies the bucket impacted by the @cors@ configuration.
pbcBucket :: Lens' PutBucketCORS BucketName
pbcBucket = lens _pbcBucket (\s a -> s {_pbcBucket = a})

-- | Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
pbcCORSConfiguration :: Lens' PutBucketCORS CORSConfiguration
pbcCORSConfiguration = lens _pbcCORSConfiguration (\s a -> s {_pbcCORSConfiguration = a})

instance AWSRequest PutBucketCORS where
  type Rs PutBucketCORS = PutBucketCORSResponse
  request = putXML s3
  response = receiveNull PutBucketCORSResponse'

instance Hashable PutBucketCORS

instance NFData PutBucketCORS

instance ToElement PutBucketCORS where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CORSConfiguration"
      . _pbcCORSConfiguration

instance ToHeaders PutBucketCORS where
  toHeaders PutBucketCORS' {..} =
    mconcat
      [ "Content-MD5" =# _pbcContentMD5,
        "x-amz-expected-bucket-owner" =# _pbcExpectedBucketOwner
      ]

instance ToPath PutBucketCORS where
  toPath PutBucketCORS' {..} = mconcat ["/", toBS _pbcBucket]

instance ToQuery PutBucketCORS where
  toQuery = const (mconcat ["cors"])

-- | /See:/ 'putBucketCORSResponse' smart constructor.
data PutBucketCORSResponse = PutBucketCORSResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketCORSResponse' with the minimum fields required to make a request.
putBucketCORSResponse ::
  PutBucketCORSResponse
putBucketCORSResponse = PutBucketCORSResponse'

instance NFData PutBucketCORSResponse
