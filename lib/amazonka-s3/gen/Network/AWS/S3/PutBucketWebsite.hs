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
-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the configuration of the website that is specified in the @website@ subresource. To configure a bucket as a website, you can add this subresource on the bucket with website configuration information such as the file name of the index document and any redirect rules. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> .
--
--
-- This PUT operation requires the @S3:PutBucketWebsite@ permission. By default, only the bucket owner can configure the website attached to a bucket; however, bucket owners can allow other users to set the website configuration by writing a bucket policy that grants them the @S3:PutBucketWebsite@ permission.
--
-- To redirect all website requests sent to the bucket's website endpoint, you add a website configuration with the following elements. Because all requests are sent to another website, you don't need to provide index document name for the bucket.
--
--     * @WebsiteConfiguration@
--
--     * @RedirectAllRequestsTo@
--
--     * @HostName@
--
--     * @Protocol@
--
--
--
-- If you want granular control over redirects, you can use the following elements to add routing rules that describe conditions for redirecting requests and information about the redirect destination. In this case, the website configuration must provide an index document for the bucket, because some requests might not be redirected.
--
--     * @WebsiteConfiguration@
--
--     * @IndexDocument@
--
--     * @Suffix@
--
--     * @ErrorDocument@
--
--     * @Key@
--
--     * @RoutingRules@
--
--     * @RoutingRule@
--
--     * @Condition@
--
--     * @HttpErrorCodeReturnedEquals@
--
--     * @KeyPrefixEquals@
--
--     * @Redirect@
--
--     * @Protocol@
--
--     * @HostName@
--
--     * @ReplaceKeyPrefixWith@
--
--     * @ReplaceKeyWith@
--
--     * @HttpRedirectCode@
--
--
--
-- Amazon S3 has a limitation of 50 routing rules per website configuration. If you require more than 50 routing rules, you can use object redirect. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html Configuring an Object Redirect> in the /Amazon Simple Storage Service Developer Guide/ .
module Network.AWS.S3.PutBucketWebsite
  ( -- * Creating a Request
    putBucketWebsite,
    PutBucketWebsite,

    -- * Request Lenses
    pbwContentMD5,
    pbwExpectedBucketOwner,
    pbwBucket,
    pbwWebsiteConfiguration,

    -- * Destructuring the Response
    putBucketWebsiteResponse,
    PutBucketWebsiteResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketWebsite' smart constructor.
data PutBucketWebsite = PutBucketWebsite'
  { _pbwContentMD5 ::
      !(Maybe Text),
    _pbwExpectedBucketOwner :: !(Maybe Text),
    _pbwBucket :: !BucketName,
    _pbwWebsiteConfiguration :: !WebsiteConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketWebsite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbwContentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> . For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbwExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbwBucket' - The bucket name.
--
-- * 'pbwWebsiteConfiguration' - Container for the request.
putBucketWebsite ::
  -- | 'pbwBucket'
  BucketName ->
  -- | 'pbwWebsiteConfiguration'
  WebsiteConfiguration ->
  PutBucketWebsite
putBucketWebsite pBucket_ pWebsiteConfiguration_ =
  PutBucketWebsite'
    { _pbwContentMD5 = Nothing,
      _pbwExpectedBucketOwner = Nothing,
      _pbwBucket = pBucket_,
      _pbwWebsiteConfiguration = pWebsiteConfiguration_
    }

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> . For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbwContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwContentMD5 = lens _pbwContentMD5 (\s a -> s {_pbwContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbwExpectedBucketOwner :: Lens' PutBucketWebsite (Maybe Text)
pbwExpectedBucketOwner = lens _pbwExpectedBucketOwner (\s a -> s {_pbwExpectedBucketOwner = a})

-- | The bucket name.
pbwBucket :: Lens' PutBucketWebsite BucketName
pbwBucket = lens _pbwBucket (\s a -> s {_pbwBucket = a})

-- | Container for the request.
pbwWebsiteConfiguration :: Lens' PutBucketWebsite WebsiteConfiguration
pbwWebsiteConfiguration = lens _pbwWebsiteConfiguration (\s a -> s {_pbwWebsiteConfiguration = a})

instance AWSRequest PutBucketWebsite where
  type Rs PutBucketWebsite = PutBucketWebsiteResponse
  request = putXML s3
  response = receiveNull PutBucketWebsiteResponse'

instance Hashable PutBucketWebsite

instance NFData PutBucketWebsite

instance ToElement PutBucketWebsite where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}WebsiteConfiguration"
      . _pbwWebsiteConfiguration

instance ToHeaders PutBucketWebsite where
  toHeaders PutBucketWebsite' {..} =
    mconcat
      [ "Content-MD5" =# _pbwContentMD5,
        "x-amz-expected-bucket-owner" =# _pbwExpectedBucketOwner
      ]

instance ToPath PutBucketWebsite where
  toPath PutBucketWebsite' {..} = mconcat ["/", toBS _pbwBucket]

instance ToQuery PutBucketWebsite where
  toQuery = const (mconcat ["website"])

-- | /See:/ 'putBucketWebsiteResponse' smart constructor.
data PutBucketWebsiteResponse = PutBucketWebsiteResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketWebsiteResponse' with the minimum fields required to make a request.
putBucketWebsiteResponse ::
  PutBucketWebsiteResponse
putBucketWebsiteResponse = PutBucketWebsiteResponse'

instance NFData PutBucketWebsiteResponse
