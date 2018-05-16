{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketRequestPayment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the request payment configuration for a bucket. By default, the bucket owner pays for downloads from the bucket. This configuration parameter enables the bucket owner (only) to specify that the person requesting the download will be charged for the download. Documentation on requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html
module Network.AWS.S3.PutBucketRequestPayment
    (
    -- * Creating a Request
      putBucketRequestPayment
    , PutBucketRequestPayment
    -- * Request Lenses
    , pbrpContentMD5
    , pbrpBucket
    , pbrpRequestPaymentConfiguration

    -- * Destructuring the Response
    , putBucketRequestPaymentResponse
    , PutBucketRequestPaymentResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketRequestPayment' smart constructor.
data PutBucketRequestPayment = PutBucketRequestPayment'
  { _pbrpContentMD5                  :: !(Maybe Text)
  , _pbrpBucket                      :: !BucketName
  , _pbrpRequestPaymentConfiguration :: !RequestPaymentConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketRequestPayment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbrpContentMD5' - Undocumented member.
--
-- * 'pbrpBucket' - Undocumented member.
--
-- * 'pbrpRequestPaymentConfiguration' - Undocumented member.
putBucketRequestPayment
    :: BucketName -- ^ 'pbrpBucket'
    -> RequestPaymentConfiguration -- ^ 'pbrpRequestPaymentConfiguration'
    -> PutBucketRequestPayment
putBucketRequestPayment pBucket_ pRequestPaymentConfiguration_ =
  PutBucketRequestPayment'
    { _pbrpContentMD5 = Nothing
    , _pbrpBucket = pBucket_
    , _pbrpRequestPaymentConfiguration = pRequestPaymentConfiguration_
    }


-- | Undocumented member.
pbrpContentMD5 :: Lens' PutBucketRequestPayment (Maybe Text)
pbrpContentMD5 = lens _pbrpContentMD5 (\ s a -> s{_pbrpContentMD5 = a})

-- | Undocumented member.
pbrpBucket :: Lens' PutBucketRequestPayment BucketName
pbrpBucket = lens _pbrpBucket (\ s a -> s{_pbrpBucket = a})

-- | Undocumented member.
pbrpRequestPaymentConfiguration :: Lens' PutBucketRequestPayment RequestPaymentConfiguration
pbrpRequestPaymentConfiguration = lens _pbrpRequestPaymentConfiguration (\ s a -> s{_pbrpRequestPaymentConfiguration = a})

instance AWSRequest PutBucketRequestPayment where
        type Rs PutBucketRequestPayment =
             PutBucketRequestPaymentResponse
        request = putXML s3
        response
          = receiveNull PutBucketRequestPaymentResponse'

instance Hashable PutBucketRequestPayment where

instance NFData PutBucketRequestPayment where

instance ToElement PutBucketRequestPayment where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}RequestPaymentConfiguration"
              .
              _pbrpRequestPaymentConfiguration

instance ToHeaders PutBucketRequestPayment where
        toHeaders PutBucketRequestPayment'{..}
          = mconcat ["Content-MD5" =# _pbrpContentMD5]

instance ToPath PutBucketRequestPayment where
        toPath PutBucketRequestPayment'{..}
          = mconcat ["/", toBS _pbrpBucket]

instance ToQuery PutBucketRequestPayment where
        toQuery = const (mconcat ["requestPayment"])

-- | /See:/ 'putBucketRequestPaymentResponse' smart constructor.
data PutBucketRequestPaymentResponse =
  PutBucketRequestPaymentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketRequestPaymentResponse' with the minimum fields required to make a request.
--
putBucketRequestPaymentResponse
    :: PutBucketRequestPaymentResponse
putBucketRequestPaymentResponse = PutBucketRequestPaymentResponse'


instance NFData PutBucketRequestPaymentResponse where
