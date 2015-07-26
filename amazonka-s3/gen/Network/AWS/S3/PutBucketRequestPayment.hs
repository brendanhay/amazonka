{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketRequestPayment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the request payment configuration for a bucket. By default, the
-- bucket owner pays for downloads from the bucket. This configuration
-- parameter enables the bucket owner (only) to specify that the person
-- requesting the download will be charged for the download. Documentation
-- on requester pays buckets can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/RequesterPaysBuckets.html
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketRequestPayment.html>
module Network.AWS.S3.PutBucketRequestPayment
    (
    -- * Request
      PutBucketRequestPayment
    -- ** Request constructor
    , putBucketRequestPayment
    -- ** Request lenses
    , pbrpContentMD5
    , pbrpBucket
    , pbrpRequestPaymentConfiguration

    -- * Response
    , PutBucketRequestPaymentResponse
    -- ** Response constructor
    , putBucketRequestPaymentResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketRequestPayment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbrpContentMD5'
--
-- * 'pbrpBucket'
--
-- * 'pbrpRequestPaymentConfiguration'
data PutBucketRequestPayment = PutBucketRequestPayment'
    { _pbrpContentMD5                  :: !(Maybe Text)
    , _pbrpBucket                      :: !BucketName
    , _pbrpRequestPaymentConfiguration :: !RequestPaymentConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketRequestPayment' smart constructor.
putBucketRequestPayment :: BucketName -> RequestPaymentConfiguration -> PutBucketRequestPayment
putBucketRequestPayment pBucket_ pRequestPaymentConfiguration_ =
    PutBucketRequestPayment'
    { _pbrpContentMD5 = Nothing
    , _pbrpBucket = pBucket_
    , _pbrpRequestPaymentConfiguration = pRequestPaymentConfiguration_
    }

-- | FIXME: Undocumented member.
pbrpContentMD5 :: Lens' PutBucketRequestPayment (Maybe Text)
pbrpContentMD5 = lens _pbrpContentMD5 (\ s a -> s{_pbrpContentMD5 = a});

-- | FIXME: Undocumented member.
pbrpBucket :: Lens' PutBucketRequestPayment BucketName
pbrpBucket = lens _pbrpBucket (\ s a -> s{_pbrpBucket = a});

-- | FIXME: Undocumented member.
pbrpRequestPaymentConfiguration :: Lens' PutBucketRequestPayment RequestPaymentConfiguration
pbrpRequestPaymentConfiguration = lens _pbrpRequestPaymentConfiguration (\ s a -> s{_pbrpRequestPaymentConfiguration = a});

instance AWSRequest PutBucketRequestPayment where
        type Sv PutBucketRequestPayment = S3
        type Rs PutBucketRequestPayment =
             PutBucketRequestPaymentResponse
        request = putXML
        response
          = receiveNull PutBucketRequestPaymentResponse'

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
          = mconcat ["/", toPath _pbrpBucket]

instance ToQuery PutBucketRequestPayment where
        toQuery = const (mconcat ["requestPayment"])

-- | /See:/ 'putBucketRequestPaymentResponse' smart constructor.
data PutBucketRequestPaymentResponse =
    PutBucketRequestPaymentResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketRequestPaymentResponse' smart constructor.
putBucketRequestPaymentResponse :: PutBucketRequestPaymentResponse
putBucketRequestPaymentResponse = PutBucketRequestPaymentResponse'
