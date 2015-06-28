{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.GetBucketRequestPayment
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the request payment configuration of a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketRequestPayment.html>
module Network.AWS.S3.GetBucketRequestPayment
    (
    -- * Request
      GetBucketRequestPayment
    -- ** Request constructor
    , getBucketRequestPayment
    -- ** Request lenses
    , gbrpBucket

    -- * Response
    , GetBucketRequestPaymentResponse
    -- ** Response constructor
    , getBucketRequestPaymentResponse
    -- ** Response lenses
    , gbrprPayer
    , gbrprStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketRequestPayment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrpBucket'
newtype GetBucketRequestPayment = GetBucketRequestPayment'
    { _gbrpBucket :: BucketName
    } deriving (Eq,Read,Show)

-- | 'GetBucketRequestPayment' smart constructor.
getBucketRequestPayment :: BucketName -> GetBucketRequestPayment
getBucketRequestPayment pBucket =
    GetBucketRequestPayment'
    { _gbrpBucket = pBucket
    }

-- | FIXME: Undocumented member.
gbrpBucket :: Lens' GetBucketRequestPayment BucketName
gbrpBucket = lens _gbrpBucket (\ s a -> s{_gbrpBucket = a});

instance AWSRequest GetBucketRequestPayment where
        type Sv GetBucketRequestPayment = S3
        type Rs GetBucketRequestPayment =
             GetBucketRequestPaymentResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketRequestPaymentResponse' <$>
                   (x .@? "Payer") <*> (pure s))

instance ToHeaders GetBucketRequestPayment where
        toHeaders = const mempty

instance ToPath GetBucketRequestPayment where
        toPath GetBucketRequestPayment'{..}
          = mconcat ["/", toText _gbrpBucket]

instance ToQuery GetBucketRequestPayment where
        toQuery = const (mconcat ["requestPayment"])

-- | /See:/ 'getBucketRequestPaymentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrprPayer'
--
-- * 'gbrprStatus'
data GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse'
    { _gbrprPayer  :: !(Maybe Payer)
    , _gbrprStatus :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetBucketRequestPaymentResponse' smart constructor.
getBucketRequestPaymentResponse :: Status -> GetBucketRequestPaymentResponse
getBucketRequestPaymentResponse pStatus =
    GetBucketRequestPaymentResponse'
    { _gbrprPayer = Nothing
    , _gbrprStatus = pStatus
    }

-- | Specifies who pays for the download and request fees.
gbrprPayer :: Lens' GetBucketRequestPaymentResponse (Maybe Payer)
gbrprPayer = lens _gbrprPayer (\ s a -> s{_gbrprPayer = a});

-- | FIXME: Undocumented member.
gbrprStatus :: Lens' GetBucketRequestPaymentResponse Status
gbrprStatus = lens _gbrprStatus (\ s a -> s{_gbrprStatus = a});
