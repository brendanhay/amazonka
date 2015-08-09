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
-- Module      : Network.AWS.S3.GetBucketRequestPayment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the request payment configuration of a bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketRequestPayment.html AWS API Reference> for GetBucketRequestPayment.
module Network.AWS.S3.GetBucketRequestPayment
    (
    -- * Creating a Request
      getBucketRequestPayment
    , GetBucketRequestPayment
    -- * Request Lenses
    , gbrpBucket

    -- * Destructuring the Response
    , getBucketRequestPaymentResponse
    , GetBucketRequestPaymentResponse
    -- * Response Lenses
    , gbrprsPayer
    , gbrprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketRequestPayment' smart constructor.
newtype GetBucketRequestPayment = GetBucketRequestPayment'
    { _gbrpBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketRequestPayment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrpBucket'
getBucketRequestPayment
    :: BucketName -- ^ 'gbrpBucket'
    -> GetBucketRequestPayment
getBucketRequestPayment pBucket_ =
    GetBucketRequestPayment'
    { _gbrpBucket = pBucket_
    }

-- | Undocumented member.
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
                   (x .@? "Payer") <*> (pure (fromEnum s)))

instance ToHeaders GetBucketRequestPayment where
        toHeaders = const mempty

instance ToPath GetBucketRequestPayment where
        toPath GetBucketRequestPayment'{..}
          = mconcat ["/", toBS _gbrpBucket]

instance ToQuery GetBucketRequestPayment where
        toQuery = const (mconcat ["requestPayment"])

-- | /See:/ 'getBucketRequestPaymentResponse' smart constructor.
data GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse'
    { _gbrprsPayer  :: !(Maybe Payer)
    , _gbrprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketRequestPaymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrprsPayer'
--
-- * 'gbrprsStatus'
getBucketRequestPaymentResponse
    :: Int -- ^ 'gbrprsStatus'
    -> GetBucketRequestPaymentResponse
getBucketRequestPaymentResponse pStatus_ =
    GetBucketRequestPaymentResponse'
    { _gbrprsPayer = Nothing
    , _gbrprsStatus = pStatus_
    }

-- | Specifies who pays for the download and request fees.
gbrprsPayer :: Lens' GetBucketRequestPaymentResponse (Maybe Payer)
gbrprsPayer = lens _gbrprsPayer (\ s a -> s{_gbrprsPayer = a});

-- | The response status code.
gbrprsStatus :: Lens' GetBucketRequestPaymentResponse Int
gbrprsStatus = lens _gbrprsStatus (\ s a -> s{_gbrprsStatus = a});
