{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketRequestPayment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the request payment configuration of a bucket.
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
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketRequestPayment = GetBucketRequestPayment
    { _gbrpBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketRequestPayment' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
getBucketRequestPayment :: BucketName -- ^ 'gbrpBucket'
                        -> GetBucketRequestPayment
getBucketRequestPayment p1 = GetBucketRequestPayment
    { _gbrpBucket = p1
    }

gbrpBucket :: Lens' GetBucketRequestPayment BucketName
gbrpBucket = lens _gbrpBucket (\s a -> s { _gbrpBucket = a })

instance ToPath GetBucketRequestPayment

instance ToQuery GetBucketRequestPayment

instance ToHeaders GetBucketRequestPayment

instance ToBody GetBucketRequestPayment

newtype GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse
    { _gbrprPayer :: Maybe Payer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketRequestPaymentResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Payer ::@ @Maybe Payer@
--
getBucketRequestPaymentResponse :: GetBucketRequestPaymentResponse
getBucketRequestPaymentResponse = GetBucketRequestPaymentResponse
    { _gbrprPayer = Nothing
    }

-- | Specifies who pays for the download and request fees.
gbrprPayer :: Lens' GetBucketRequestPaymentResponse (Maybe Payer)
gbrprPayer = lens _gbrprPayer (\s a -> s { _gbrprPayer = a })

instance FromXML GetBucketRequestPaymentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketRequestPayment where
    type Sv GetBucketRequestPayment = S3
    type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse

    request = get
    response _ = xmlResponse
