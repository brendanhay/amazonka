{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketRequestPayment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the request payment configuration for a bucket. By default, the bucket
-- owner pays for downloads from the bucket. This configuration parameter
-- enables the bucket owner (only) to specify that the person requesting the
-- download will be charged for the download.
module Network.AWS.S3.PutBucketRequestPayment
    (
    -- * Request
      PutBucketRequestPayment
    -- ** Request constructor
    , putBucketRequestPayment
    -- ** Request lenses
    , pbrpBucket
    , pbrpContentMD5
    , pbrpRequestPaymentConfiguration

    -- * Response
    , PutBucketRequestPaymentResponse
    -- ** Response constructor
    , putBucketRequestPaymentResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutBucketRequestPayment = PutBucketRequestPayment
    { _pbrpBucket :: BucketName
    , _pbrpContentMD5 :: Maybe Text
    , _pbrpRequestPaymentConfiguration :: RequestPaymentConfiguration
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketRequestPayment' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @ContentMD5 ::@ @Maybe Text@
--
-- * @RequestPaymentConfiguration ::@ @RequestPaymentConfiguration@
--
putBucketRequestPayment :: BucketName -- ^ 'pbrpBucket'
                        -> RequestPaymentConfiguration -- ^ 'pbrpRequestPaymentConfiguration'
                        -> PutBucketRequestPayment
putBucketRequestPayment p1 p3 = PutBucketRequestPayment
    { _pbrpBucket = p1
    , _pbrpContentMD5 = Nothing
    , _pbrpRequestPaymentConfiguration = p3
    }

pbrpBucket :: Lens' PutBucketRequestPayment BucketName
pbrpBucket = lens _pbrpBucket (\s a -> s { _pbrpBucket = a })

pbrpContentMD5 :: Lens' PutBucketRequestPayment (Maybe Text)
pbrpContentMD5 = lens _pbrpContentMD5 (\s a -> s { _pbrpContentMD5 = a })

pbrpRequestPaymentConfiguration :: Lens' PutBucketRequestPayment RequestPaymentConfiguration
pbrpRequestPaymentConfiguration =
    lens _pbrpRequestPaymentConfiguration
         (\s a -> s { _pbrpRequestPaymentConfiguration = a })

instance ToPath PutBucketRequestPayment

instance ToQuery PutBucketRequestPayment

instance ToHeaders PutBucketRequestPayment where
    toHeaders PutBucketRequestPayment{..} = concat
        [ "Content-MD5" =: _pbrpContentMD5
        ]

instance ToBody PutBucketRequestPayment where
    toBody = toBody . encodeXML . _pbrpRequestPaymentConfiguration

data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketRequestPaymentResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
putBucketRequestPaymentResponse :: PutBucketRequestPaymentResponse
putBucketRequestPaymentResponse = PutBucketRequestPaymentResponse

instance AWSRequest PutBucketRequestPayment where
    type Sv PutBucketRequestPayment = S3
    type Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse

    request = get
    response _ = nullaryResponse PutBucketRequestPaymentResponse
