{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketRequestPayment
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
module Network.AWS.S3.V2006_03_01.PutBucketRequestPayment
    (
    -- * Request
      PutBucketRequestPayment
    -- ** Request constructor
    , mkPutBucketRequestPaymentRequest
    -- ** Request lenses
    , pbrprBucket
    , pbrprContentMD5
    , pbrprRequestPaymentConfiguration

    -- * Response
    , PutBucketRequestPaymentResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketRequestPayment' request.
mkPutBucketRequestPaymentRequest :: BucketName -- ^ 'pbrprBucket'
                                 -> RequestPaymentConfiguration -- ^ 'pbrprRequestPaymentConfiguration'
                                 -> PutBucketRequestPayment
mkPutBucketRequestPaymentRequest p1 p2 = PutBucketRequestPayment
    { _pbrprBucket = p1
    , _pbrprContentMD5 = Nothing
    , _pbrprRequestPaymentConfiguration = p3
    }
{-# INLINE mkPutBucketRequestPaymentRequest #-}

data PutBucketRequestPayment = PutBucketRequestPayment
    { _pbrprBucket :: BucketName
    , _pbrprContentMD5 :: Maybe Text
    , _pbrprRequestPaymentConfiguration :: RequestPaymentConfiguration
    } deriving (Show, Generic)

pbrprBucket :: Lens' PutBucketRequestPayment (BucketName)
pbrprBucket = lens _pbrprBucket (\s a -> s { _pbrprBucket = a })
{-# INLINE pbrprBucket #-}

pbrprContentMD5 :: Lens' PutBucketRequestPayment (Maybe Text)
pbrprContentMD5 = lens _pbrprContentMD5 (\s a -> s { _pbrprContentMD5 = a })
{-# INLINE pbrprContentMD5 #-}

pbrprRequestPaymentConfiguration :: Lens' PutBucketRequestPayment (RequestPaymentConfiguration)
pbrprRequestPaymentConfiguration = lens _pbrprRequestPaymentConfiguration (\s a -> s { _pbrprRequestPaymentConfiguration = a })
{-# INLINE pbrprRequestPaymentConfiguration #-}

instance ToPath PutBucketRequestPayment where
    toPath PutBucketRequestPayment{..} = mconcat
        [ "/"
        , toBS _pbrprBucket
        ]

instance ToQuery PutBucketRequestPayment where
    toQuery PutBucketRequestPayment{..} = mconcat
        [ "requestPayment"
        ]

instance ToHeaders PutBucketRequestPayment

instance ToBody PutBucketRequestPayment

data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketRequestPayment where
    type Sv PutBucketRequestPayment = S3
    type Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse

    request = put
    response _ = nullaryResponse PutBucketRequestPaymentResponse
