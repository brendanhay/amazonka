{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.S3.V2006_03_01.PutBucketRequestPayment where

import Control.Lens
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketRequestPayment' request.
putBucketRequestPayment :: RequestPaymentConfiguration -- ^ '_pbrprRequestPaymentConfiguration'
                        -> BucketName -- ^ '_pbrprBucket'
                        -> PutBucketRequestPayment
putBucketRequestPayment p1 p2 = PutBucketRequestPayment
    { _pbrprRequestPaymentConfiguration = p1
    , _pbrprBucket = p2
    , _pbrprContentMD5 = Nothing
    }

data PutBucketRequestPayment = PutBucketRequestPayment
    { _pbrprRequestPaymentConfiguration :: RequestPaymentConfiguration
    , _pbrprBucket :: BucketName
    , _pbrprContentMD5 :: Maybe Text
    } deriving (Generic)

makeLenses ''PutBucketRequestPayment

instance ToPath PutBucketRequestPayment where
    toPath PutBucketRequestPayment{..} = mconcat
        [ "/"
        , toBS _pbrprBucket
        ]

instance ToQuery PutBucketRequestPayment

instance ToHeaders PutBucketRequestPayment where
    toHeaders PutBucketRequestPayment{..} = concat
        [ "Content-MD5" =: _pbrprContentMD5
        ]

instance ToBody PutBucketRequestPayment where
    toBody = toBody . encodeXML . _pbrprRequestPaymentConfiguration

data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutBucketRequestPaymentResponse

instance AWSRequest PutBucketRequestPayment where
    type Sv PutBucketRequestPayment = S3
    type Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse

    request = put
    response _ _ = return (Right PutBucketRequestPaymentResponse)
