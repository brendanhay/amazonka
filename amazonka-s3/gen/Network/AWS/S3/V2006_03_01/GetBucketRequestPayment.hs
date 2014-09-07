{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketRequestPayment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the request payment configuration of a bucket.
module Network.AWS.S3.V2006_03_01.GetBucketRequestPayment
    (
    -- * Request
      GetBucketRequestPayment
    -- ** Request constructor
    , mkGetBucketRequestPayment
    -- ** Request lenses
    , gbrpBucket

    -- * Response
    , GetBucketRequestPaymentResponse
    -- ** Response lenses
    , gbrprsPayer
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketRequestPayment = GetBucketRequestPayment
    { _gbrpBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketRequestPayment' request.
mkGetBucketRequestPayment :: BucketName -- ^ 'gbrpBucket'
                          -> GetBucketRequestPayment
mkGetBucketRequestPayment p1 = GetBucketRequestPayment
    { _gbrpBucket = p1
    }

gbrpBucket :: Lens' GetBucketRequestPayment BucketName
gbrpBucket = lens _gbrpBucket (\s a -> s { _gbrpBucket = a })

instance ToPath GetBucketRequestPayment where
    toPath GetBucketRequestPayment{..} = mconcat
        [ "/"
        , toBS _gbrpBucket
        ]

instance ToQuery GetBucketRequestPayment where
    toQuery GetBucketRequestPayment{..} = mconcat
        [ "requestPayment"
        ]

instance ToHeaders GetBucketRequestPayment

instance ToBody GetBucketRequestPayment

newtype GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse
    { _gbrprsPayer :: Maybe Payer
    } deriving (Show, Generic)

-- | Specifies who pays for the download and request fees.
gbrprsPayer :: Lens' GetBucketRequestPaymentResponse (Maybe Payer)
gbrprsPayer = lens _gbrprsPayer (\s a -> s { _gbrprsPayer = a })

instance FromXML GetBucketRequestPaymentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketRequestPayment where
    type Sv GetBucketRequestPayment = S3
    type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse

    request = get
    response _ = xmlResponse
