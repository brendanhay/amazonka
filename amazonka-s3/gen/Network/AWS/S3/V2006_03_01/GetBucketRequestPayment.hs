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
    , mkGetBucketRequestPaymentRequest
    -- ** Request lenses
    , gbrprBucket

    -- * Response
    , GetBucketRequestPaymentResponse
    -- ** Response lenses
    , gbrpoPayer
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketRequestPayment' request.
mkGetBucketRequestPaymentRequest :: BucketName -- ^ 'gbrprBucket'
                                 -> GetBucketRequestPayment
mkGetBucketRequestPaymentRequest p1 = GetBucketRequestPayment
    { _gbrprBucket = p1
    }
{-# INLINE mkGetBucketRequestPaymentRequest #-}

newtype GetBucketRequestPayment = GetBucketRequestPayment
    { _gbrprBucket :: BucketName
    } deriving (Show, Generic)

gbrprBucket :: Lens' GetBucketRequestPayment (BucketName)
gbrprBucket = lens _gbrprBucket (\s a -> s { _gbrprBucket = a })
{-# INLINE gbrprBucket #-}

instance ToPath GetBucketRequestPayment where
    toPath GetBucketRequestPayment{..} = mconcat
        [ "/"
        , toBS _gbrprBucket
        ]

instance ToQuery GetBucketRequestPayment where
    toQuery GetBucketRequestPayment{..} = mconcat
        [ "requestPayment"
        ]

instance ToHeaders GetBucketRequestPayment

instance ToBody GetBucketRequestPayment

newtype GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse
    { _gbrpoPayer :: Maybe Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Show, Generic)

-- | Specifies who pays for the download and request fees.
gbrpoPayer :: Lens' GetBucketRequestPaymentResponse (Maybe Payer)
gbrpoPayer = lens _gbrpoPayer (\s a -> s { _gbrpoPayer = a })
{-# INLINE gbrpoPayer #-}

instance FromXML GetBucketRequestPaymentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketRequestPayment where
    type Sv GetBucketRequestPayment = S3
    type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse

    request = get
    response _ = xmlResponse
