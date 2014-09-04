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
    , getBucketRequestPayment
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

-- | Minimum specification for a 'GetBucketRequestPayment' request.
getBucketRequestPayment :: BucketName -- ^ 'gbrprBucket'
                        -> GetBucketRequestPayment
getBucketRequestPayment p1 = GetBucketRequestPayment
    { _gbrprBucket = p1
    }
{-# INLINE getBucketRequestPayment #-}

data GetBucketRequestPayment = GetBucketRequestPayment
    { _gbrprBucket :: BucketName
    } deriving (Show, Generic)

gbrprBucket :: Lens' GetBucketRequestPayment (BucketName)
gbrprBucket f x =
    f (_gbrprBucket x)
        <&> \y -> x { _gbrprBucket = y }
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

data GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse
    { _gbrpoPayer :: Maybe Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Show, Generic)

-- | Specifies who pays for the download and request fees.
gbrpoPayer :: Lens' GetBucketRequestPaymentResponse (Maybe Payer)
gbrpoPayer f x =
    f (_gbrpoPayer x)
        <&> \y -> x { _gbrpoPayer = y }
{-# INLINE gbrpoPayer #-}

instance FromXML GetBucketRequestPaymentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketRequestPayment where
    type Sv GetBucketRequestPayment = S3
    type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse

    request = get
    response _ = xmlResponse
