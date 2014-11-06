{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , gbrprBucket

    -- * Response
    , GetBucketRequestPaymentOutput
    -- ** Response constructor
    , getBucketRequestPaymentOutput
    -- ** Response lenses
    , gbrpoPayer
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

newtype GetBucketRequestPayment = GetBucketRequestPayment
    { _gbrprBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketRequestPayment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrprBucket' @::@ 'BucketName'
--
getBucketRequestPayment :: BucketName -- ^ 'gbrprBucket'
                        -> GetBucketRequestPayment
getBucketRequestPayment p1 = GetBucketRequestPayment
    { _gbrprBucket = p1
    }

gbrprBucket :: Lens' GetBucketRequestPayment BucketName
gbrprBucket = lens _gbrprBucket (\s a -> s { _gbrprBucket = a })

instance ToPath GetBucketRequestPayment where
    toPath GetBucketRequestPayment{..} = mconcat
        [ "/"
        , toText _gbrprBucket
        ]

instance ToQuery GetBucketRequestPayment where
    toQuery = const "requestPayment"

instance ToHeaders GetBucketRequestPayment

newtype GetBucketRequestPaymentOutput = GetBucketRequestPaymentOutput
    { _gbrpoPayer :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

instance AWSRequest GetBucketRequestPayment where
    type Sv GetBucketRequestPayment = S3
    type Rs GetBucketRequestPayment = GetBucketRequestPaymentOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "Payer"
