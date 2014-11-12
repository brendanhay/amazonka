{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , GetBucketRequestPaymentOutput
    -- ** Response constructor
    , getBucketRequestPaymentOutput
    -- ** Response lenses
    , gbrpoPayer
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketRequestPayment = GetBucketRequestPayment
    { _gbrpBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetBucketRequestPayment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrpBucket' @::@ 'Text'
--
getBucketRequestPayment :: Text -- ^ 'gbrpBucket'
                        -> GetBucketRequestPayment
getBucketRequestPayment p1 = GetBucketRequestPayment
    { _gbrpBucket = p1
    }

gbrpBucket :: Lens' GetBucketRequestPayment Text
gbrpBucket = lens _gbrpBucket (\s a -> s { _gbrpBucket = a })

instance ToPath GetBucketRequestPayment where
    toPath GetBucketRequestPayment{..} = mconcat
        [ "/"
        , toText _gbrpBucket
        ]

instance ToQuery GetBucketRequestPayment where
    toQuery = const "requestPayment"

instance ToHeaders GetBucketRequestPayment

newtype GetBucketRequestPaymentOutput = GetBucketRequestPaymentOutput
    { _gbrpoPayer :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetBucketRequestPaymentOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrpoPayer' @::@ 'Maybe' 'Text'
--
getBucketRequestPaymentOutput :: GetBucketRequestPaymentOutput
getBucketRequestPaymentOutput = GetBucketRequestPaymentOutput
    { _gbrpoPayer = Nothing
    }

-- | Specifies who pays for the download and request fees.
gbrpoPayer :: Lens' GetBucketRequestPaymentOutput (Maybe Text)
gbrpoPayer = lens _gbrpoPayer (\s a -> s { _gbrpoPayer = a })

instance FromXML GetBucketRequestPaymentOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetBucketRequestPaymentOutput"
instance AWSRequest GetBucketRequestPayment where
    type Sv GetBucketRequestPayment = S3
    type Rs GetBucketRequestPayment = GetBucketRequestPaymentOutput

    request  = get
    response = xmlResponse $ \h x -> GetBucketRequestPaymentOutput
        <$> x %| "Payer"
