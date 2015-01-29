{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketRequestPayment
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the request payment configuration of a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketRequestPayment.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketRequestPayment = GetBucketRequestPayment
    { _gbrpBucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

newtype GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse
    { _gbrprPayer :: Maybe Payer
    } deriving (Eq, Read, Show)

-- | 'GetBucketRequestPaymentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrprPayer' @::@ 'Maybe' 'Payer'
--
getBucketRequestPaymentResponse :: GetBucketRequestPaymentResponse
getBucketRequestPaymentResponse = GetBucketRequestPaymentResponse
    { _gbrprPayer = Nothing
    }

-- | Specifies who pays for the download and request fees.
gbrprPayer :: Lens' GetBucketRequestPaymentResponse (Maybe Payer)
gbrprPayer = lens _gbrprPayer (\s a -> s { _gbrprPayer = a })

instance ToPath GetBucketRequestPayment where
    toPath GetBucketRequestPayment{..} = mconcat
        [ "/"
        , toText _gbrpBucket
        ]

instance ToQuery GetBucketRequestPayment where
    toQuery = const "requestPayment"

instance ToHeaders GetBucketRequestPayment

instance ToXMLRoot GetBucketRequestPayment where
    toXMLRoot = const (namespaced ns "GetBucketRequestPayment" [])

instance ToXML GetBucketRequestPayment

instance AWSRequest GetBucketRequestPayment where
    type Sv GetBucketRequestPayment = S3
    type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketRequestPaymentResponse where
    parseXML x = GetBucketRequestPaymentResponse
        <$> x .@? "Payer"
