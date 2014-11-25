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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketRequestPayment.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketRequestPayment = PutBucketRequestPayment
    { _pbrpBucket                      :: Text
    , _pbrpContentMD5                  :: Maybe Text
    , _pbrpRequestPaymentConfiguration :: RequestPaymentConfiguration
    } deriving (Eq, Show)

-- | 'PutBucketRequestPayment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbrpBucket' @::@ 'Text'
--
-- * 'pbrpContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbrpRequestPaymentConfiguration' @::@ 'RequestPaymentConfiguration'
--
putBucketRequestPayment :: Text -- ^ 'pbrpBucket'
                        -> RequestPaymentConfiguration -- ^ 'pbrpRequestPaymentConfiguration'
                        -> PutBucketRequestPayment
putBucketRequestPayment p1 p2 = PutBucketRequestPayment
    { _pbrpBucket                      = p1
    , _pbrpRequestPaymentConfiguration = p2
    , _pbrpContentMD5                  = Nothing
    }

pbrpBucket :: Lens' PutBucketRequestPayment Text
pbrpBucket = lens _pbrpBucket (\s a -> s { _pbrpBucket = a })

pbrpContentMD5 :: Lens' PutBucketRequestPayment (Maybe Text)
pbrpContentMD5 = lens _pbrpContentMD5 (\s a -> s { _pbrpContentMD5 = a })

pbrpRequestPaymentConfiguration :: Lens' PutBucketRequestPayment RequestPaymentConfiguration
pbrpRequestPaymentConfiguration =
    lens _pbrpRequestPaymentConfiguration
        (\s a -> s { _pbrpRequestPaymentConfiguration = a })

data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketRequestPaymentResponse' constructor.
putBucketRequestPaymentResponse :: PutBucketRequestPaymentResponse
putBucketRequestPaymentResponse = PutBucketRequestPaymentResponse

instance ToPath PutBucketRequestPayment where
    toPath PutBucketRequestPayment{..} = mconcat
        [ "/"
        , toText _pbrpBucket
        ]

instance ToQuery PutBucketRequestPayment where
    toQuery = const "requestPayment"

instance ToHeaders PutBucketRequestPayment where
    toHeaders PutBucketRequestPayment{..} = mconcat
        [ "Content-MD5" =: _pbrpContentMD5
        ]

instance ToXMLRoot PutBucketRequestPayment where
    toXMLRoot PutBucketRequestPayment{..} = namespaced ns "PutBucketRequestPayment"
        [ "RequestPaymentConfiguration" =@ _pbrpRequestPaymentConfiguration
        ]

instance ToXML PutBucketRequestPayment

instance AWSRequest PutBucketRequestPayment where
    type Sv PutBucketRequestPayment = S3
    type Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse

    request  = put
    response = nullResponse PutBucketRequestPaymentResponse
