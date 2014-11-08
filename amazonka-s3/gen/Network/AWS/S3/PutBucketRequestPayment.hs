{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , pbrprBucket
    , pbrprContentMD5
    , pbrprRequestPaymentConfiguration

    -- * Response
    , PutBucketRequestPaymentResponse
    -- ** Response constructor
    , putBucketRequestPaymentResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data PutBucketRequestPayment = PutBucketRequestPayment
    { _pbrprBucket                      :: Text
    , _pbrprContentMD5                  :: Maybe Text
    , _pbrprRequestPaymentConfiguration :: RequestPaymentConfiguration
    } deriving (Eq, Show, Generic)

-- | 'PutBucketRequestPayment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbrprBucket' @::@ 'Text'
--
-- * 'pbrprContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbrprRequestPaymentConfiguration' @::@ 'RequestPaymentConfiguration'
--
putBucketRequestPayment :: Text -- ^ 'pbrprBucket'
                        -> RequestPaymentConfiguration -- ^ 'pbrprRequestPaymentConfiguration'
                        -> PutBucketRequestPayment
putBucketRequestPayment p1 p2 = PutBucketRequestPayment
    { _pbrprBucket                      = p1
    , _pbrprRequestPaymentConfiguration = p2
    , _pbrprContentMD5                  = Nothing
    }

pbrprBucket :: Lens' PutBucketRequestPayment Text
pbrprBucket = lens _pbrprBucket (\s a -> s { _pbrprBucket = a })

pbrprContentMD5 :: Lens' PutBucketRequestPayment (Maybe Text)
pbrprContentMD5 = lens _pbrprContentMD5 (\s a -> s { _pbrprContentMD5 = a })

pbrprRequestPaymentConfiguration :: Lens' PutBucketRequestPayment RequestPaymentConfiguration
pbrprRequestPaymentConfiguration =
    lens _pbrprRequestPaymentConfiguration
        (\s a -> s { _pbrprRequestPaymentConfiguration = a })

instance ToPath PutBucketRequestPayment where
    toPath PutBucketRequestPayment{..} = mconcat
        [ "/"
        , toText _pbrprBucket
        ]

instance ToQuery PutBucketRequestPayment where
    toQuery = const "requestPayment"

instance ToHeaders PutBucketRequestPayment where
    toHeaders PutBucketRequestPayment{..} = mconcat
        [ "Content-MD5" =: _pbrprContentMD5
        ]

instance ToBody PutBucketRequestPayment where
    toBody = toBody . encodeXML . _pbrprRequestPaymentConfiguration

data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse

-- | 'PutBucketRequestPaymentResponse' constructor.
putBucketRequestPaymentResponse :: PutBucketRequestPaymentResponse
putBucketRequestPaymentResponse = PutBucketRequestPaymentResponse

instance AWSRequest PutBucketRequestPayment where
    type Sv PutBucketRequestPayment = S3
    type Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse

    request  = put'
    response = const (nullaryResponse PutBucketRequestPaymentResponse)
