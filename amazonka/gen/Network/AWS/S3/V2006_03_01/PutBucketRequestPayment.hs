{-# LANGUAGE DeriveGeneric               #-}
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
module Network.AWS.S3.V2006_03_01.PutBucketRequestPayment where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (Response)
import           Prelude              hiding (head)

-- | Default PutBucketRequestPayment request.
putBucketRequestPayment :: RequestPaymentConfiguration -- ^ 'pbrprRequestPaymentConfiguration'
                        -> BucketName -- ^ 'pbrprBucket'
                        -> PutBucketRequestPayment
putBucketRequestPayment p1 p2 = PutBucketRequestPayment
    { pbrprRequestPaymentConfiguration = p1
    , pbrprBucket = p2
    , pbrprContentMD5 = Nothing
    }

data PutBucketRequestPayment = PutBucketRequestPayment
    { pbrprRequestPaymentConfiguration :: RequestPaymentConfiguration
    , pbrprBucket :: BucketName
    , pbrprContentMD5 :: Maybe Text
    } deriving (Show, Generic)

instance ToPath PutBucketRequestPayment where
    toPath PutBucketRequestPayment{..} = mconcat
        [ "/"
        , toBS pbrprBucket
        ]

instance ToQuery PutBucketRequestPayment

instance ToHeaders PutBucketRequestPayment where
    toHeaders PutBucketRequestPayment{..} = concat
        [ "Content-MD5" =: pbrprContentMD5
        ]

instance ToBody PutBucketRequestPayment where
    toBody = undefined -- toBody . pbrprRequestPaymentConfiguration

instance AWSRequest PutBucketRequestPayment where
    type Sv PutBucketRequestPayment = S3

    request  = put
    response = headerResponse . const $ Right PutBucketRequestPaymentResponse

data instance Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse
    deriving (Eq, Show, Generic)
