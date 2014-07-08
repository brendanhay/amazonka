{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the cors configuration for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketCors where

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Smart constructor utilising default fields to
-- specify the minimum viable PutBucketCors request.
putBucketCors :: BucketName -- ^ 'pbcrBucket'
              -> CORSConfiguration -- ^ 'pbcrCORSConfiguration'
              -> PutBucketCors
putBucketCors p1 p2 = PutBucketCors
    { pbcrBucket = p1
    , pbcrCORSConfiguration = p2
    , pbcrContentMD5 = Nothing
    }

data PutBucketCors = PutBucketCors
    { pbcrBucket :: BucketName
    , pbcrCORSConfiguration :: CORSConfiguration
    , pbcrContentMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToPath PutBucketCors where
    toPath PutBucketCors{..} = mconcat
        [ "/"
        , toBS pbcrBucket
        ]

instance ToQuery PutBucketCors

instance ToHeaders PutBucketCors where
    toHeaders PutBucketCors{..} = concat
        [ "Content-MD5" =: pbcrContentMD5
        ]

instance ToBody PutBucketCors where
    toBody = undefined -- toBody . pbcrCORSConfiguration

instance AWSRequest PutBucketCors where
    type Sv PutBucketCors = S3

    request  = put
    response = response' undefined

data instance Rs PutBucketCors = PutBucketCorsResponse
    deriving (Eq, Show, Generic)
