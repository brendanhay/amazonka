{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces a policy on a bucket. If the bucket already has a policy, the one
-- in this request completely replaces it.
module Network.AWS.S3.V2006_03_01.PutBucketPolicy where

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
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Default PutBucketPolicy request.
putBucketPolicy :: Text -- ^ '_pbprPolicy'
                -> BucketName -- ^ '_pbprBucket'
                -> PutBucketPolicy
putBucketPolicy p1 p2 = PutBucketPolicy
    { _pbprPolicy = p1
    , _pbprBucket = p2
    , _pbprContentMD5 = Nothing
    }

data PutBucketPolicy = PutBucketPolicy
    { _pbprPolicy :: Text
      -- ^ The bucket policy as a JSON document.
    , _pbprBucket :: BucketName
    , _pbprContentMD5 :: Maybe Text
    } deriving (Generic)

instance ToPath PutBucketPolicy where
    toPath PutBucketPolicy{..} = mconcat
        [ "/"
        , toBS _pbprBucket
        ]

instance ToQuery PutBucketPolicy

instance ToHeaders PutBucketPolicy where
    toHeaders PutBucketPolicy{..} = concat
        [ "Content-MD5" =: _pbprContentMD5
        ]

instance ToBody PutBucketPolicy where
    toBody = toBody . encodeXML . _pbprPolicy

instance AWSRequest PutBucketPolicy where
    type Sv PutBucketPolicy = S3
    type Rs PutBucketPolicy = PutBucketPolicyResponse

    request = put

    response _ = headerResponse . const $ Right PutBucketPolicyResponse

data PutBucketPolicyResponse = PutBucketPolicyResponse
    deriving (Eq, Show, Generic)
