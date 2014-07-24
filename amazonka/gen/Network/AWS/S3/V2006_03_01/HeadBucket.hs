{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.HeadBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation is useful to determine if a bucket exists and you have
-- permission to access it.
module Network.AWS.S3.V2006_03_01.HeadBucket where

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

-- | Default HeadBucket request.
headBucket :: BucketName -- ^ '_hbrBucket'
           -> HeadBucket
headBucket p1 = HeadBucket
    { _hbrBucket = p1
    }

data HeadBucket = HeadBucket
    { _hbrBucket :: BucketName
    } deriving (Generic)

instance ToPath HeadBucket where
    toPath HeadBucket{..} = mconcat
        [ "/"
        , toBS _hbrBucket
        ]

instance ToQuery HeadBucket

instance ToHeaders HeadBucket

instance ToBody HeadBucket

instance AWSRequest HeadBucket where
    type Sv HeadBucket = S3
    type Rs HeadBucket = HeadBucketResponse

    request = head

    response _ _ = return (Right HeadBucketResponse)

data HeadBucketResponse = HeadBucketResponse
    deriving (Eq, Show, Generic)
