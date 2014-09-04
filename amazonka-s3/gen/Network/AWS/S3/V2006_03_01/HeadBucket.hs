{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.HeadBucket
    (
    -- * Request
      HeadBucket
    -- ** Request constructor
    , headBucket
    -- ** Request lenses
    , hbrBucket

    -- * Response
    , HeadBucketResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'HeadBucket' request.
headBucket :: BucketName -- ^ 'hbrBucket'
           -> HeadBucket
headBucket p1 = HeadBucket
    { _hbrBucket = p1
    }
{-# INLINE headBucket #-}

data HeadBucket = HeadBucket
    { _hbrBucket :: BucketName
    } deriving (Show, Generic)

hbrBucket :: Lens' HeadBucket (BucketName)
hbrBucket f x =
    f (_hbrBucket x)
        <&> \y -> x { _hbrBucket = y }
{-# INLINE hbrBucket #-}

instance ToPath HeadBucket where
    toPath HeadBucket{..} = mconcat
        [ "/"
        , toBS _hbrBucket
        ]

instance ToQuery HeadBucket

instance ToHeaders HeadBucket

instance ToBody HeadBucket

data HeadBucketResponse = HeadBucketResponse
    deriving (Eq, Show, Generic)

instance AWSRequest HeadBucket where
    type Sv HeadBucket = S3
    type Rs HeadBucket = HeadBucketResponse

    request = head
    response _ = nullaryResponse HeadBucketResponse
