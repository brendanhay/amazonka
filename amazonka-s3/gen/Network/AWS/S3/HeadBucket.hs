{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.S3.HeadBucket
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
module Network.AWS.S3.HeadBucket
    (
    -- * Request
      HeadBucket
    -- ** Request constructor
    , headBucket
    -- ** Request lenses
    , hbBucket

    -- * Response
    , HeadBucketResponse
    -- ** Response constructor
    , headBucketResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype HeadBucket = HeadBucket
    { _hbBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'HeadBucket' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hbBucket' @::@ 'Text'
--
headBucket :: Text -- ^ 'hbBucket'
           -> HeadBucket
headBucket p1 = HeadBucket
    { _hbBucket = p1
    }

hbBucket :: Lens' HeadBucket Text
hbBucket = lens _hbBucket (\s a -> s { _hbBucket = a })

instance ToPath HeadBucket where
    toPath HeadBucket{..} = mconcat
        [ "/"
        , toText _hbBucket
        ]

instance ToQuery HeadBucket where
    toQuery = const mempty

instance ToHeaders HeadBucket

data HeadBucketResponse = HeadBucketResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'HeadBucketResponse' constructor.
headBucketResponse :: HeadBucketResponse
headBucketResponse = HeadBucketResponse

instance AWSRequest HeadBucket where
    type Sv HeadBucket = S3
    type Rs HeadBucket = HeadBucketResponse

    request  = head
    response = nullaryResponse HeadBucketResponse
