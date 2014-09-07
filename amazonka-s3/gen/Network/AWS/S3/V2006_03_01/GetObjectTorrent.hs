{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetObjectTorrent
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return torrent files from a bucket.
module Network.AWS.S3.V2006_03_01.GetObjectTorrent
    (
    -- * Request
      GetObjectTorrent
    -- ** Request constructor
    , mkGetObjectTorrent
    -- ** Request lenses
    , gotBucket
    , gotKey

    -- * Response
    , GetObjectTorrentResponse
    -- ** Response lenses
    , gotrsBody
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data GetObjectTorrent = GetObjectTorrent
    { _gotBucket :: BucketName
    , _gotKey :: ObjectKey
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObjectTorrent' request.
mkGetObjectTorrent :: BucketName -- ^ 'gotBucket'
                   -> ObjectKey -- ^ 'gotKey'
                   -> GetObjectTorrent
mkGetObjectTorrent p1 p2 = GetObjectTorrent
    { _gotBucket = p1
    , _gotKey = p2
    }

gotBucket :: Lens' GetObjectTorrent BucketName
gotBucket = lens _gotBucket (\s a -> s { _gotBucket = a })

gotKey :: Lens' GetObjectTorrent ObjectKey
gotKey = lens _gotKey (\s a -> s { _gotKey = a })

instance ToPath GetObjectTorrent where
    toPath GetObjectTorrent{..} = mconcat
        [ "/"
        , toBS _gotBucket
        , "/"
        , toBS _gotKey
        ]

instance ToQuery GetObjectTorrent where
    toQuery GetObjectTorrent{..} = mconcat
        [ "torrent"
        ]

instance ToHeaders GetObjectTorrent

instance ToBody GetObjectTorrent

newtype GetObjectTorrentResponse = GetObjectTorrentResponse
    { _gotrsBody :: RsBody
    } deriving (Show, Generic)

gotrsBody :: Lens' GetObjectTorrentResponse RsBody
gotrsBody = lens _gotrsBody (\s a -> s { _gotrsBody = a })

instance AWSRequest GetObjectTorrent where
    type Sv GetObjectTorrent = S3
    type Rs GetObjectTorrent = GetObjectTorrentResponse

    request = get
    response _ = bodyResponse $ \_ bdy ->
        return $! pure GetObjectTorrentResponse
            <*> pure (RsBody bdy)
